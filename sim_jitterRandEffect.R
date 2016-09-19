library(nlme)
library(mvtnorm)
library(parallel)

setwd('~/Dropbox/Research/pestControlLandscape')

## helper function for exponential covar
expCoVar <- function(dat, r, sig) {
    d <- as.matrix(dist(dat[, c('lon', 'lat')]))
    out <- exp(-d/r)
    diag(out) <- sig^2
    
    return(out)
}


## make explanitory variables
makeV <- function(nfarm, nsite, nrep) {
    ## xy coordinates
    xy <- expand.grid(1:ceiling(sqrt(nfarm)), 1:ceiling(sqrt(nfarm)))
    xy <- xy[sample(nrow(xy), nfarm), ]
    xy <- cbind(rep(xy[, 1], each = nsite), rep(xy[, 2], each = nsite))
    xy[, 1] <- xy[, 1] + runif(nrow(xy), -0.15*sqrt(nfarm) , 0.15*sqrt(nfarm))
    xy[, 2] <- xy[, 2] + runif(nrow(xy), -0.15*sqrt(nfarm) , 0.15*sqrt(nfarm))
    
    ## farm IDs
    farms <- rep(1:nfarm, each = nsite)
    
    ## combind in a data.frame
    dat <- data.frame(farm = farms, 
                      site = as.numeric(as.factor(paste(farms, 's', 1:nsite, sep = ''))),
                      lon = xy[, 1], lat = xy[, 2])
    
    ## replicate each row according to number of reps
    dat <- dat[rep(1:nrow(dat), each = nrep), ]
    rownames(dat) <- NULL
    
    ## add explanitory variable
    dat$x <- runif(nrow(dat))
    
    return(dat)
}


## simulate model
simMod <- function(nsim, nfarm, nsite, nrep, b, sig.farm, sig.site, sig.resid, r.spCor) {
    ## make the explanitory vars
    dat <- makeV(nfarm, nsite, nrep)
    
    ## jitter lon
    dat$lon_up <- dat$lon + 1:nrow(dat) / 100000000
    
    ## simulate the spatial part
    datUnique <- dat[!duplicated(dat$site), ]
    sp <- rmvnorm(nsim, sigma = expCoVar(datUnique, r.spCor, sig.resid), 
                    method = 'chol')
    spDup <- sp[, rep(1:nrow(datUnique), each = nrep)]
    
    ## loop over number of simulations, returning difference between known and estimated parameters
    out <- mclapply(1:nsim, mc.cores = 6, FUN = function(i) {
        dat$y <- b * dat$x + rnorm(nfarm, sd = sig.farm)[dat$farm] + 
            rnorm(nsite*nfarm, sd = sig.site)[dat$site] + spDup[i, ]
        mod <- lme(fixed = y ~ 0 + x, random = ~1 | farm, correlation = corExp(form = ~lon_up + lat), 
                   method = 'ML', data = dat, control = lmeControl(opt = 'optim'))
        
        return(mod$coefficients$fixed - b)
    })
    
    out <- unlist(out)
    names(out) <- NULL
    
    return(out)
}


## loop over several sig.site values and 2 r.spCor values

sigs.site <- seq(0.5, 4, length = 8)
rs.spCor <- c(0.1, 1)

out <- lapply(sigs.site, function(s) {
    res <- sapply(rs.spCor, function(r) {
        res <- simMod(nsim = 500, nfarm = 20, nsite = 5, nrep = 3, b = 10, 
                      sig.farm = 1.5, sig.site = s, sig.resid = 2, r.spCor = r)
        c(mean(res), quantile(res, prob = c(0.025, 0.975)))
    })
    
    res <- cbind(rs.spCor, t(res))
    colnames(res) <- c('r', 'mean', 'ci0.025', 'ci0.975')
    
    return(res)
})

out <- as.data.frame(cbind(s = rep(sigs.site, each = length(rs.spCor)), do.call(rbind, out)))


## plot it
ylim <- max(abs(range(out[, 3:5]))) * c(-1, 1)

pdf(file = 'fig_jitterSim.pdf', width = 6, height = 6)
par(mfcol = c(length(rs.spCor), 1), mar = c(1, 0, 1, 2) + 0.1, oma = c(3, 4, 0, 0) + 0.1)

for(i in length(rs.spCor):1) {
    with(out[out$r == rs.spCor[i], ], {
         plot(s, mean, ylim = ylim, xaxt = 'n',
              pch = 21, bg = 'white', cex = 1.2,
              panel.first = {
                  abline(h = 0, col = 'gray')
                  arrows(x0 = s, y0 = ci0.025, y1 = ci0.975,
                         code = 3, length =  0.075, angle = 90)
              })
         mtext(substitute(rho == r, list(r = rs.spCor[i])), side = 4, line = 1)
    })
}

axis(1)
mtext('SD of repeated measures', side = 1, line = 1.5, outer = TRUE)
mtext(expression('Difference between true and estimated'~beta), side = 2, line = 2.5, outer = TRUE)

dev.off()
