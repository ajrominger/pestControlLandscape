library(nlme)
library(mvtnorm)

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
simMod <- function(nsim, dat, b, sig.farm, sig.site, sig.resid, r.spCor) {
    sp <- rmvnorm(nsim, rep(0, length(unique(dat$site))), )
    # dat$y <- 
}



# Mean_FourthStand_NPerEffort ~ LC1Gau250 + LC2Gau250 + LC3Gau250 + LC4Gau250 + LC5Gau250
# ~1 | Farm

mod <- lme(fixed=formula(paste(response,covariates,LUC)),
    random=random, 
    correlation=corExp(form=~X_up+Y_up|Farm),
    method="ML",
    data=df6)