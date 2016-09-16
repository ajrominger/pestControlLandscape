#######################
# Start Customization #
#######################
setwd("~/Documents/SESYNC/Data/Results") # change to your working drive. this is where results will saved. 
study_directory<- study_id <-'Karp01' #change to match study name
response_name <-"Karp_SESYNC_DataForm_v2.xls" #change to name of spreadsheet where data is
isXLSX <- "No" #Change to indicate whether or not the spreadsheet is an xls or xlsx. If you cannot load the package below XLConnect, change the data file to an .xlsx, alter this line, and proceed with the code. 
response_path <- '~/Documents/SESYNC/Data/ToBeCleanedSpreadsheets/' #Change to where the spreadsheet with the data is located. Remember to include the forward slash at the end. 
landscape_name <- "V1_Karp01_AllC_Gaussian_3_decays_weighted_cover_percent.csv" #Change to name of the landscape file.
landscape_path <- '~/Documents/SESYNC/Data/LandUseData/Karp01/' #Change to where the land use spreadsheet is located. Remember to include the forward slash at the end. 
response_type <- "Yield" #Fill in the general response variable you want. Can be either "Abundance", "Activity", or "Yield" 
#####################
# End Customization #
#####################

# Setup 
library(openxlsx)
library(XLConnect) 
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(gdalUtils)
library(doBy)
library(plyr)
library(reshape2)
library(nlme)
library(MuMIn)
library(MASS)
library(spdep)
library(visreg)
library(dplyr)
library(mapview)
options(stringsAsFactors=F)
source("http://www.highstat.com/Book2/HighstatLibV6.R")

# static file name and tab stuff
lname <- paste(landscape_path,landscape_name,sep=""); lname
ls1 <- read.csv(lname)
ls1[,1]=tolower(ls1[,1]); ls1[,2]=tolower(ls1[,2]); ls1[,3]=tolower(ls1[,3])

rname <- paste(response_path,response_name,sep=""); rname
if(response_type == "Abundance") {wsNum <- 5}; if(response_type == "Abundance") {yearNum <- 3}
if(response_type == "Activity") {wsNum <- 6}; if(response_type == "Activity") {yearNum <- 3}
if(response_type == "Yield") {wsNum <- 7}; if(response_type == "Yield") {yearNum <- 3}

if(isXLSX != "Yes"){ # Reads in file
  df0 <- readWorksheetFromFile(rname, sheet=wsNum, startRow=3)
} else {
  df0 <- read.xlsx(xlsxFile=rname, sheet=wsNum, startRow=3)
}

df0 <- data.frame(lapply(df0, function(v) { # Makes the file lowercase 
  if (is.character(v)) return(tolower(v))
  else return(v)
}))

df0=df0[df0$Study_ID%in%tolower(study_id),] #Subsets to the study of interest
df0[,"Function_Data"] <- as.numeric(df0[,"Function_Data"]) # change abundance to numeric with proper NAs where text

# Read in site tab 
if(isXLSX != "Yes"){ # Reads in file
  siteTAB <- readWorksheetFromFile(rname, sheet=4, startRow=3)
} else {
  siteTAB <- read.xlsx(xlsxFile=rname, sheet=4, startRow=3)
}

siteTAB <- data.frame(lapply(siteTAB, function(v) { # Makes the file lowercase 
  if (is.character(v)) return(tolower(v))
  else return(v)
}))
siteTAB=siteTAB[siteTAB$Study_ID%in%tolower(study_id),] #Subsets to the study of interest

# Read in metadata
if(isXLSX != "Yes"){ # Reads in file
  metad <- readWorksheetFromFile(rname, sheet=1, startRow=3)
} else {
  metad <- read.xlsx(xlsxFile=rname, sheet=1, startRow=3)
}
metad=metad[metad$Study_ID==study_id,]


# Subset to function type of interest #
yield_summary=summarize(group_by(df0, Measure_Type, Marketable_or_Total)); print(yield_summary) # Prints all the possible study functions and the pests associated with them. 

#######################
# Start Customization # Choose yield type to analyze
#######################
analysis_type= "true_yield" # MUST MATCH "yield_summary" ABOVE. Fill in the name of the analysis you will conduct. Can be: "true_yield" or "within_plant_yield". If any of the yield_summary (above) does not match one of these names, then the dataset needs to be cleaned still. 
yield_type<-"total" # MUST MATCH "yield_summary" ABOVE. Specify if it is marketable or total yield
#################### 
# End Customization#
####################

df1=df0[df0$Measure_Type%in%analysis_type,] # Use this to subset down the broad function type
if(yield_type!=""){df1=df1[df1$Marketable_or_Total%in%yield_type,]}

# We assume that each site that was surveyed at a given time has data reported (unlike the abundance analyses where we add zeros). Here we do not add any 0s. 
df1=as.data.frame(df1)
df1$Function_Data=as.numeric(df1$Function_Data)
df1$SqrtYield=sqrt(df1$Function_Data)
df1$FourthYield=(df1$Function_Data)^.25
df1$LogYield=log(df1$Function_Data)

# Standardize data: Data is standardized within a given year so that no one year is more influential than the others. 
years=unique(df1$Study_Year)
df2=array(dim=c(0,length(colnames(df1))+4))
colnames(df2)=c(colnames(df1),"Stand_Yield","SqrtStand_Yield","FourthStand_Yield","LogStand_Yield")

for (i in 1:length(years)){
  oneYEAR=df1[which(df1$Study_Year==years[i]),]
  oneYEAR$Stand_Yield=scale(oneYEAR$Function_Data)
  oneYEAR$SqrtStand_Yield=scale(oneYEAR$SqrtYield)
  oneYEAR$FourthStand_Yield=scale(oneYEAR$FourthYield)
  oneYEAR$LogStand_Yield=scale(oneYEAR$LogYield)    
  df2=rbind(df2,oneYEAR)
  }

# Average over all values at a given site in a given year 
df3 <- summarize(group_by(df2, Study_Year, Site), Mean_Stand_Yield=mean(Stand_Yield),Mean_SqrtStand_Yield=mean(SqrtStand_Yield) ,Mean_FourthStand_Yield=mean(FourthStand_Yield),Mean_LogStand_Yield=mean(LogStand_Yield) )

##################### 
#START CUSTOMIZATION#
#####################
# Add in landscape data # NOTE: First need to check if any sites were not listed in the site attribute table, see below 
notINCLUDED=df3[is.na(match(df3$Site,ls1$Site)),3]; print(notINCLUDED) # CHECK THIS BEFORE PROCEEDING: If sites show up here, this means we do not have land use data for these sites. Check the site tab to see if the missing sites are listed- call View(siteTAB). If missing sites are in the site tab, contact danny. If missing sites are missing from the site tab, then need to alert the authors that they failed to include a site. 
###################
#End CUSTOMIZATION#
###################

df4 <- merge(df3, ls1, by="Site") # Merges in the land use data. Do not change code. 
df5<-  merge(df4, siteTAB, by=c("Site", "X", "Y")) # Merges in the site tab. Do not change code. 

df5=df5[,is.na(match(1:length(df5[1,]),which(colnames(df5)=="Study_Year.y")))] # Gets rid of second study year column
colnames(df5)[which(colnames(df5)=="Study_Year.x")]="Study_Year" # redefines Study Year after merge

# Change the coordinates slightly for later model functioning, do not change code. 
df5$X_up=df5$X+1:length(df5[,1])/100000000
df5$Y_up=df5$Y

#####################
# Start Customization#
######################
# Choose land use classes and spatial scale
#View(ls1) # Views the land use data to decide which land use classes should be analyzed. 8 can substitute for 1,2,3 (if they are missing); 9 can substitute for 4,5
landuseclasses=c(1,2,3,4,5) # CUSTOMIZE: Never use 6 or 7. Define which land use classes are present and will be analyzed. Numbers are the class numbers. 
scale="250" # CUSTOMIZE: the spatial scale that will be analyzed
####################
# End Customization#
####################

# Collinearity check: This automatically checks collinearity. If any VIFs are >2.5, then collinearity is present. Variables are iteratively deleted until all VIFs are <2.5
VIFs=corvif(df5[,paste("LC",landuseclasses,"Gau",scale,sep="")]) 
collinearity=FALSE
subsetclasses=landuseclasses
while (TRUE%in%(VIFs[,1]>2.5)){
  collinearity=TRUE
  maxVIF=rownames(VIFs)[which(VIFs==max(VIFs))] # Variable with max VIF
  subsetclasses=subsetclasses[which(subsetclasses!=as.numeric(substr(maxVIF,3,3)))] #redefines land use classes without the most collinear one. 
  VIFs=corvif(df5[,paste("LC",subsetclasses,"Gau",scale,sep="")]) 
}
LUC=paste(paste("LC",landuseclasses,"Gau",scale,sep=""),collapse="+") # adds in all the land use classes defined above. 
SUB_LUC=paste(paste("LC",subsetclasses,"Gau",scale,sep=""),collapse="+") # adds in all the land use classes defined above. 

#######################
# Start Customization #
#######################
# FIXED EFFECTS: Beyond the land use variables, also include the following variables: (1) sampling method- if multiple methods are present, (2) crop type- if multiple crop types are includes, (3) any other covariates indicated as important by study co-authors
# RANDOM EFFECTS: If multiple values are reported per farm and or site (e.g., two or more sampling methods), then include farm and site as potentially nested random effects. Can also include other random effects if they are appropriate to the study- up to user discretion. 

# View some of the possible covariates and random effects
table(df5$Site) # How many observations at each site
table(siteTAB$Farm) # Tells you whether there are multiple sites on each farm. If so, then include farm as a random effect. 
table(siteTAB$Crop_Species) # Visualizes the crops that have been studied. If different crops are included, then include as a covariate.
table(siteTAB$Site_Covariate1) # Visualizes potential covariates that the authors could have added
table(siteTAB$Site_Covariate2) # Visualizes potential covariates that the authors could have added
table(df5$Study_Year)

response="Mean_LogStand_Yield" # Choose which response to include. If want a transformation for normality (see below for model checks), then choose either "Mean_LogStand_Yield", "Mean_SqrtStand_Yield", or "Mean_FourthStand_Yield" otherwise use "Mean_Stand_Yield" Note: Log transformation cannot be used if there were 0s present. Will appear as "NaN" if this is the case. 
covariates="~0+" # Add in any other covariates here, be sure to leave plus at end of line and ~ at the beginning. Land use classes added in next line. Be sure to keep in 0 if year is included. 
random=formula(~1|Farm) # Define random effects here 
random_effects= TRUE #Customize: Tell us whether there is a random effect. If no random effects, write "FALSE"
####################
# End Customization#
####################

# Run models 
if(random_effects==TRUE){
  M_full <- lme(
    fixed=formula(paste(response,covariates,LUC)), # Type of model used if there ARE random effects
    random=random, 
    correlation=corExp(form=~X_up+Y_up),
    method="ML",
    data=df5)
} else {
  M_full <- gls(formula(paste(response,covariates,LUC)), # Type of model used if there ARE NO random effects
                correlation=corExp(form=~X_up+Y_up),
                method="ML",
                data=df5)
}
aic=AIC(M_full)
full_av=model.avg(dredge(M_full, rank="AIC", extra="R^2")); full_sum<-summary(full_av) # Model Average

if (collinearity==TRUE & random_effects==TRUE){
  M_subset <- lme(
    fixed= formula(paste(response,covariates,SUB_LUC)), 
    random=random, 
    correlation=corExp(form=~X_up+Y_up), 
    method="ML",
    data=df5)
  Sub_AIC=AIC(M_subset)
  sub_av=model.avg(dredge(M_subset, rank="AIC", extra="R^2")); sub_sum<-summary(sub_av)
} 
if(collinearity == TRUE & random_effects==FALSE){
  M_subset <- gls(formula(paste(response,covariates,SUB_LUC)), # Type of model used if there ARE NO random effects
                  method="ML",
                  correlation=corExp(form=~X_up+Y_up),
                  data=df5)
  Sub_AIC=AIC(M_subset)
  sub_av=model.avg(dredge(M_subset, rank="AIC", extra="R^2")); sub_sum<-summary(sub_av)
}

#######################
# Start Customization #
#######################
# Check models: Residuals of the model should appear normal in histogram. If not, try a square root, fourth root, or log transformation by selecting the appropriate response variable (see above when specifying model). If still not normal, need to note it below. Likewise, if bottom plot does not look like a scatter plot. (i.e., there's a strong funnel pattern), then need to either transform or note that residuals are heteroskedastic.
layout(c(1,2)); par(mar=c(5,5,1,2))
qqnorm(resid(M_full)); plot(resid(M_full)~fitted(M_full))  
Transformation= "fourth" # Fill in either "none","sqrt", "fourth", or "log" if you transformed the data.
Normality= "normal" # Fill in either "normal" or "not normal" depending on if residuals are normal in this model
Heteroskedascity= "none" # Fill in either "none" or "patterned residuals" if there are patterns in the residuals 
#####################
# End Customization #
#####################

# Build output matrix
calc.predictors=function(model_summary){ # Function for extracting information from model averaging 
  predictors=model_summary$coefmat.subset
  predictors=cbind(rownames(predictors),predictors)
  rownames(predictors)=rep("",length(predictors[,1]))
  colnames(predictors)[1]="Variables"
  importance=array(dim=c(length(predictors[,1]),1),NA)
  for (i in 1:length(model_summary$importance)){
    importance[grep(names(model_summary$importance)[i],predictors[,1])]=model_summary$importance[i]
  }
  predictors=cbind(predictors,importance)
  colnames(predictors)[7]="Importance"
  return(predictors)
}

obs.exp=function(model_summary){ # Function for comparing observed data to predicted (expected) data generated from the model
  fixef=model_summary$coefmat.subset[,1]
  out=array(dim=c(length(df5[,1]),1),0)
  for (i in 1:length(fixef)){
    index=match(names(fixef[i]),colnames(df5))
    if(is.na(index)=="FALSE") {
      out=out+df5[,index]*fixef[i]
    } else {
      catvector=df5[,which(sapply(colnames(df5),function(x) grep(x,names(fixef[i])))>0)]
      catindex=which(sapply(catvector,function(x) grep(x,names(fixef[i])))>0)
      out[catindex]=out[catindex]+fixef[i]
    }
  }
  correlation=as.numeric(cor(out,df5[,match(response,colnames(df5))],method="pearson"))
  cor_p=cor.test(out,df5[,match(response,colnames(df5))])$p.value
  out=cbind(correlation,cor_p)
  colnames(out)=c("corr","corr_p")
  out
}

# Compile Data
study=study_id
country=metad$Country
region=metad$Region
crops=paste(unique(df5$Crop),sep="",collapse="; ")
b_response=response_type
fun_type= paste(analysis_type)

StudyAtr=cbind(study,country,region,crops,b_response,fun_type,yield_type,scale, collinearity, Transformation,Normality,Heteroskedascity,aic,obs.exp(full_sum))
LongStudyAtr=StudyAtr
predictors=calc.predictors(full_sum)
for (i in 1:(length(predictors[,1])-1)){LongStudyAtr=rbind(LongStudyAtr,StudyAtr)}
ModResults=cbind(LongStudyAtr,predictors)

if(collinearity==TRUE){
  StudyAtr=cbind(study,country,region,crops,b_response,fun_type,yield_type,scale, "FALSE", Transformation,Normality,Heteroskedascity,Sub_AIC,obs.exp(sub_sum))
  LongStudyAtr=StudyAtr
  predictors=calc.predictors(sub_sum)
  for (i in 1:(length(predictors[,1])-1)){LongStudyAtr=rbind(LongStudyAtr,StudyAtr)}
  ColResults=cbind(LongStudyAtr,predictors)
  ModResults=rbind(ModResults,ColResults)
}

if(exists("CompiledResults")){
  CompiledResults=rbind(CompiledResults,ModResults)
} else{
  CompiledResults=ModResults
}

######################
# Start CUSTOMIZATION#
######################
# Run this code when you want to save your results. But careful- it will overwrite previous files of the same name. Therefore, update the date below to not overwrite your work. 
date="8_3_2016"
write.csv(CompiledResults,paste(study_id,response_type,date,".csv",sep=""))