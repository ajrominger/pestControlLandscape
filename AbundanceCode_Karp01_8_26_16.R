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
response_type <- "Abundance" #Fill in the general response variable you want. Can be either "Abundance", "Activity", or "Yield" 
#####################
# End Customization #
#####################

# Setup: No customization needed, skip everything until you see customization again. 
library(openxlsx)
library(XLConnect) 
library(sp)
library(doBy)
library(plyr)
library(reshape2)
library(nlme)
library(MuMIn)
library(MASS)
library(spdep)
library(visreg)
library(dplyr)
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

df0[,"Abundance"] <- as.numeric(df0[,"Abundance"]) # change abundance to numeric with proper NAs where text
df0[,"Abundance_Duration"] <- as.numeric(df0[,"Abundance_Duration"])
df0[,"Number_Censuses"] <- as.numeric(df0[,"Number_Censuses"])

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


# Make decisions about which functional groups to analyze 
dom_pests=df0[which(df0$Functional_Group=="dominant pest"),] # Subsets to dominant pests. 
ag_dom_pests=summarize(group_by(dom_pests,Order,Family,Species),Abun=sum(Abundance)) # Aggregates abundances of each dominant pest
sp_to_analyze=ag_dom_pests[which(ag_dom_pests[,4]/sum(ag_dom_pests[,4])>.05),3];sp_to_analyze # Restricts analysis to dominant pests that make up greater than 5% of surveyed individuals. Prints dominant pests that should be analyzed individually. 

possible_analysis_types=c("dominant_enemies","dominant_pests","all_enemies","all_pests","specific_dominant_pest"); print(possible_analysis_types) #Prints the 5 analysis categories that we want to target
fun_group_summary=summarize(group_by(df0, Functional_Group), Abun=sum(Abundance));fun_group_summary # Prints the number of captures of all functional groups that have been named in the dataset
#######################
# Start Customization #
#######################
analysis_type= "dominant_pests" #MUST MATCH "possible_analysis_types" ABOVE. Fill in the analysis type that you will conduct. If a particular pest, then write out the name of the pest. Only choose to analyze a particular dominant pest if it appears above in the "sp_to_analyze" table 
functional_group <- c("dominant pest") #MUST MATCH STUDY NAMES OF FUNCTONAL GROUPS. Choose the name(s) of the study functional group(s) that your analysis will focus on. May be 1 or more functional groups but names must match the names in the spreadsheet. If you will focus on one dominant pest species, still put in dominant pest. 
species<-"" #MUST MATCH STUDY NAME OF SPECIES. If doing an analysis of a particular dominant pest, fill this in otherwise do not fill this in. 

View(df0) # Views df1. Look at all the values written in the abundance column and try to decide whether the authors reported the number of insects trapped across surveys or a mean abundance across surveys. A mean abundance would usually be indicated by decimal numbers in the abundance column. 
reportmean=FALSE # Set this to true if they report a mean abundance. Keep it as false if they report a total abundance.
#######################
# End Customization #
#######################

df1=df0[df0$Functional_Group%in%functional_group,]
if(species!=""){df1=df1[df1$Species%in%species,]}

# Add Zeros: Adds zeros  to all the sites that did not observe a given functional group at a given site. Code is automated, no need to change anything. 
# Determine how many times each site was sampled in each year and sampling method 
df0$indicator=1
ag1 <- summarize(group_by(df0, Site,Study_Year,Sampling_Date,Sampling_Method), MeanInd=mean(indicator)) 
SurveyTabulator<- summarize(group_by(ag1, Site,Study_Year,Sampling_Method), SumInd=sum(MeanInd))
SurveyTabulator$SurveyedSites=paste(SurveyTabulator$Site,SurveyTabulator$Study_Year,SurveyTabulator$Sampling_Method) 

# Summarize how many times each site appears after subseting down to the function guild 
df1$indicator=1
ag1 <- summarize(group_by(df1, Site,Study_Year,Sampling_Date,Sampling_Method), MeanInd=mean(indicator)) 
ObservationTabulator<- summarize(group_by(ag1, Site,Study_Year,Sampling_Method), SumInd=sum(MeanInd))
ObservationTabulator$SurveyedSites=paste(ObservationTabulator$Site,ObservationTabulator$Study_Year,ObservationTabulator$Sampling_Method) 

# Figure out which sites need to be added and how many times 
AddIndex=as.vector(ObservationTabulator[match(SurveyTabulator$SurveyedSites,ObservationTabulator$SurveyedSites),4])
AddIndex[is.na(AddIndex)]=0 #Redefine NAs as 0s
AddNum=as.vector(SurveyTabulator$SumInd)-AddIndex

if(max(AddNum)>0){
  for (i in 1:max(AddNum)){
    sub=SurveyTabulator[which(AddNum==i),]
    addSITES=array(dim=c(dim(sub)[1],length(df1[1,])))
    colnames(addSITES)=colnames(df1)               
    addSITES[,1]=df1[1,1]; addSITES[,2]=sub$Site; addSITES[,3]=sub$Study_Year; addSITES[,9]=0; addSITES[,c(10,12,14)]=1; addSITES[,c(11)]=sub$Sampling_Method
      for (j in 1:i){
      defSAMPDATE=addSITES
      defSAMPDATE[,4]=j*1000000+1:length(defSAMPDATE[,1])
      df1=rbind(df1,defSAMPDATE)
    }
  } 
} 

df1=as.data.frame(df1)
df1$Abundance=as.numeric(df1$Abundance)
df1$Number_Censuses=as.numeric(df1$Number_Censuses)
df1$Abundance_Duration=as.numeric(df1$Abundance_Duration)

# Sum All Arthropods Observed in a Single Sampling Round 
df2 <- summarize(group_by(df1, Site, Study_Year,Sampling_Date,Number_Censuses,Sampling_Method,Abundance_Duration), TotAbun=sum(Abundance))

# Check sampling methods: Only include sampling methods where the pest was surveyed more than 5% of the time. 
ag_samp_method=aggregate(df2$TotAbun,list(df2$Sampling_Method),sum)
include_methods=ag_samp_method[which(ag_samp_method[,2]/sum(ag_samp_method[,2])>.05),1] #Identifies methods that have more than 5% of detections
df2=df2[df2$Sampling_Method%in%include_methods,]

#Create N/Effort 
df2$Number_Censuses[which(df2$Number_Censuses<1|is.na(df2$Number_Censuses))]=1
df2$Abundance_Duration[which(df2$Abundance_Duration<1|is.na(df2$Abundance_Duration))]=1

# If they reported a mean abundance, then no need to standardize by effort. Otherwise, divide by number of censuses and abundance duration. 
if (reportmean==TRUE){
  df2$nPerEffort=df2$TotAbun  
  df2$SqrtnPerEffort=sqrt(df2$TotAbun)
  df2$FourthnPerEffort=(df2$TotAbun)^.25
  df2$LognPerEffort=log(df2$TotAbun)
} 

if (reportmean==FALSE){
  df2$nPerEffort=df2$TotAbun/df2$Number_Censuses/df2$Abundance_Duration
  df2$SqrtnPerEffort=sqrt(df2$TotAbun)/df2$Number_Censuses/df2$Abundance_Duration
  df2$FourthnPerEffort=((df2$TotAbun)^.25)/df2$Number_Censuses/df2$Abundance_Duration
  df2$LognPerEffort=log(df2$TotAbun)/df2$Number_Censuses/df2$Abundance_Duration
 }

# Standardize data: Data is standardized within a given year and sampling method so that no one year or sampling method is more influential than the others. 
s_methods=unique(df2$Sampling_Method)
years=unique(df2$Study_Year)
df3=array(dim=c(0,length(colnames(df2))+4))
colnames(df3)=c(colnames(df2),"Stand_NPerEffort","SqrtStand_NPerEffort","FourthStand_NPerEffort","LogStand_NPerEffort")

for (i in 1:length(s_methods)){
  oneMETHOD=df2[which(df2$Sampling_Method==s_methods[i]),]
  for (j in 1:length(years)){
    oneYEAR=oneMETHOD[which(oneMETHOD$Study_Year==years[j]),]
    oneYEAR$Stand_NPerEffort=scale(oneYEAR$nPerEffort)
    oneYEAR$SqrtStand_NPerEffort=scale(oneYEAR$SqrtnPerEffort)  
    oneYEAR$FourthStand_NPerEffort=scale(oneYEAR$FourthnPerEffort)    
    oneYEAR$LogStand_NPerEffort=scale(oneYEAR$LognPerEffort)    
    df3=rbind(df3,oneYEAR)
    }
}
df4 <- summarize(group_by(df3, Study_Year, Sampling_Method, Site), Mean_Stand_NPerEffort=mean(Stand_NPerEffort),Mean_SqrtStand_NPerEffort=mean(SqrtStand_NPerEffort),Mean_FourthStand_NPerEffort=mean(FourthStand_NPerEffort),Mean_LogStand_NPerEffort=mean(LogStand_NPerEffort) ) # Average over all surveys conducted using the same method in the same year

##################### 
#START CUSTOMIZATION#
#####################
# Add in landscape data # NOTE: First need to check if any sites were not listed in the site attribute table, see below 
notINCLUDED=df4[is.na(match(df4$Site,ls1$Site)),3]; print(notINCLUDED) # CHECK THIS BEFORE PROCEEDING: If sites show up here, this means we do not have land use data for these sites. Check the site tab to see if the missing sites are listed- call View(siteTAB). If missing sites are in the site tab, contact danny. If missing sites are missing from the site tab, then need to alert the authors that they failed to include a site. 
###################
#End CUSTOMIZATION#
###################

df5 <- merge(df4, ls1, by="Site") # Merges in the land use data. Do not change code. 
df6<-  merge(df5, siteTAB, by=c("Site", "X", "Y")) # Merges in the site tab. Do not change code. 
df6=df6[,is.na(match(1:length(df6[1,]),which(colnames(df6)=="Study_Year.y")))] # Gets rid of second study year column
colnames(df6)[which(colnames(df6)=="Study_Year.x")]="Study_Year" # redefines Study Year after merge

# Change the coordinates slightly for later model functioning
df6$X_up=df6$X+1:length(df6[,1])/100000000
df6$Y_up=df6$Y

######################
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
VIFs=corvif(df6[,paste("LC",landuseclasses,"Gau",scale,sep="")]) 
collinearity=FALSE
subsetclasses=landuseclasses
while (TRUE%in%(VIFs[,1]>2.5)){
  collinearity=TRUE
  maxVIF=rownames(VIFs)[which(VIFs==max(VIFs))] # Variable with max VIF
  subsetclasses=subsetclasses[which(subsetclasses!=as.numeric(substr(maxVIF,3,3)))] #redefines land use classes without the most collinear one. 
  VIFs=corvif(df6[,paste("LC",subsetclasses,"Gau",scale,sep="")]) 
}
LUC=paste(paste("LC",landuseclasses,"Gau",scale,sep=""),collapse="+") # adds in all the land use classes defined above.
SUB_LUC=paste(paste("LC",subsetclasses,"Gau",scale,sep=""),collapse="+") # adds in all the land use classes defined above. 

#######################
# Start Customization #
#######################
# FIXED EFFECTS: Beyond the land use variables, also include the following variables: (1) sampling method- if multiple methods are present, (2) crop type- if multiple crop types are includes, (3) any other covariates indicated as important by study co-authors
# RANDOM EFFECTS: If multiple values are reported per farm (e.g., two or more sampling methods), then include farm as a random effect. Can also include other random effects if they are appropriate to the study- up to user discretion. 

# View some of the possible covariates and random effects
table(df6$Farm) # How many observations at each site
table(siteTAB$Crop_Species) # Visualizes the crops that have been studied. If different crops are included, then include as a covariate.
table(siteTAB$Site_Covariate1) # Visualizes potential covariates that the authors could have added
table(siteTAB$Site_Covariate2) # Visualizes potential covariates that the authors could have added
table(df6$Study_Year) # Visualizes variation in years. if only 1 year is present, do not include as a covariate
table(df6$Sampling_Method) # Visualzies variation in sampling methods. if only 1 method is present, do not include as a covariat

response="Mean_FourthStand_NPerEffort";print(df6[,response]) # CUSTOMIZE: Choose which response to include. If want a transformation for normality (see below for model checks), then choose either "Mean_LogStand_NPerEffort", "Mean_SqrtStand_NPerEffort", or "Mean_FourthStand_NPerEffort," otherwise use "Mean_Stand_NPerEffort." Note: Log transformation cannot be used if there were 0s present. Will appear as "NaN" if this is the case. 
covariates="~0+Sampling_Method+" # CUSTOMIZE: Add in any other covariates here, be sure to leave + at end of line and ~ in the beginning. Land use classes added in next line. Be sure to keep in 0 if analyzing sampling method or year is included. 
random=formula(~1|Farm) #CUSTOMIZE: Define random effects here 
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
    data=df6)
} else {
  M_full <- gls(formula(paste(response,covariates,LUC)), # Type of model used if there ARE NO random effects
    correlation=corExp(form=~X_up+Y_up),
    method="ML",
    data=df6)
}
aic=AIC(M_full)
full_av=model.avg(dredge(M_full, rank="AIC", extra="R^2")); full_sum<-summary(full_av) # Model Average

if (collinearity==TRUE & random_effects==TRUE){
  M_subset <- lme(
    fixed= formula(paste(response,covariates,SUB_LUC)), 
    random=random, 
    correlation=corExp(form=~X_up+Y_up), 
    method="ML",
    data=df6)
  Sub_AIC=AIC(M_subset)
  sub_av=model.avg(dredge(M_subset, rank="AIC", extra="R^2")); sub_sum<-summary(sub_av)
} 
if(collinearity == TRUE & random_effects==FALSE){
  M_subset <- gls(formula(paste(response,covariates,SUB_LUC)), # Type of model used if there ARE NO random effects
     method="ML",
     correlation=corExp(form=~X_up+Y_up),
     data=df6)
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
  out=array(dim=c(length(df6[,1]),1),0)
  for (i in 1:length(fixef)){
    index=match(names(fixef[i]),colnames(df6))
    if(is.na(index)=="FALSE") {
      out=out+df6[,index]*fixef[i]
    } else {
      catvector=df6[,which(sapply(colnames(df6),function(x) grep(x,names(fixef[i])))>0)]
      catindex=which(sapply(catvector,function(x) grep(x,names(fixef[i])))>0)
      out[catindex]=out[catindex]+fixef[i]
    }
  }
  correlation=as.numeric(cor(out,df6[,match(response,colnames(df6))],method="pearson"))
  cor_p=cor.test(out,df6[,match(response,colnames(df6))])$p.value
  out=cbind(correlation,cor_p)
  colnames(out)=c("corr","corr_p")
  out
}

# Compile Data
study=study_id
country=metad$Country
region=metad$Region
crops=paste(unique(df6$Crop),sep="",collapse="; ")
b_response=response_type
fun_group= analysis_type

StudyAtr=cbind(study,country,region,crops,b_response,fun_group,species,scale, collinearity, Transformation,Normality,Heteroskedascity,aic,obs.exp(full_sum))
LongStudyAtr=StudyAtr
predictors=calc.predictors(full_sum)
for (i in 1:(length(predictors[,1])-1)){LongStudyAtr=rbind(LongStudyAtr,StudyAtr)}
ModResults=cbind(LongStudyAtr,predictors)

if(collinearity==TRUE){
  StudyAtr=cbind(study,country,region,crops,b_response,fun_group,species,scale, "FALSE", Transformation,Normality,Heteroskedascity,Sub_AIC,obs.exp(sub_sum))
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
