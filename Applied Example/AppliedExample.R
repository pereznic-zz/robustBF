library(MASS)
library(robust)
library(Bain)

# Make sure you first ran the code in the Markdown file from the "Script.Rmd" and that the repro data is present in your Global environment. If properly loaded, the dimensions should be 238 observations and 258 variables.

# Creating the dataframe with the interesting variables for the study
filteredData<-data.frame(SatLive_Average=repro$SatLive_Average,wantHave=repro$wantHave,
                         haveWant=repro$haveWant,both=repro$wantHave*repro$haveWant)

# Scaling the data for IHT

scaledData<-data.frame(cbind(SatLive_Average=filteredData[,1],scale(as.matrix(filteredData[,-1]),
                                                center=TRUE,scale=TRUE)))
pairs(scaledData)
boxplot(scaledData)
summary(scaledData)
apply(scaledData,MARGIN = 2,FUN=sd)

summary(scaledData)

# Modeling the outcome variable "SatLive" with the three different regression methods

model_OLS<-lm(data=scaledData, SatLive_Average~.)
model_MM<-rlm(data=scaledData, SatLive_Average~.,method="MM",psi=psi.bisquare,init="lts",
              maxit=100)
model_LMROB<-lmRob(data=scaledData, SatLive_Average~.)

summary(model_OLS)
summary(model_MM)
summary(model_LMROB)

# Specifying the constraints for IHT
ERr1<-matrix(c(1,-1,0,0,
               0,1,-1,0), nrow=2,ncol=4,byrow = TRUE)

IRr1<-matrix(0,0,0)

# Setting constraints for B3>B1>B2>0
ERr2<-matrix(0,0,0)

IRr2<-matrix(c(-1,0,1,0,
               1,-1,0,0), nrow=2,ncol=4,byrow = TRUE)

# Setting constraints for B3>B2>B1>0 
ERr3<-matrix(0,0,0)

IRr3<-matrix(c(1,0,0,0,
               0,1,0,0,
               0,0,-1,0),nrow=3,ncol=4,byrow = TRUE)


# Extract coefficients and covariance matrices for each model
coeffs_OLS<-model_OLS$coefficients[-1]
coeffs_MM<-model_MM$coefficients[-1]
coeffs_LMROB<-model_LMROB$coefficients[-1]

vcov_OLS<-vcov(model_OLS)[-1,-1]
vcov_MM<-vcov(model_MM)[-1,-1]
vcov_LMROB<-model_LMROB$cov[-1,-1]

# Computing the Bayes Factors
Bain(unlist(coeffs_OLS),vcov_OLS,nspec=0,njoint=3,samp=nrow(scaledData),ERr1,IRr1,ERr2,IRr2,
     ERr3,IRr3,print=FALSE)$testResult

Bain(unlist(coeffs_MM),vcov_MM,nspec=0,njoint=3,samp=nrow(scaledData),ERr1,IRr1,ERr2,IRr2,
     ERr3,IRr3,print=FALSE)$testResult

Bain(unlist(coeffs_LMROB),vcov_LMROB,nspec=0,njoint=3,samp=nrow(scaledData),ERr1,IRr1,ERr2,IRr2,
     ERr3,IRr3,print=FALSE)$testResult
  