## ------------------------ WORKSPACE PREPARATION AND LIBRARIES--------------------------------

# Clean the environment
rm(list=ls())

source("DataRobustFunctions.R")

set.seed(63534)

init<-Sys.time()

## ------------------- PARAMETER SETUP FOR IHT TESTING IN BAIN --------------------------------

# Setting contraints for the Classic null hypotesis
ERr1<-matrix(c(1,-1,0,0,
               0,1,-1,0), nrow=2,ncol=4,byrow = TRUE)

IRr1<-matrix(0,0,0)

# Setting constraint for the Ordered Positive hypothesis
ERr2<-matrix(0,0,0)

IRr2<-matrix(c(1,-1,0,0,
               0,1,-1,0), nrow=2,ncol=4,byrow = TRUE)

# Setting constraints for Direction Hypothesis 
ERr3<-matrix(0,0,0)

IRr3<-matrix(c(1,0,0,0,
               0,1,0,0,
               0,0,-1,0),nrow=3,ncol=4,byrow = TRUE)

## ----------------------------------- SCENARIO 3---------------------------------------------

# Parameters for the data generation for all datasets
n_obs<-100
n_datasets<-1000
n_predictors<-3
betas<-c(5,4,3,-2)
r_squared<-0.35
x_correlations<-matrix(c(1,0.11,0.08,0.11,1,0.14,0.08,0.14,1),nrow=3,ncol=3)

# Parameters for data generation of heteroscedastic residuals
alpha1<-0.8

# Parameters for data generation of outliers datasets
n_outliers<-10
coeffs<-c(1,2,3)
minValIQR<-1.5
maxValIQR<-3

# Generating the normal datasets
normalDatasets<-genNormData(betas,x_correlations,n_predictors,n_obs,n_datasets,r_squared)

# set data to use
currentData<-normalDatasets

# Applying different models to the data and storing the results
LR_results<-resultsLR(currentData)
RLM_results<-resultsRLM(currentData)
LMROB_results<-resultsLMROB(currentData)

# Computing coverage of the CI's for the 2 types of regression
estimatesCoverage(data.frame(LR_results[1]),data.frame(LR_results[2]),betas)
estimatesCoverage(data.frame(RLM_results[1]),data.frame(RLM_results[2]),betas)
estimatesCoverage(data.frame(LMROB_results[1]),data.frame(LMROB_results[2]),betas)

# Computing the results of the Post. Probs. for the Linear Regression Model OLS
LR_PostProbs<-BainPostProbs(LR_results,n_obs,ERr1,IRr1,ERr2,IRr2,ERr3,IRr3)
LR_PercRC<-sum(apply(LR_PostProbs,1,function(x) which(x==max(x))==2))/nrow(LR_PostProbs)

# Computing the results of the Post. Probs. for the Linear Regression Model RLM
RLM_PostProbs<-BainPostProbs(RLM_results,n_obs,ERr1,IRr1,ERr2,IRr2,ERr3,IRr3)
RLM_PercRC<-sum(apply(RLM_PostProbs,1,function(x) which(x==max(x))==2))/nrow(RLM_PostProbs)

# Computing the results of the Post. Probs. for the Linear Regression Model LMROB
LMROB_PostProbs<-BainPostProbs(LMROB_results,n_obs,ERr1,IRr1,ERr2,IRr2,ERr3,IRr3)
LMROB_PercRC<-sum(apply(LMROB_PostProbs,1,function(x) which(x==max(x))==2))/nrow(LMROB_PostProbs)

write.csv(LR_PostProbs,"Scenario3_LR_Probs.csv")
write.csv(RLM_PostProbs,"Scenario3_RLM_Probs.csv")
write.csv(LMROB_PostProbs,"Scenario3_LMROB_Probs.csv")
write.csv(LR_results[[1]],"Scenario3_LR_Betas.csv")
write.csv(RLM_results[[1]],"Scenario3_RLM_Betas.csv")
write.csv(LMROB_results[[1]],"Scenario3_LMROB_Betas.csv")

print(paste("Running time was:",Sys.time()-init))

## ----------------------- PARAMETERS SETUP AND DATA GENERATION -------------------------------

# Generating datasets with heteroscedastic residuals
# heteroscDatasets<-genHetData(alpha1,betas,x_correlations,n_predictors,n_obs,n_datasets,r_squared)
