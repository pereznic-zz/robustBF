library(MASS)
library(robust)
library(Matrix)
library(svMisc)
library(Bain)

## ------------------ FUNCTION TO GENERATE LINEAR REGRESSION DATASETS -------------------------

# Function that generates the datasets
genNormData<-function(betas,xCorrels,numPredictors,numObservations,numDatasets,rSquared)
{
  datasets<-array(0,dim=c(numObservations,numPredictors+1,numDatasets))
  x_means<-rep(0,numPredictors)
  for(i in 1:numDatasets)
  {
    x_matrix<-matrix(0,nrow=numObservations,ncol=numPredictors)
    x_matrix<-mvrnorm(numObservations,mu=x_means,Sigma=xCorrels,empirical=FALSE)
    pred_y<-apply(t(t(cbind(rep(1,numObservations),x_matrix))*betas),MARGIN=1,FUN=sum)
    res_var<-ifelse(betas[-1]==0,(1-rSquared)/rSquared,
           sum((betas[-1]%*%t(betas[-1])*xCorrels))*(1-rSquared)/rSquared)
    samp_Y<-pred_y+rnorm(numObservations,0,sqrt(res_var))
    datasets[,,i]<-cbind(x_matrix,samp_Y)
  }
  colnames(datasets)<-c(paste("x",seq(1,numPredictors),sep=""),"y")
  rownames(datasets)<-c(1:numObservations)
  
  return(datasets)
  
}


## ----------- FUNCTION TO GENERATE LR DATASETS WITH HETEROSCEDASTIC RESIDUALS ----------------

# Given a value of alpha 1 sets the value of alpha zero to fit a model with desired R2
genHetData<-function(alpha1,betas,xCorrels,numPredictors,numObservations,numDatasets,rSquared)
{
  datasets<-array(0,dim=c(numObservations,numPredictors+1,numDatasets))
  x_means<-rep(0,numPredictors)
  for(i in 1:numDatasets)
  {
    x_matrix<-matrix(0,nrow=numObservations,ncol=numPredictors)
    x_matrix<-mvrnorm(numObservations,mu=x_means,Sigma=xCorrels,empirical=FALSE)
    pred_y<-apply(t(t(cbind(rep(1,numObservations),x_matrix))*betas),MARGIN=1,FUN=sum)
    res_var<-ifelse(betas[-1]==0,(1-rSquared)/rSquared,
                    sum((betas[-1]%*%t(betas[-1])*xCorrels))*(1-rSquared)/rSquared)
    alpha_0<-log(res_var)/2-alpha1^2
    hetError<-rnorm(numObservations,mean=0,sd=exp(alpha_0+alpha1*x_matrix[,1]))
    samp_Y<-pred_y+hetError
    datasets[,,i]<-cbind(x_matrix,samp_Y)
  }
  colnames(datasets)<-c(paste("x",seq(1,numPredictors),sep=""),"y")
  rownames(datasets)<-c(1:numObservations)
  
  return(datasets)
  
}

## -------------- FUNCTION TO GENERATE OUTLIERS IN THE X SPACE USING REPLACEMENT ------------------

# cofefsVec is a vector that indicates which X's should be considered. The Xs are created such that they have really small values (really high is a symmetrical case)
outliers_X<-function(data,nOutliers,coeffsVec,minIQR,maxIQR)
{
  n_datasets<-dim(data)[3]
  n_var<-dim(data)[2]
  n_obs<-dim(data)[1]
  returnDatasets<-array(0,dim=c(n_obs,n_var,n_datasets))
  
  for(i in 1:n_datasets)
  {
    activeData<-data[,,i]
    quant_25<-apply(X=activeData,MARGIN=2,FUN=quantile, probs=c(0.25))
    quant_75<-apply(X=activeData,MARGIN=2,FUN=quantile, probs=c(0.75))
    IQR<-quant_75-quant_25
    
    assignedCols<-sample(coeffsVec,nOutliers,replace=TRUE)
    
    for(k in 1:n_var-1)
    {
      numOutsPred<-sum(assignedCols==k)
      if(numOutsPred>0)
      {
        sampleRows<-sample(c(1:n_obs),numOutsPred,replace = FALSE)
        imputedVals<-runif(numOutsPred,quant_25[k]-maxIQR*IQR[k],quant_25[k]-minIQR*IQR[k])
        activeData[sampleRows,k]<-imputedVals
      }
    }
    
    returnDatasets[,,i]<-activeData
  }
  
  colnames(returnDatasets)<-c(paste("x",seq(1,n_var-1),sep=""),"y")
  rownames(returnDatasets)<-c(1:n_obs)
  
  return(returnDatasets)
}

## -------------- FUNCTION TO GENERATE OUTLIERS IN THE Y SPACE USING REPLACEMENT ------------------

outliers_Y<-function(data,nOutliers,minIQR,maxIQR)
{
  n_datasets<-dim(data)[3]
  n_var<-dim(data)[2]
  n_obs<-dim(data)[1]
  returnDatasets<-array(0,dim=c(n_obs,n_var,n_datasets))
  
  for(i in 1:n_datasets)
  {
    activeData<-data[,,i]
    tempModel<-lm(data=data.frame(activeData),y~.)
    quant_25<-quantile(tempModel$residuals,0.25)
    quant_75<-quantile(tempModel$residuals,0.75)
    IQR_res<-quant_75-quant_25
    
    sampleRows<-sample(c(1:n_obs),nOutliers,replace = FALSE)
    imputedRes<-runif(nOutliers,quant_75+minIQR*IQR_res,quant_75+maxIQR*IQR_res)
    direction<-ifelse(runif(nOutliers,0,1)>0.5,1,-1)
    pred_vals<-predict.lm(tempModel,data.frame(activeData[sampleRows,-n_var]))
    activeData[sampleRows,n_var]<-pred_vals+imputedRes*direction
    
    returnDatasets[,,i]<-activeData
  }
  
  colnames(returnDatasets)<-c(paste("x",seq(1,n_var-1),sep=""),"y")
  rownames(returnDatasets)<-c(1:n_obs)
  
  return(returnDatasets)
}

## --------------- FUNCTION TO GENERATE OUTLIERS IN THE XY SPACE USING REPLACEMENT ----------------

# coeffsVec is a vetcor that indicates which X's should be considered, if "lowHigh" is true, the outliers are created using lo Xs and high Ys, otherwise thy are created with high Xs and high Ys
outliers_XY<-function(data,nOutliers,coeffsVec,lowHigh,minIQR,maxIQR)
{
  n_datasets<-dim(data)[3]
  n_var<-dim(data)[2]
  n_obs<-dim(data)[1]
  returnDatasets<-array(0,dim=c(n_obs,n_var,n_datasets))
  
  for(i in 1:n_datasets)
  {
    activeData<-data[,,i]
    quant_25<-apply(X=activeData,MARGIN=2,FUN=quantile, probs=c(0.25))
    quant_75<-apply(X=activeData,MARGIN=2,FUN=quantile, probs=c(0.75))
    IQR<-quant_75-quant_25
    
    assignedCols<-sample(coeffsVec,nOutliers,replace=TRUE)
    
    for(k in 1:n_var-1)
    {
      numOutsPred<-sum(assignedCols==k)
      if(numOutsPred>0)
      { 
        sampledRows<-sample(c(1:n_obs),numOutsPred,replace = FALSE)
        
        if(lowHigh==TRUE)
        {
          imputed_X<-runif(numOutsPred,quant_25[k]-maxIQR*IQR[k],quant_25[k]-minIQR*IQR[k])  
        }
        else
        {
          imputed_X<-runif(numOutsPred,quant_75[k]+minIQR*IQR[k],quant_75[k]+maxIQR*IQR[k])  
        }
        
        imputed_Y<-runif(numOutsPred,quant_75[n_var]+minIQR*IQR[n_var],quant_75[n_var]+
                           maxIQR*IQR[n_var])
        activeData[sampledRows,k]<-imputed_X
        activeData[sampledRows,n_var]<-imputed_Y
      }
    }
    
    returnDatasets[,,i]<-activeData
  }
  
  colnames(returnDatasets)<-c(paste("x",seq(1,n_var-1),sep=""),"y")
  rownames(returnDatasets)<-c(1:n_obs)
  
  return(returnDatasets)
}

## ------------------------ FUNCTIONS TO PERFORM LIN REG AND ROB REG --------------------------

# Function that performs linear regression on datasets and extracts the coefficients
resultsLR<-function(datasets)
{
  n_datasets<-dim(datasets)[3]
  n_var<-dim(datasets)[2]
  resultsCoeff<-matrix(0,nrow=n_datasets,ncol=n_var)
  resultsSE<-matrix(0,nrow=n_datasets,ncol=n_var)
  resultsCovMat<-array(0,dim=c(n_var,n_var,n_datasets))
  for(i in 1:n_datasets)
  {
    activeData<-data.frame(datasets[,,i])
    model<-lm(activeData$y~activeData$x1+activeData$x2+activeData$x3)
    for(j in 1:n_var)
    {
      resultsCoeff[i,j]<-coefficients(model)[j] 
      resultsSE[i,j]<-coefficients(summary(model))[j,2]
    }
    
    resultsCovMat[,,i]<-vcov(model)
    
  }
  colnames(resultsCoeff)<-c("Intercept","Beta1","Beta2","Beta3")
  rownames(resultsCoeff)<-c(1:n_datasets)
  
  colnames(resultsSE)<-c("SE_Int","SE_B1","SE_B2","SE_B3")
  rownames(resultsSE)<-c(1:n_datasets)
  
  return(list(resultsCoeff,resultsSE,resultsCovMat))
}

# Function that performs robust regression (rlm) on datasets and extracts the results
resultsRLM<-function(datasets)
{
  n_datasets<-dim(datasets)[3]
  n_var<-dim(datasets)[2]
  resultsCoeff<-matrix(0,nrow=n_datasets,ncol=n_var)
  resultsSE<-matrix(0,nrow=n_datasets,ncol=n_var)
  resultsCovMat<-array(0,dim=c(n_var,n_var,n_datasets))
  for(i in 1:n_datasets)
  {
    activeData<-data.frame(datasets[,,i])
    model<-rlm(activeData$y~activeData$x1+activeData$x2+activeData$x3,method="MM",psi=psi.bisquare,init="lts",maxit=100)
    for(j in 1:n_var)
    {
      resultsCoeff[i,j]<-coefficients(model)[j] 
      resultsSE[i,j]<-coefficients(summary(model))[j,2]
    }
    
    resultsCovMat[,,i]<-vcov(model)
    
  }
  colnames(resultsCoeff)<-c("Intercept","Beta1","Beta2","Beta3")
  rownames(resultsCoeff)<-c(1:n_datasets)
  
  colnames(resultsSE)<-c("SE_Int","SE_B1","SE_B2","SE_B3")
  rownames(resultsSE)<-c(1:n_datasets)
  
  return(list(resultsCoeff,resultsSE,resultsCovMat))
}


# Function that performs robust regression lmRob on datasets and extracts the results
resultsLMROB<-function(datasets)
{
  n_datasets<-dim(datasets)[3]
  n_var<-dim(datasets)[2]
  resultsCoeff<-matrix(0,nrow=n_datasets,ncol=n_var)
  resultsSE<-matrix(0,nrow=n_datasets,ncol=n_var)
  resultsCovMat<-array(0,dim=c(n_var,n_var,n_datasets))
  for(i in 1:n_datasets)
  {
    activeData<-data.frame(datasets[,,i])
    model<-lmRob(activeData$y~activeData$x1+activeData$x2+activeData$x3)
    for(j in 1:n_var)
    {
      resultsCoeff[i,j]<-coefficients(model)[j] 
      resultsSE[i,j]<-coefficients(summary(model))[j,2]
    }
    
    resultsCovMat[,,i]<-model$cov
    
  }
  
  colnames(resultsCoeff)<-c("Intercept","Beta1","Beta2","Beta3")
  rownames(resultsCoeff)<-c(1:n_datasets)
  
  colnames(resultsSE)<-c("SE_Int","SE_B1","SE_B2","SE_B3")
  rownames(resultsSE)<-c(1:n_datasets)
  
  return(list(resultsCoeff,resultsSE,resultsCovMat))
}



## -------------------- FUNCTION TO COMPUTE COVERAGE OF ESTIMATES------------------------------

estimatesCoverage<-function(coefResults, SDResults, popCoeffs)
{
  resultsCoverage<-data.frame(matrix(0,nrow=2,ncol=4))
  colnames(resultsCoverage)=c("Intercept","Beta1","Beta2","Beta3")
  rownames(resultsCoverage)=c("Cases","Cov. Prob.")
  for(i in 1:4)
  {
    meanVal<-popCoeffs[i]
    lowB<-coefResults[,i]+qt(0.025,df=n_obs-4)*SDResults[,i]
    upB<-coefResults[,i]+qt(0.975,df=n_obs-4)*SDResults[,i]
    
    resultsCoverage[1,i]<-sum((meanVal>=lowB)&(meanVal<=upB))
    resultsCoverage[2,i]<-resultsCoverage[1,i]/nrow(coefResults)
    
  }
  
  return(resultsCoverage)
}

# Function to extract posterior probabilites with the defined constraints 
BainPostProbs<-function(estimateResults,numObs,ERr1,IRr1,ERr2,IRr2,ERr3,IRr3)
{
  coeffEst<-estimateResults[[1]]
  vcovEst<-estimateResults[[3]]
  iterations<-dim(coeffEst)[1]
  postProbs<-NULL
  for(i in 1:iterations)
  {
    currCoeffs<-coeffEst[i,-1]
    currVcov<-vcovEst[-1,-1,i]
    
    BainResults<-Bain(unlist(currCoeffs),currVcov,nspec=0,njoint=3,samp=numObs,ERr1,IRr1,
                      ERr2,IRr2,ERr3,IRr3,print=FALSE)$testResult$PMPb
    
    postProbs<-rbind(postProbs,c(BainResults,1-sum(BainResults)))
    print(i)
  }
  
  return(postProbs)
}
