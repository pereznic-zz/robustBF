
## -------------------------------- HETEROSCEDASTICITY SIMULATIONS ---------------------------------

set.seed(123)
library(tidyverse)
library(MASS)

# Estimate residual variance under the model log(sigma^2)=alpha_0 + alpha_1*X1
alpha_0<-0
alpha_1<-1

results<-matrix(0,nrow=20,ncol=20)

for(i in 1:20)
{
  alpha_0<-2*i/20
 
  
  for(j in 1:20)
  {
    alpha_1=2*j/20
    stored_res<-NULL 
    for(k in 1:1000)
    {
      sampled_x<-rnorm(10000,0,1)
      residuals<-rnorm(10000,mean=0,sd=exp(alpha_0+alpha_1*sampled_x))
      stored_res<-cbind(stores_res,var(residuals))
    }
    
    results[i,j]<-mean(stored_res)
    rownames(results[i,j])=alpha_0
    colnames(results[i,j])=alpha_1
    
  }
  
}

## --------------------- FUNCTION TO GENERATE DATA WITH HETEROSCEDASTIC RESIDUALS -----------------

# Given a value of alpha 1 sets the value of alpha zero to fit a model with desires R2
genHetData<-function(alpha1,betas,xCorrels,numPredictors,numObservations,numDatasets,rSquared)
{
  datasets<-array(0,dim=c(numObservations,numPredictors+1,numDatasets))
  x_means<-rep(0,numPredictors)
  for(i in 1:numDatasets)
  {
    x_matrix<-matrix(0,nrow=numObservations,ncol=numPredictors)
    x_matrix<-mvrnorm(numObservations,mu=x_means,Sigma=xCorrels,empirical=FALSE)
    pred_y<-apply(t(t(cbind(rep(1,numObservations),x_matrix))*betas),MARGIN=1,FUN=sum)
    res_var<-sum((betas[-1]%*%t(betas[-1])*xCorrels))*(1-rSquared)/rSquared
    alpha_0<-log(res_var)/2-alpha1^2
    hetError<-rnorm(numObservations,mean=0,sd=exp(alpha_0+alpha1*x_matrix[,1]))
    samp_Y<-pred_y+hetError
    datasets[,,i]<-cbind(x_matrix,samp_Y)
  }
  colnames(datasets)<-c(paste("x",seq(1,numPredictors),sep=""),"y")
  rownames(datasets)<-c(1:numObservations)
  
  return(datasets)
  
}

## -------------------------  TESTING THE PROCEDURE WITH A 1000 DATASETS --------------------------

# GENERATING GRAPH IN FIGURE 2.A (WEAK FUNNEL)

# setting the parameters
n_obs<-10000
n_datasets<-1000
n_predictors<-3
betas<-c(2,3,4,5)
r_squared<-0.5
alpha1<-(0.2)
x_correlations<-matrix(c(1,0.11,0.08,0.11,1,0.14,0.08,0.14,1),nrow=3,ncol=3)

# Generating the data
hetData<-genHetData(alpha1,betas,x_correlations,n_predictors,n_obs,n_datasets,r_squared)

# Analyzing residuals and R2 of the simulated data
allresiduals<-matrix(0,n_obs,n_datasets)
R2<-rep(0,1000)
for(i in 1:n_datasets)
{
  activeHet<-data.frame(hetData[,,i])
  activeHet<-activeHet[order(activeHet$x1),]
  allresiduals[,i]<-lm(activeHet$y~activeHet$x1+activeHet$x2+activeHet$x3)$residuals
  R2[i]<-summary(lm(activeHet$y~activeHet$x1+activeHet$x2+activeHet$x3))$r.squared
}

activeHet<-data.frame(hetData[,,1])
activeHet$res<-lm(y~x1+x2+x3, data=activeHet)$residuals

res_var<-sum((betas[-1]%*%t(betas[-1])*x_correlations))*(1-r_squared)/r_squared
alpha_0<-log(res_var)/2-alpha1^2

activeHet$sigma<-exp(alpha_0+alpha1*activeHet$x1)

# Generating the plots in Figure 2
hetPlot<-ggplot(data=activeHet) +
  geom_point(mapping=aes(x=x1,y=res),col="chartreuse4")+
  geom_line(mapping=aes(x=x1,y=sigma),col="darkblue",size=1.05,linetype=2)+
  geom_line(mapping=aes(x=x1,y=-sigma),col="darkblue",size=1.05,linetype=2)+
  geom_line(mapping=aes(x=x1,y=3*sigma),col="brown1",size=1.05,linetype=2)+
  geom_line(mapping=aes(x=x1,y=-3*sigma),col="brown1",size=1.05,linetype=2)+
  xlab("observed x1")+
  ylab("residual")+
  ylim(c(-100,100))

  
pdf("weak_hetero.pdf",height=5, width = 6)
hetPlot
dev.off()

# GENERATING GRAPH IN FIGURE 2.A (STRONG FUNNEL)

# setting the parameters
n_obs<-10000
n_datasets<-1000
n_predictors<-3
betas<-c(2,3,4,5)
r_squared<-0.5
alpha1<-(0.6)
x_correlations<-matrix(c(1,0.11,0.08,0.11,1,0.14,0.08,0.14,1),nrow=3,ncol=3)

# Generating the data
hetData<-genHetData(alpha1,betas,x_correlations,n_predictors,n_obs,n_datasets,r_squared)

# Analyzing residuals and R2 of the simulated data
allresiduals<-matrix(0,n_obs,n_datasets)
R2<-rep(0,1000)
for(i in 1:n_datasets)
{
  activeHet<-data.frame(hetData[,,i])
  activeHet<-activeHet[order(activeHet$x1),]
  allresiduals[,i]<-lm(activeHet$y~activeHet$x1+activeHet$x2+activeHet$x3)$residuals
  R2[i]<-summary(lm(activeHet$y~activeHet$x1+activeHet$x2+activeHet$x3))$r.squared
}

activeHet<-data.frame(hetData[,,1])
activeHet$res<-lm(y~x1+x2+x3, data=activeHet)$residuals

res_var<-sum((betas[-1]%*%t(betas[-1])*x_correlations))*(1-r_squared)/r_squared
alpha_0<-log(res_var)/2-alpha1^2

activeHet$sigma<-exp(alpha_0+alpha1*activeHet$x1)

# Generating the plots in Figure 2
hetPlot<-ggplot(data=activeHet) +
  geom_point(mapping=aes(x=x1,y=res),col="chartreuse4")+
  geom_line(mapping=aes(x=x1,y=sigma),col="darkblue",size=1.05,linetype=2)+
  geom_line(mapping=aes(x=x1,y=-sigma),col="darkblue",size=1.05,linetype=2)+
  geom_line(mapping=aes(x=x1,y=3*sigma),col="brown1",size=1.05,linetype=2)+
  geom_line(mapping=aes(x=x1,y=-3*sigma),col="brown1",size=1.05,linetype=2)+
  xlab("observed x1")+
  ylab("residual")+
  ylim(c(-100,100))


pdf("strong_hetero.pdf",height=5, width = 6)
hetPlot
dev.off()


