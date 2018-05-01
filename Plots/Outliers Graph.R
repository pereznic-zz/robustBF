library(MASS)
library(ggplot2)


genNormData<-function(betas,xCorrels,numPredictors,numObservations,numDatasets,rSquared)
{
  datasets<-array(0,dim=c(numObservations,numPredictors+1,numDatasets))
  x_means<-rep(0,numPredictors)
  for(i in 1:numDatasets)
  {
    x_matrix<-matrix(0,nrow=numObservations,ncol=numPredictors)
    x_matrix<-mvrnorm(numObservations,mu=x_means,Sigma=xCorrels,empirical=FALSE)
    pred_y<-apply(t(t(cbind(rep(1,numObservations),x_matrix))*betas),MARGIN=1,FUN=sum)
    res_var<-sum((betas[-1]%*%t(betas[-1])*xCorrels))*(1-rSquared)/rSquared
    samp_Y<-pred_y+rnorm(numObservations,0,sqrt(res_var))
    datasets[,,i]<-cbind(x_matrix,samp_Y)
  }
  colnames(datasets)<-c(paste("x",seq(1,numPredictors),sep=""),"y")
  rownames(datasets)<-c(1:numObservations)
  
  return(datasets)
  
}

set.seed(65432)

plotData<-data.frame(genNormData(c(0,1),matrix(c(1),nrow=1,ncol=1),1,300,1,0.5))
colnames(plotData)<-c("x1","y")
colnames(plotData)<-c("x","y")

model1<-lm(data=plotData,y~x)
resModel1<-model1$residuals
predict.lm(model1,c(lowerX))

IQR_X<-quantile(plotData$x,0.75)-quantile(plotData$x,0.25)
LowerX<-quantile(plotData$x,0.25)-1.5*IQR_X
UpperX<-quantile(plotData$x,0.75)+1.5*IQR_X

IQR_Y<-quantile(resModel1,0.75)-quantile(resModel1,0.25)
LowerY1<-predict.lm(model1,data.frame(x=c(LowerX)))-1.5*IQR_Y
UpperY1<-predict.lm(model1,data.frame(x=c(LowerX)))+1.5*IQR_Y
LowerY2<-predict.lm(model1,data.frame(x=c(UpperX)))-1.5*IQR_Y
UpperY2<-predict.lm(model1,data.frame(x=c(UpperX)))+1.5*IQR_Y

IQR_Y_Prime<-quantile(plotData$y,0.75)-quantile(plotData$y,0.25)
LowerYPrime<-quantile(plotData$y,0.25)-1.5*IQR_Y_Prime
UpperYPrime<-quantile(plotData$y,0.75)+1.5*IQR_Y_Prime

polygon1<-data.frame(x=c(LowerX,UpperX,UpperX,LowerX),y=c(6.5,6.5,UpperY2,LowerY2))
polygon2<-data.frame(x=c(LowerX,UpperX,UpperX,LowerX),y=c(-6.5,-6.5,UpperY1,LowerY1))

a<-ggplot(data=plotData,aes(x=x,y=y))+
  geom_point(aes(x=x,y=y),col="dodgerblue")+
  stat_ellipse(col="firebrick",size=1.05)+
  xlim(-3,3)+
  ylim(-6.5,6.5)+
  annotate("rect", xmin = -Inf, xmax = LowerX, ymin = Inf, ymax = UpperYPrime, fill= "navy",alpha=0.3)+
  annotate("rect", xmin = -Inf, xmax = LowerX, ymin = -Inf, ymax = LowerYPrime, fill= "navy",alpha=0.3)+
  annotate("rect", xmin = -Inf, xmax = LowerX, ymin = LowerYPrime, ymax = UpperYPrime, fill= "green3",alpha=0.3)+
  annotate("rect", xmin = UpperX, xmax = Inf, ymin = -Inf, ymax = LowerYPrime, fill= "navy",alpha=0.3)+
  annotate("rect", xmin = UpperX, xmax = Inf, ymin = Inf, ymax = UpperYPrime, fill= "navy",alpha=0.3)+
  annotate("rect", xmin = UpperX, xmax = Inf, ymin = LowerYPrime, ymax = UpperYPrime, fill= "green3",alpha=0.3)+
  annotate("rect", xmin = LowerX, xmax = UpperX, ymin = 6.5, ymax = Inf, fill= "gold",alpha=0.3)+
  annotate("rect", xmin = LowerX, xmax = UpperX, ymin = -6.5, ymax = -Inf, fill= "gold",alpha=0.3)+
  annotate("text", x = 2.9, y = -0.5, label = "A")+
  annotate("text", x = -2.9, y = 0.5, label = "A")+
  annotate("text", x = 0, y = 5, label = "B")+
  annotate("text", x = 0, y = -5, label = "B")+
  annotate("text", x = -2.9, y = 5.65, label = "C")+
  annotate("text", x = -2.9, y = -5.65, label = "C")+
  annotate("text", x = 2.9, y = 5.65, label = "C")+
  annotate("text", x = 2.9, y = -5.65, label = "C")+
  geom_segment(aes(x = -3, xend = 3, y = -3 , yend = 3),color="black",size=1.02)+
  geom_polygon(data=polygon1,aes(x,y),fill="gold",alpha=0.3)+
  geom_polygon(data=polygon2,aes(x,y),fill="gold",alpha=0.3)
  
pdf("outlierScenarios.pdf",width=9,height=6)
a
dev.off()



