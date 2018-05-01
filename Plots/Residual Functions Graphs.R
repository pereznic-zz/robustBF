
library(ggplot2)
library(ggpubr)

residuals<-seq(from=-5, to=5, by=0.01)

huberM<-function(residuals,k)
{
  newRes<-ifelse(abs(residuals)<=k,(residuals^2)/2,k*abs(residuals)-(k^2)/2)
  return(newRes)
}

biSquare<-function(residuals,k)
{
  newRes<-ifelse(abs(residuals)<=k,k^2/6*(1-(1-(residuals/k)^2)^3),k^2/6)
  return(newRes)
}

plotData<-data.frame(residuals,huber=huberM(residuals,1.345),squares=residuals^2,
                     biSq=biSquare(residuals,4.685))

squaresPlot<-ggplot(data=plotData)+
  geom_line(aes(x=residuals,y=squares),color="firebrick",size=1.02)+
  xlab(label=expression(e[i]))+
  ylab(label=expression(rho[M](e[i])))+
  ggtitle("Square function (A)")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=4.685^2/6,lty="dashed",col="brown")


huberPlot<-ggplot(data=plotData)+
  geom_line(aes(x=residuals,y=huber),color="darkblue",size=1.02)+
  xlab(label=expression(e[i]))+
  ylab(label=expression(rho[M](e[i])))+
  ggtitle("Huber function (B)")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=4.685^2/6,lty="dashed",col="brown")

bisquarePlot<-ggplot(data=plotData)+
  geom_line(aes(x=residuals,y=biSq),color="darkgreen",size=1.02)+
  xlab(label=expression(e[i]))+
  ylab(label=expression(rho[M](e[i])))+
  ggtitle("Bisquare function (C)")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=4.685^2/6,lty="dashed",col="brown")+
  ylim(c(0,4.5))

ggarrange(squaresPlot, huberPlot, bisquarePlot,ncol = 3, nrow = 1)

pdf("weight_funcs.pdf", width=9,height = 3)
ggarrange(squaresPlot, huberPlot, bisquarePlot,ncol = 3, nrow = 1)
dev.off()


