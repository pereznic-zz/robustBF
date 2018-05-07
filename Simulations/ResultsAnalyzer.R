library(tidyverse)
library(ggpubr)

rm(list=ls())

# Generate matrix with reference population coefficients in each scenario
coeffsMat<-matrix(c(3,0,0,0,5,4,3,2,5,4,3,-2),nrow=3,ncol=4,byrow = TRUE)
refPopcoeffs<-cbind(rep(1:30,3),do.call(rbind, replicate(30, coeffsMat, simplify=FALSE)))

# Read Posterior Probabilities results obtained in Scenarios simulations
PP_results_LR<-data.frame()
PP_results_RLM<-data.frame()
PP_results_LMROB<-data.frame()

# Read coefficients results obtained in Scenarios simulations
Est_results_LR<-data.frame()
Est_results_RLM<-data.frame()
Est_results_LMROB<-data.frame()

# Formating the data results
for(i in 1:30)
{
  currScen<-paste(paste(paste(resultsPath,"Scenario",sep=""),i,sep=""),"_LR_Probs.csv",sep="")
  temp<-read.csv(file=currScen,header=TRUE)
  temp[,1]<-NULL
  temp[,5]<-i
  PP_results_LR<-rbind(PP_results_LR,temp)
  
  currScen<-paste(paste(paste(resultsPath,"Scenario",sep=""),i,sep=""),"_RLM_Probs.csv",sep="")
  temp<-read.csv(file=currScen,header=TRUE)
  temp[,1]<-NULL
  temp[,5]<-i
  PP_results_RLM<-rbind(PP_results_RLM,temp)
  
  currScen<-paste(paste(paste(resultsPath,"Scenario",sep=""),i,sep=""),"_LMROB_Probs.csv",sep="")
  temp<-read.csv(file=currScen,header=TRUE)
  temp[,1]<-NULL
  temp[,5]<-i
  PP_results_LMROB<-rbind(PP_results_LMROB,temp)
  
  currScen<-paste(paste(paste(resultsPath,"Scenario",sep=""),i,sep=""),"_LR_Betas.csv",sep="")
  temp<-read.csv(file=currScen,header=TRUE)
  temp[,1]<-NULL
  temp[,5]<-i
  Est_results_LR<-rbind(Est_results_LR,temp)
  
  currScen<-paste(paste(paste(resultsPath,"Scenario",sep=""),i,sep=""),"_RLM_Betas.csv",sep="")
  temp<-read.csv(file=currScen,header=TRUE)
  temp[,1]<-NULL
  temp[,5]<-i
  Est_results_RLM<-rbind(Est_results_RLM,temp)
  
  currScen<-paste(paste(paste(resultsPath,"Scenario",sep=""),i,sep=""),"_LMROB_Betas.csv",sep="")
  temp<-read.csv(file=currScen,header=TRUE)
  temp[,1]<-NULL
  temp[,5]<-i
  Est_results_LMROB<-rbind(Est_results_LMROB,temp)
}

# Renaming variables in the joint results files
colnames(PP_results_LR)<-c("H1","H2","H3","H4","Scenario")
colnames(PP_results_RLM)<-c("H1","H2","H3","H4","Scenario")
colnames(PP_results_LMROB)<-c("H1","H2","H3","H4","Scenario")
colnames(Est_results_LR)<-c("Intercept","Beta1","Beta2","Beta3","Scenario")
colnames(Est_results_RLM)<-c("Intercept","Beta1","Beta2","Beta3","Scenario")
colnames(Est_results_LMROB)<-c("Intercept","Beta1","Beta2","Beta3","Scenario")

# Summarizing results for coefficients and posterior probabilites for sets of scenarios under the same hypothesis
group_H1=seq(from=1, to=28 ,by=3)
group_H2=seq(from=2, to=29 ,by=3)
group_H3=seq(from=3, to=30 ,by=3)

# Summarizing coefficients of the LR models for sets of scenarios under the same hypothesis
sumBetasLR<-Est_results_LR %>%
  group_by(Scenario) %>%
  summarise(avgIntercept=mean(Intercept),avgBeta1=mean(Beta1),avgBeta2=mean(Beta2),
            avgBeta3=mean(Beta3)) %>%
  mutate(Method="OLS")

# Summarizing coefficients of the RLM models for sets of scenarios under the same hypothesis
sumBetasRLM<-Est_results_RLM %>%
  group_by(Scenario) %>%
  summarise(avgIntercept=mean(Intercept),avgBeta1=mean(Beta1),avgBeta2=mean(Beta2),
            avgBeta3=mean(Beta3))%>%
  mutate(Method="MM")

# Summarizing coefficients of the LMROB models for sets of scenarios under the same hypothesis
sumBetasLMROB<-Est_results_LMROB %>%
  group_by(Scenario) %>%
  summarise(avgIntercept=mean(Intercept),avgBeta1=mean(Beta1),avgBeta2=mean(Beta2),
            avgBeta3=mean(Beta3)) %>%
  mutate(Method="LMROB")

# Summarizing posterior model probabilities in the LR model
sumPP_LR<-data.frame(cbind(t(apply(PP_results_LR[,1:4],1,function(x)x==max(x))),
                Scenario=PP_results_LR[,5]),Method="OLS")
# Summarizing posterior model probabilities in the RLM model
sumPP_RLM<-data.frame(cbind(t(apply(PP_results_RLM[,1:4],1,function(x)x==max(x))),
                 Scenario=PP_results_RLM[,5]),Method="MM")
# Summarizing posterior model probabilities in the LMROB model
sumPP_LMROB<-data.frame(cbind(t(apply(PP_results_LMROB[,1:4],1,function(x)x==max(x))),
                   Scenario=PP_results_LMROB[,5]),Method="LMROB")

# Counting and summarizing amount of right classified for LR models for sets of scenarios under the same hypothesis
class_LR<-sumPP_LR %>%
  group_by(Scenario) %>%
  summarise(nH1=sum(H1),nH2=sum(H2),nH3=sum(H3),nH4=sum(H4)) %>%
  mutate(Method="OLS")

# Counting and summarizing amount of right classified for RLM models for sets of scenarios under the same hypothesis
class_RLM<-sumPP_RLM %>%
  group_by(Scenario) %>%
  summarise(nH1=sum(H1),nH2=sum(H2),nH3=sum(H3),nH4=sum(H4)) %>%
  mutate(Method="MM")

# Counting and summarizing amount of right classified for LMROB models for sets of scenarios under the same hypothesis
class_LMROB<-sumPP_LMROB %>%
  group_by(Scenario) %>%
  summarise(nH1=sum(H1),nH2=sum(H2),nH3=sum(H3),nH4=sum(H4)) %>%
  mutate(Method="LMROB")

PProbsResults<-do.call("rbind",list(class_LR,class_RLM,class_LMROB))

# Computing the  bias
coeffsResults<-do.call("rbind",list(sumBetasLR,sumBetasRLM,sumBetasLMROB))
coeffsBias<-(coeffsResults[,2:5]-refPopcoeffs[,2:5])
coeffsBiasResults<-cbind(coeffsResults[,-(2:5)],coeffsBias)

# Split results in scenarios that are comparable
coeffsBiasResults_H1<-filter(coeffsBiasResults,Scenario %in% group_H1)
coeffsBiasResults_H2<-filter(coeffsBiasResults,Scenario %in% group_H2)
coeffsBiasResults_H3<-filter(coeffsBiasResults,Scenario %in% group_H3)

PProbsResults_H1<-filter(PProbsResults,Scenario %in% group_H1)
PProbsResults_H2<-filter(PProbsResults,Scenario %in% group_H2)
PProbsResults_H3<-filter(PProbsResults,Scenario %in% group_H3)

# Code to generate the righlty classified datasets plot (Figure 6 in the paper)
PPH1<-ggplot(data=PProbsResults_H1)+
  geom_point(aes(x=Scenario,y=nH1/1000,colour=Method))+
  geom_line(aes(x=Scenario,y=nH1/1000,colour=Method))+
  ylim(c(0,1))+
  xlab("Scenario")+
  ylab(expression("P"*"("*H[1]*")"))+
  annotate("text", label = "S1", x = 1, y = 0.87, size = 3, colour = "darkblue")+
  annotate("text", label = "S4", x = 4, y = 0.87, size = 3, colour = "darkblue")+
  annotate("text", label = "S7", x = 7, y = 0.87, size = 3, colour = "darkblue")+
  annotate("text", label = "S10", x = 10, y = 0.930, size = 3, colour = "darkblue")+
  annotate("text", label = "S13", x = 13, y = 0.900, size = 3, colour = "darkblue")+
  annotate("text", label = "S16", x = 16, y = 0.940, size = 3, colour = "darkblue")+
  annotate("text", label = "S19", x = 19, y = 0.970, size = 3, colour = "darkblue")+
  annotate("text", label = "S22", x = 22, y = 0.970, size = 3, colour = "darkblue")+
  annotate("text", label = "S25", x = 25, y = 0.625, size = 3, colour = "darkblue")+
  annotate("text", label = "S28", x = 28, y = 0.95, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())

PPH2<-ggplot(data=PProbsResults_H2)+
  geom_point(aes(x=Scenario,y=nH2/1000,colour=Method))+
  geom_line(aes(x=Scenario,y=nH2/1000,colour=Method))+
  ylim(c(0,1))+
  xlab("Scenario")+
  ylab(expression("P"*"("*H[2]*")"))+
  annotate("text", label = "S2", x = 2, y = 0.45, size = 3, colour = "darkblue")+
  annotate("text", label = "S5", x = 5, y = 0.45, size = 3, colour = "darkblue")+
  annotate("text", label = "S8", x = 8, y = 0.4, size = 3, colour = "darkblue")+
  annotate("text", label = "S11", x = 11, y = 0.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S14", x = 14, y = 0.45, size = 3, colour = "darkblue")+
  annotate("text", label = "S17", x = 17, y = 0.4, size = 3, colour = "darkblue")+
  annotate("text", label = "S20", x = 20, y = 0.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S23", x = 23, y = 0.6, size = 3, colour = "darkblue")+
  annotate("text", label = "S26", x = 26, y = 0.55, size = 3, colour = "darkblue")+
  annotate("text", label = "S29", x = 29, y = 0.3, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())

PPH3<-ggplot(data=PProbsResults_H3)+
  geom_point(aes(x=Scenario,y=nH3/1000,colour=Method))+
  geom_line(aes(x=Scenario,y=nH3/1000,colour=Method))+
  ylim(c(0,1.00))+
  xlab("Scenario")+
  ylab(expression("P"*"("*H[3]*")"))+
  annotate("text", label = "S3", x = 3, y = 0.87, size = 3, colour = "darkblue")+
  annotate("text", label = "S6", x = 6, y = 0.75, size = 3, colour = "darkblue")+
  annotate("text", label = "S9", x = 9, y = 0.67, size = 3, colour = "darkblue")+
  annotate("text", label = "S12", x = 12, y = 0.75, size = 3, colour = "darkblue")+
  annotate("text", label = "S15", x = 15, y = 0.55, size = 3, colour = "darkblue")+
  annotate("text", label = "S18", x = 18, y = 0.7, size = 3, colour = "darkblue")+
  annotate("text", label = "S21", x = 21, y = 0.6, size = 3, colour = "darkblue")+
  annotate("text", label = "S24", x = 24, y = 0.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S27", x = 27, y = 0.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S30", x = 30, y = 0.87, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())

PPCs_graph<-ggarrange(PPH1, PPH2, PPH3, ncol = 1, nrow = 3)

# Exporting graph for the PCCs in pdf format
pdf("PPC_graph.pdf",height=9,width=11)
PPCs_graph
dev.off()

# Code to generate the Bias graphs for H1 
BiasH1Int<-ggplot(data=coeffsBiasResults_H1)+
  geom_point(aes(x=Scenario,y=avgIntercept,colour=Method))+
  geom_line(aes(x=Scenario,y=avgIntercept,colour=Method))+
  xlab("Scenario")+
  ylab(expression(beta[0]*" bias "*" "*H[1]))+
  ylim(c(0,0.8))+
  annotate("text", label = "S1", x = 1, y = 0.05, size = 3, colour = "darkblue")+
  annotate("text", label = "S4", x = 4, y = 0.05, size = 3, colour = "darkblue")+
  annotate("text", label = "S7", x = 7, y = 0.2, size = 3, colour = "darkblue")+
  annotate("text", label = "S10", x = 10, y = 0.45, size = 3, colour = "darkblue")+
  annotate("text", label = "S13", x = 13, y = 0.55, size = 3, colour = "darkblue")+
  annotate("text", label = "S16", x = 16, y = 0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S19", x = 19, y = 0.7, size = 3, colour = "darkblue")+
  annotate("text", label = "S22", x = 22, y = 0.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S25", x = 25, y = 0.3, size = 3, colour = "darkblue")+
  annotate("text", label = "S28", x = 28, y = 0.05, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

BiasH1B1<-ggplot(data=coeffsBiasResults_H1)+
  geom_point(aes(x=Scenario,y=avgBeta1,colour=Method))+
  geom_line(aes(x=Scenario,y=avgBeta1,colour=Method))+
  xlab("Scenario")+
  ylab(expression(beta[1]*" bias "*" "*H[1]))+
  ylim(c(-0.8,0.1))+
  annotate("text", label = "S1", x = 1, y = -0.05, size = 3, colour = "darkblue")+
  annotate("text", label = "S4", x = 4, y = -0.05, size = 3, colour = "darkblue")+
  annotate("text", label = "S7", x = 7, y = -0.2, size = 3, colour = "darkblue")+
  annotate("text", label = "S10", x = 10, y = -0.45, size = 3, colour = "darkblue")+
  annotate("text", label = "S13", x = 13, y = -0.55, size = 3, colour = "darkblue")+
  annotate("text", label = "S16", x = 16, y = -0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S19", x = 19, y = -0.7, size = 3, colour = "darkblue")+
  annotate("text", label = "S22", x = 22, y = -0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S25", x = 25, y = -0.3, size = 3, colour = "darkblue")+
  annotate("text", label = "S28", x = 28, y = -0.05, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

BiasH1B2<-ggplot(data=coeffsBiasResults_H1)+
  geom_point(aes(x=Scenario,y=avgBeta2,colour=Method))+
  geom_line(aes(x=Scenario,y=avgBeta2,colour=Method))+
  xlab("Scenario")+
  ylab(expression(beta[2]*" bias "*" "*H[1]))+
  ylim(c(-0.8,0.1))+
  annotate("text", label = "S1", x = 1, y = -0.05, size = 3, colour = "darkblue")+
  annotate("text", label = "S4", x = 4, y = -0.05, size = 3, colour = "darkblue")+
  annotate("text", label = "S7", x = 7, y = -0.2, size = 3, colour = "darkblue")+
  annotate("text", label = "S10", x = 10, y = -0.45, size = 3, colour = "darkblue")+
  annotate("text", label = "S13", x = 13, y = -0.55, size = 3, colour = "darkblue")+
  annotate("text", label = "S16", x = 16, y = -0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S19", x = 19, y = -0.7, size = 3, colour = "darkblue")+
  annotate("text", label = "S22", x = 22, y = -0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S25", x = 25, y = -0.3, size = 3, colour = "darkblue")+
  annotate("text", label = "S28", x = 28, y = -0.05, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

BiasH1B3<-ggplot(data=coeffsBiasResults_H1)+
  geom_point(aes(x=Scenario,y=avgBeta3,colour=Method))+
  geom_line(aes(x=Scenario,y=avgBeta3,colour=Method))+
  xlab("Scenario")+
  ylab(expression(beta[3]*" bias "*" "*H[1]))+
  ylim(c(-0.8,0.1))+
  annotate("text", label = "S1", x = 1, y = -0.05, size = 3, colour = "darkblue")+
  annotate("text", label = "S4", x = 4, y = -0.05, size = 3, colour = "darkblue")+
  annotate("text", label = "S7", x = 7, y = -0.2, size = 3, colour = "darkblue")+
  annotate("text", label = "S10", x = 10, y = -0.45, size = 3, colour = "darkblue")+
  annotate("text", label = "S13", x = 13, y = -0.55, size = 3, colour = "darkblue")+
  annotate("text", label = "S16", x = 16, y = -0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S19", x = 19, y = -0.7, size = 3, colour = "darkblue")+
  annotate("text", label = "S22", x = 22, y = -0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S25", x = 25, y = -0.3, size = 3, colour = "darkblue")+
  annotate("text", label = "S28", x = 28, y = -0.05, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")


# Code to generate the Bias graphs for H2
BiasH2Int<-ggplot(data=coeffsBiasResults_H2)+
  geom_point(aes(x=Scenario,y=avgIntercept,colour=Method))+
  geom_line(aes(x=Scenario,y=avgIntercept,colour=Method))+
  xlab("Scenario")+
  ylab(expression(beta[0]*" bias "*" "*H[2]))+
  annotate("text", label = "S2", x = 2, y = 0.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S5", x = 5, y = 1.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S8", x = 8, y = 1, size = 3, colour = "darkblue")+
  annotate("text", label = "S11", x = 11, y = 4, size = 3, colour = "darkblue")+
  annotate("text", label = "S14", x = 14, y = 5, size = 3, colour = "darkblue")+
  annotate("text", label = "S17", x = 17, y = 6, size = 3, colour = "darkblue")+
  annotate("text", label = "S20", x = 20, y = 6.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S23", x = 23, y = 5, size = 3, colour = "darkblue")+
  annotate("text", label = "S26", x = 26, y = 3, size = 3, colour = "darkblue")+
  annotate("text", label = "S29", x = 29, y = 0.5, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

BiasH2B1<-ggplot(data=coeffsBiasResults_H2)+
  geom_point(aes(x=Scenario,y=avgBeta1,colour=Method))+
  geom_line(aes(x=Scenario,y=avgBeta1,colour=Method))+
  xlab("Scenario")+
  ylab(expression(beta[1]*" bias "*" "*H[2]))+
  ylim(c(-6.8,0.1))+
  annotate("text", label = "S2", x = 2, y = -0.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S5", x = 5, y = -1.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S8", x = 8, y = -1, size = 3, colour = "darkblue")+
  annotate("text", label = "S11", x = 11, y =- 4.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S14", x = 14, y = -5.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S17", x = 17, y = -6.2, size = 3, colour = "darkblue")+
  annotate("text", label = "S20", x = 20, y = -6.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S23", x = 23, y = -6.6, size = 3, colour = "darkblue")+
  annotate("text", label = "S26", x = 26, y = -3, size = 3, colour = "darkblue")+
  annotate("text", label = "S29", x = 29, y = -0.5, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

BiasH2B2<-ggplot(data=coeffsBiasResults_H2)+
  geom_point(aes(x=Scenario,y=avgBeta2,colour=Method))+
  geom_line(aes(x=Scenario,y=avgBeta2,colour=Method))+
  xlab("Scenario")+
  ylab(expression(beta[2]*" bias "*" "*H[2]))+
  ylim(c(-6.8,0.1))+
  annotate("text", label = "S2", x = 2, y = -0.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S5", x = 5, y = -1.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S8", x = 8, y = -1, size = 3, colour = "darkblue")+
  annotate("text", label = "S11", x = 11, y =- 4.3, size = 3, colour = "darkblue")+
  annotate("text", label = "S14", x = 14, y = -5.2, size = 3, colour = "darkblue")+
  annotate("text", label = "S17", x = 17, y = -5.8, size = 3, colour = "darkblue")+
  annotate("text", label = "S20", x = 20, y = -6.3, size = 3, colour = "darkblue")+
  annotate("text", label = "S23", x = 23, y = -6.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S26", x = 26, y = -3, size = 3, colour = "darkblue")+
  annotate("text", label = "S29", x = 29, y = -0.5, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

BiasH2B3<-ggplot(data=coeffsBiasResults_H2)+
  geom_point(aes(x=Scenario,y=avgBeta3,colour=Method))+
  geom_line(aes(x=Scenario,y=avgBeta3,colour=Method))+
  xlab("Scenario")+
  ylab(expression(beta[3]*" bias "*" "*H[2]))+
  ylim(c(-6.8,0.1))+
  annotate("text", label = "S2", x = 2, y = -0.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S5", x = 5, y = -1.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S8", x = 8, y = -1, size = 3, colour = "darkblue")+
  annotate("text", label = "S11", x = 11, y =- 4.1, size = 3, colour = "darkblue")+
  annotate("text", label = "S14", x = 14, y = -5., size = 3, colour = "darkblue")+
  annotate("text", label = "S17", x = 17, y = -5.6, size = 3, colour = "darkblue")+
  annotate("text", label = "S20", x = 20, y = -6.1, size = 3, colour = "darkblue")+
  annotate("text", label = "S23", x = 23, y = -6.1, size = 3, colour = "darkblue")+
  annotate("text", label = "S26", x = 26, y = -3, size = 3, colour = "darkblue")+
  annotate("text", label = "S29", x = 29, y = -0.5, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")


# Code to generate the Bias graphs for H3
BiasH3Int<-ggplot(data=coeffsBiasResults_H3)+
  geom_point(aes(x=Scenario,y=avgIntercept,colour=Method))+
  geom_line(aes(x=Scenario,y=avgIntercept,colour=Method))+
  xlab("Scenario")+
  ylab(expression(beta[0]*" bias "*" "*H[3]))+
  annotate("text", label = "S3", x = 3, y = 0.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S6", x = 6, y = 1.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S9", x = 9, y = 1, size = 3, colour = "darkblue")+
  annotate("text", label = "S12", x = 12, y = 3.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S15", x = 15, y = 4.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S18", x = 18, y = 5, size = 3, colour = "darkblue")+
  annotate("text", label = "S21", x = 21, y = 5.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S24", x = 24, y = 5, size = 3, colour = "darkblue")+
  annotate("text", label = "S27", x = 27, y = 3, size = 3, colour = "darkblue")+
  annotate("text", label = "S30", x = 30, y = 0.5, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

BiasH3B1<-ggplot(data=coeffsBiasResults_H3)+
  geom_point(aes(x=Scenario,y=avgBeta1,colour=Method))+
  geom_line(aes(x=Scenario,y=avgBeta1,colour=Method))+
  xlab("Scenario")+
  ylab(expression(beta[1]*" bias "*" "*H[3]))+
  ylim(c(-6.8,1.5))+
  annotate("text", label = "S3", x = 3, y = -0.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S6", x = 6, y = -1.8, size = 3, colour = "darkblue")+
  annotate("text", label = "S9", x = 9, y = -1, size = 3, colour = "darkblue")+
  annotate("text", label = "S12", x = 12, y = -4, size = 3, colour = "darkblue")+
  annotate("text", label = "S15", x = 15, y = -5.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S18", x = 18, y = -6, size = 3, colour = "darkblue")+
  annotate("text", label = "S21", x = 21, y = -6.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S24", x = 24, y = -6.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S27", x = 27, y = -3, size = 3, colour = "darkblue")+
  annotate("text", label = "S30", x = 30, y = -0.5, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

BiasH3B2<-ggplot(data=coeffsBiasResults_H3)+
  geom_point(aes(x=Scenario,y=avgBeta2,colour=Method))+
  geom_line(aes(x=Scenario,y=avgBeta2,colour=Method))+
  xlab("Scenario")+
  ylab(expression(beta[2]*" bias "*" "*H[3]))+
  ylim(c(-6.8,1.5))+
  annotate("text", label = "S3", x = 3, y = -0.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S6", x = 6, y = -1.8, size = 3, colour = "darkblue")+
  annotate("text", label = "S9", x = 9, y = -1, size = 3, colour = "darkblue")+
  annotate("text", label = "S12", x = 12, y = -4, size = 3, colour = "darkblue")+
  annotate("text", label = "S15", x = 15, y = -5, size = 3, colour = "darkblue")+
  annotate("text", label = "S18", x = 18, y = -5.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S21", x = 21, y = -6, size = 3, colour = "darkblue")+
  annotate("text", label = "S24", x = 24, y = -6, size = 3, colour = "darkblue")+
  annotate("text", label = "S27", x = 27, y = -3, size = 3, colour = "darkblue")+
  annotate("text", label = "S30", x = 30, y = -0.5, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

BiasH3B3<-ggplot(data=coeffsBiasResults_H3)+
  geom_point(aes(x=Scenario,y=avgBeta3,colour=Method))+
  geom_line(aes(x=Scenario,y=avgBeta3,colour=Method))+
  xlab("Scenario")+
  ylab(expression(beta[3]*" bias "*" "*H[3]))+
  ylim(c(-6.8,1.5))+
  annotate("text", label = "S3", x = 3, y = -0.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S6", x = 6, y = -0, size = 3, colour = "darkblue")+
  annotate("text", label = "S9", x = 9, y = -1, size = 3, colour = "darkblue")+
  annotate("text", label = "S12", x = 12, y = -3, size = 3, colour = "darkblue")+
  annotate("text", label = "S15", x = 15, y = -3.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S18", x = 18, y = -3.7, size = 3, colour = "darkblue")+
  annotate("text", label = "S21", x = 21, y = -4, size = 3, colour = "darkblue")+
  annotate("text", label = "S24", x = 24, y = -3.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S27", x = 27, y = -2, size = 3, colour = "darkblue")+
  annotate("text", label = "S30", x = 30, y = -0.5, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")


# Exporting the gridplot for bias in different hypotheses (Figure 4)
bias_graph<-ggarrange(BiasH1Int, BiasH2Int, BiasH3Int, BiasH1B1, BiasH2B1, BiasH3B1,
                      BiasH1B2,BiasH2B2, BiasH3B2, BiasH1B3, BiasH2B3, BiasH3B3,
                      ncol = 3, nrow = 4)

pdf("bias_graph.pdf",height=14,width=12)
bias_graph
dev.off()

# Separate plots for bias for each paraemter
bias_graph_ints<-ggarrange(BiasH1Int, BiasH2Int, BiasH3Int, ncol = 3, nrow = 1)
bias_graph_b1<-ggarrange(BiasH1B1, BiasH2B1, BiasH3B1, ncol = 3, nrow = 1)
bias_graph_b2<-ggarrange(BiasH1B2,BiasH2B2, BiasH3B2, ncol = 3, nrow = 1)
bias_graph_b3<-ggarrange(BiasH1B3, BiasH2B3, BiasH3B3, ncol = 3, nrow = 1)

pdf("bias_graph_ints.pdf",height=4,width=13)
bias_graph_ints
dev.off()

pdf("bias_graph_b1.pdf",height=4,width=13)
bias_graph_b1
dev.off()

pdf("bias_graph_b2.pdf",height=4,width=13)
bias_graph_b2
dev.off()

pdf("bias_graph_b3.pdf",height=4,width=13)
bias_graph_b3
dev.off()

# Read results for cover probabilities
covProbs<-read.csv2("CovProbs.csv",header=TRUE)
colnames(covProbs)<-c("Scenario","Method","Intercept","Beta1","Beta2","Beta3")
covProbs[,3:6]<-covProbs[,3:6]/1000

# Split results in scenarios that are comparable
covProbs_H1<-filter(covProbs,Scenario %in% group_H1)
covProbs_H2<-filter(covProbs,Scenario %in% group_H2)
covProbs_H3<-filter(covProbs,Scenario %in% group_H3)

# Code to generate the coverage probabilities graph for H1
CPH1Int<-ggplot(data=covProbs_H1)+
  geom_point(aes(x=Scenario,y=Intercept,colour=Method))+
  geom_line(aes(x=Scenario,y=Intercept,colour=Method))+
  xlab("Scenario")+
  ylab(expression("CP "*beta[0]*" "*H[1]))+
  ylim(c(0,1))+
  annotate("text", label = "S1", x = 1, y = 0.85, size = 3, colour = "darkblue")+
  annotate("text", label = "S4", x = 4, y = 0.85, size = 3, colour = "darkblue")+
  annotate("text", label = "S7", x = 7, y = 0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S10", x = 10, y = 0.45, size = 3, colour = "darkblue")+
  annotate("text", label = "S13", x = 13, y = 0.25, size = 3, colour = "darkblue")+
  annotate("text", label = "S16", x = 16, y = 0.15, size = 3, colour = "darkblue")+
  annotate("text", label = "S19", x = 19, y = 0.1, size = 3, colour = "darkblue")+
  annotate("text", label = "S22", x = 22.8, y = 0.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S25", x = 25.5, y = 0.8, size = 3, colour = "darkblue")+
  annotate("text", label = "S28", x = 28, y = 0.7, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

CPH1B1<-ggplot(data=covProbs_H1)+
  geom_point(aes(x=Scenario,y=Beta1,colour=Method))+
  geom_line(aes(x=Scenario,y=Beta1,colour=Method))+
  xlab("Scenario")+
  ylab(expression("CP "*beta[1]*" "*H[1]))+
  ylim(c(0,1))+
  annotate("text", label = "S1", x = 1, y = 0.85, size = 3, colour = "darkblue")+
  annotate("text", label = "S4", x = 4, y = 0.85, size = 3, colour = "darkblue")+
  annotate("text", label = "S7", x = 7, y = 0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S10", x = 10, y = 0.33, size = 3, colour = "darkblue")+
  annotate("text", label = "S13", x = 13, y = 0.15, size = 3, colour = "darkblue")+
  annotate("text", label = "S16", x = 16, y = 0.05, size = 3, colour = "darkblue")+
  annotate("text", label = "S19", x = 19, y = 0.0, size = 3, colour = "darkblue")+
  annotate("text", label = "S22", x = 22, y = 1, size = 3, colour = "darkblue")+
  annotate("text", label = "S25", x = 25.5, y = 0.6, size = 3, colour = "darkblue")+
  annotate("text", label = "S28", x = 28, y = 0.7, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

CPH1B2<-ggplot(data=covProbs_H1)+
  geom_point(aes(x=Scenario,y=Beta2,colour=Method))+
  geom_line(aes(x=Scenario,y=Beta2,colour=Method))+
  xlab("Scenario")+
  ylab(expression("CP "*beta[2]*" "*H[1]))+
  ylim(c(0,1))+
  annotate("text", label = "S1", x = 1, y = 0.85, size = 3, colour = "darkblue")+
  annotate("text", label = "S4", x = 4, y = 0.85, size = 3, colour = "darkblue")+
  annotate("text", label = "S7", x = 7, y = 0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S10", x = 10, y = 0.35, size = 3, colour = "darkblue")+
  annotate("text", label = "S13", x = 13, y = 0.15, size = 3, colour = "darkblue")+
  annotate("text", label = "S16", x = 16, y = 0.05, size = 3, colour = "darkblue")+
  annotate("text", label = "S19", x = 19, y = 0.0, size = 3, colour = "darkblue")+
  annotate("text", label = "S22", x = 22, y = 1, size = 3, colour = "darkblue")+
  annotate("text", label = "S25", x = 25.5, y = 0.6, size = 3, colour = "darkblue")+
  annotate("text", label = "S28", x = 28, y = 0.8, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

CPH1B3<-ggplot(data=covProbs_H1)+
  geom_point(aes(x=Scenario,y=Beta3,colour=Method))+
  geom_line(aes(x=Scenario,y=Beta3,colour=Method))+
  xlab("Scenario")+
  ylab(expression("CP "*beta[3]*" "*H[1]))+
  ylim(c(0,1))+
  annotate("text", label = "S1", x = 1, y = 0.85, size = 3, colour = "darkblue")+
  annotate("text", label = "S4", x = 4, y = 0.85, size = 3, colour = "darkblue")+
  annotate("text", label = "S7", x = 7, y = 0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S10", x = 10, y = 0.35, size = 3, colour = "darkblue")+
  annotate("text", label = "S13", x = 13, y = 0.15, size = 3, colour = "darkblue")+
  annotate("text", label = "S16", x = 16, y = 0.05, size = 3, colour = "darkblue")+
  annotate("text", label = "S19", x = 19, y = 0.0, size = 3, colour = "darkblue")+
  annotate("text", label = "S22", x = 22, y = 1, size = 3, colour = "darkblue")+
  annotate("text", label = "S25", x = 25.5, y = 0.6, size = 3, colour = "darkblue")+
  annotate("text", label = "S28", x = 28, y = 0.8, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

# Code to generate the coverage probabilities graph for H2
CPH2Int<-ggplot(data=covProbs_H2)+
  geom_point(aes(x=Scenario,y=Intercept,colour=Method))+
  geom_line(aes(x=Scenario,y=Intercept,colour=Method))+
  xlab("Scenario")+
  ylab(expression("CP "*beta[0]*" "*H[2]))+
  ylim(c(0,1))+
  annotate("text", label = "S2", x = 2, y = 0.8, size = 3, colour = "darkblue")+
  annotate("text", label = "S5", x = 5, y = 0.8, size = 3, colour = "darkblue")+
  annotate("text", label = "S8", x = 8, y = 0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S11", x = 11, y = 0.25, size = 3, colour = "darkblue")+
  annotate("text", label = "S14", x = 14, y = 0.1, size = 3, colour = "darkblue")+
  annotate("text", label = "S17", x = 17, y = 0.0, size = 3, colour = "darkblue")+
  annotate("text", label = "S20", x = 20, y = 0.9, size = 3, colour = "darkblue")+
  annotate("text", label = "S23", x = 23, y = 0.3, size = 3, colour = "darkblue")+
  annotate("text", label = "S26", x = 26, y = 0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S29", x = 29, y = 0.7, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

CPH2B1<-ggplot(data=covProbs_H2)+
  geom_point(aes(x=Scenario,y=Beta1,colour=Method))+
  geom_line(aes(x=Scenario,y=Beta1,colour=Method))+
  xlab("Scenario")+
  ylab(expression("CP "*beta[1]*" "*H[2]))+
  ylim(c(0,1))+
  annotate("text", label = "S2", x = 2, y = 0.8, size = 3, colour = "darkblue")+
  annotate("text", label = "S5", x = 5, y = 0.6, size = 3, colour = "darkblue")+
  annotate("text", label = "S8", x = 8, y = 0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S11", x = 11, y = 0.1, size = 3, colour = "darkblue")+
  annotate("text", label = "S14", x = 14, y = 0.0, size = 3, colour = "darkblue")+
  annotate("text", label = "S17", x = 17, y = 0.15, size = 3, colour = "darkblue")+
  annotate("text", label = "S20", x = 20, y = 0.9, size = 3, colour = "darkblue")+
  annotate("text", label = "S23", x = 23, y = 0.0, size = 3, colour = "darkblue")+
  annotate("text", label = "S26", x = 27, y = 0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S29", x = 29, y = 0.7, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

CPH2B2<-ggplot(data=covProbs_H2)+
  geom_point(aes(x=Scenario,y=Beta2,colour=Method))+
  geom_line(aes(x=Scenario,y=Beta2,colour=Method))+
  xlab("Scenario")+
  ylab(expression("CP "*beta[2]*" "*H[2]))+
  ylim(c(0,1))+
  annotate("text", label = "S2", x = 2, y = 0.8, size = 3, colour = "darkblue")+
  annotate("text", label = "S5", x = 5, y = 0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S8", x = 8, y = 0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S11", x = 11, y = 0.15, size = 3, colour = "darkblue")+
  annotate("text", label = "S14", x = 14, y = 0.05, size = 3, colour = "darkblue")+
  annotate("text", label = "S17", x = 17, y = 0.15, size = 3, colour = "darkblue")+
  annotate("text", label = "S20", x = 20, y = 0.9, size = 3, colour = "darkblue")+
  annotate("text", label = "S23", x = 23, y = 0.0, size = 3, colour = "darkblue")+
  annotate("text", label = "S26", x = 27, y = 0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S29", x = 29, y = 0.8, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

CPH2B3<-ggplot(data=covProbs_H2)+
  geom_point(aes(x=Scenario,y=Beta3,colour=Method))+
  geom_line(aes(x=Scenario,y=Beta3,colour=Method))+
  xlab("Scenario")+
  ylab(expression("CP "*beta[3]*" "*H[2]))+
  ylim(c(0,1))+
  annotate("text", label = "S2", x = 2, y = 0.8, size = 3, colour = "darkblue")+
  annotate("text", label = "S5", x = 5, y = 0.75, size = 3, colour = "darkblue")+
  annotate("text", label = "S8", x = 8, y = 0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S11", x = 11, y = 0.2, size = 3, colour = "darkblue")+
  annotate("text", label = "S14", x = 14, y = 0.1, size = 3, colour = "darkblue")+
  annotate("text", label = "S17", x = 17, y = 0.0, size = 3, colour = "darkblue")+
  annotate("text", label = "S20", x = 20, y = 0.9, size = 3, colour = "darkblue")+
  annotate("text", label = "S23", x = 23, y = 0.0, size = 3, colour = "darkblue")+
  annotate("text", label = "S26", x = 27, y = 0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S29", x = 29, y = 0.85, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

# Code to generate the coverage probabilities graph for H3
CPH3Int<-ggplot(data=covProbs_H3)+
  geom_point(aes(x=Scenario,y=Intercept,colour=Method))+
  geom_line(aes(x=Scenario,y=Intercept,colour=Method))+
  xlab("Scenario")+
  ylab(expression("CP "*beta[0]*" "*H[3]))+
  ylim(c(0,1))+
  annotate("text", label = "S3", x = 3, y = 0.8, size = 3, colour = "darkblue")+
  annotate("text", label = "S6", x = 6, y = 0.8, size = 3, colour = "darkblue")+
  annotate("text", label = "S9", x = 9, y = 0.7, size = 3, colour = "darkblue")+
  annotate("text", label = "S12", x = 12, y = 0.3, size = 3, colour = "darkblue")+
  annotate("text", label = "S15", x = 15, y = 0.13, size = 3, colour = "darkblue")+
  annotate("text", label = "S18", x = 18, y = 0.05, size = 3, colour = "darkblue")+
  annotate("text", label = "S21", x = 21, y = 0.0, size = 3, colour = "darkblue")+
  annotate("text", label = "S24", x = 24, y = 0.3, size = 3, colour = "darkblue")+
  annotate("text", label = "S27", x = 27.4, y = 0.7, size = 3, colour = "darkblue")+
  annotate("text", label = "S30", x = 30, y = 0.8, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

CPH3B1<-ggplot(data=covProbs_H3)+
  geom_point(aes(x=Scenario,y=Beta1,colour=Method))+
  geom_line(aes(x=Scenario,y=Beta1,colour=Method))+
  xlab("Scenario")+
  ylab(expression("CP "*beta[1]*" "*H[3]))+
  ylim(c(0,1))+
  annotate("text", label = "S3", x = 3, y = 0.8, size = 3, colour = "darkblue")+
  annotate("text", label = "S6", x = 6, y = 0.45, size = 3, colour = "darkblue")+
  annotate("text", label = "S9", x = 9, y = 0.6, size = 3, colour = "darkblue")+
  annotate("text", label = "S12", x = 12, y = 0.05, size = 3, colour = "darkblue")+
  annotate("text", label = "S15", x = 15, y = 0.18, size = 3, colour = "darkblue")+
  annotate("text", label = "S18", x = 18, y = 0.13, size = 3, colour = "darkblue")+
  annotate("text", label = "S21", x = 21, y = 0.13, size = 3, colour = "darkblue")+
  annotate("text", label = "S24", x = 24, y = 0.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S27", x = 27.4, y = 0.7, size = 3, colour = "darkblue")+
  annotate("text", label = "S30", x = 30, y = 0.85, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

CPH3B2<-ggplot(data=covProbs_H3)+
  geom_point(aes(x=Scenario,y=Beta2,colour=Method))+
  geom_line(aes(x=Scenario,y=Beta2,colour=Method))+
  xlab("Scenario")+
  ylab(expression("CP "*beta[2]*" "*H[3]))+
  ylim(c(0,1))+
  annotate("text", label = "S3", x = 3, y = 0.75, size = 3, colour = "darkblue")+
  annotate("text", label = "S6", x = 6, y = 0.6, size = 3, colour = "darkblue")+
  annotate("text", label = "S9", x = 8.8, y = 0.65, size = 3, colour = "darkblue")+
  annotate("text", label = "S12", x = 12, y = 0.15, size = 3, colour = "darkblue")+
  annotate("text", label = "S15", x = 15, y = 0.2, size = 3, colour = "darkblue")+
  annotate("text", label = "S18", x = 18, y = 0.15, size = 3, colour = "darkblue")+
  annotate("text", label = "S21", x = 21, y = 0.1, size = 3, colour = "darkblue")+
  annotate("text", label = "S24", x = 24, y = 0.5, size = 3, colour = "darkblue")+
  annotate("text", label = "S27", x = 27.4, y = 0.7, size = 3, colour = "darkblue")+
  annotate("text", label = "S30", x = 30, y = 0.85, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

CPH3B3<-ggplot(data=covProbs_H3)+
  geom_point(aes(x=Scenario,y=Beta3,colour=Method))+
  geom_line(aes(x=Scenario,y=Beta3,colour=Method))+
  xlab("Scenario")+
  ylab(expression("CP "*beta[3]*" "*H[3]))+
  ylim(c(0,1))+
  annotate("text", label = "S3", x = 3, y = 0.8, size = 3, colour = "darkblue")+
  annotate("text", label = "S6", x = 6, y = 0.7, size = 3, colour = "darkblue")+
  annotate("text", label = "S9", x = 9, y = 0.8, size = 3, colour = "darkblue")+
  annotate("text", label = "S12", x = 12, y = 0.4, size = 3, colour = "darkblue")+
  annotate("text", label = "S15", x = 15, y = 0.15, size = 3, colour = "darkblue")+
  annotate("text", label = "S18", x = 18, y = 0.1, size = 3, colour = "darkblue")+
  annotate("text", label = "S21", x = 21, y = 0.05, size = 3, colour = "darkblue")+
  annotate("text", label = "S24", x = 24, y = 0.3, size = 3, colour = "darkblue")+
  annotate("text", label = "S27", x = 27.4, y = 0.7, size = 3, colour = "darkblue")+
  annotate("text", label = "S30", x = 30, y = 0.8, size = 3, colour = "darkblue")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "bottom")

# Export te gridplot for coverage probabilities in different hypotheses (Figure 5)
CPH_graph<-ggarrange(CPH1Int, CPH2Int, CPH3Int, CPH1B1, CPH2B1, CPH3B1,
                      CPH1B2,CPH2B2, CPH3B2, CPH1B3, CPH2B3, CPH3B3,
                      ncol = 3, nrow = 4)

pdf("CPH_graph.pdf",height=14,width=12)
CPH_graph
dev.off()

# Separate plots for bias for each paraemter
CPH_graph_ints<-ggarrange(CPH1Int, CPH2Int, CPH3Int, ncol = 3, nrow = 1)
CPH_graph_b1<-ggarrange(CPH1B1, CPH2B1, CPH3B1, ncol = 3, nrow = 1)
CPH_graph_b2<-ggarrange(CPH1B2,CPH2B2, CPH3B2, ncol = 3, nrow = 1)
CPH_graph_b3<-ggarrange(CPH1B3, CPH2B3, CPH3B3, ncol = 3, nrow = 1)

pdf("CPH_graph_ints.pdf",height=4,width=13)
CPH_graph_ints
dev.off()

pdf("CPH_graph_b1.pdf",height=4,width=13)
CPH_graph_b1
dev.off()

pdf("CPH_graph_b2.pdf",height=4,width=13)
CPH_graph_b2
dev.off()

pdf("CPH_graph_b3.pdf",height=4,width=13)
CPH_graph_b3
dev.off()
