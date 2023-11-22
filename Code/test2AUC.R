#Maria Nethander 2014-03-26
#MN added confidence intervals for AUC. 2019-01-30
#MN added possibility to use one sided tests. 2019-03-19

library("ROCR")
library(foreign)
library(pROC)
library(PredictABEL)
library(Hmisc)

test2AUC<-function(basemodel,riskmodel,data, response,doPlot=F, filename="ROCplot",alternative="two.sided"){

#Basemodel
predbase=predict(basemodel, newdata=data, type="response") # i newdata anger man var den ska leta efter x-variablerna som 
predbase2=prediction(predbase, response)
perfbase=performance(predbase2, "tpr", "fpr")

#Riskmodel
predrisk=predict(riskmodel, newdata=data, type="response")
predrisk2=prediction(predrisk, response)
perfrisk=performance(predrisk2, "tpr", "fpr")

#Statistical test
rocbase=roc(response, predbase,ci=T)
rocrisk=roc(response, predrisk,ci=T)
test<-roc.test(rocbase, rocrisk, reuse.auc=T,alternative="two.sided")

#Results
aucbase<-test$estimate[1]
aucrisk<-test$estimate[2]
cibase<-rocbase$ci
cirisk<-rocrisk$ci
Z<-test$statistic
p<-test$p.value

result<-list(aucbase,cibase,aucrisk,cirisk,Z,p)
## result<-list(AUCbasemodel=aucbase,AUC95CIbasemodel=cibase,AUCriskmodel=aucrisk,AUC95CIriskmodel=cirisk,Z=Z,p=p)
## result<-c(aucbase,cibase,aucrisk,cirisk,Z,p)
## names(result)<-c("AUC for basemodel","AUC 95CI for basemodel","AUC for riskmodel","AUC 95CI for riskmodel","Z","p")

#Plot
if(doPlot){
pdf(paste(filename,".pdf",sep=""))
plot(perfbase)
plot(perfrisk, avg="vertical", lwd=3, col="red", spread.estimate="stderror",plotCI.lwd=2,add=TRUE, main="Hip")
abline(coef=c(0,1), lwd=3)
plot(perfbase, col="blue", lwd=3, add=T )
dev.off()
}#endif

return(result)
}
