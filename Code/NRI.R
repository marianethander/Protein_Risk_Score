###This function calculates continous NRI according to the method described by Pencina et al in "Evaluating the added predictive ability of a new marker: From area under the ROC curve to reclassification and beyond" and "Extensions of net reclassification improvement calculations to measure usefulness of new biomarkers".

#Maria Nethander
#2014-03-26


NRI<-function(basemodel,riskmodel,events,nonevents){
#basemodel and riskmodel are the output from a glm()
#events and nonevents are vectors with the row numbers of events/nonevents in the data frame used in the glm for the base and risk models

predbase = predRisk(basemodel)
predrisk = predRisk(riskmodel)

p_up_ev<-length(which((predbase[events]-predrisk[events])<0))/length(events)
p_down_ev<-length(which((predbase[events]-predrisk[events])>0))/length(events)
p_up_nonev<-length(which((predbase[-events]-predrisk[-events])<0))/length(nonevents)
p_down_nonev<-length(which((predbase[-events]-predrisk[-events])>0))/length(nonevents)

NRI_ev<-p_up_ev-p_down_ev
NRI_nonev<-p_down_nonev-p_up_nonev
NRI<-NRI_ev+NRI_nonev

SE_ev<-sqrt((p_up_ev+p_down_ev)/length(events) - (p_up_ev-p_down_ev)^2/length(events))
SE_nonev<-sqrt((p_up_nonev+p_down_nonev)/length(nonevents) - (p_down_nonev-p_up_nonev)^2/length(nonevents))
SE<-sqrt(SE_ev^2+SE_nonev^2)

z_ev<-NRI_ev/SE_ev
z_nonev<-NRI_nonev/SE_nonev
z<-NRI/SE

p_ev<- 2 * (1 - pnorm(abs(z_ev)))
p_nonev<- 2 * (1 - pnorm(abs(z_nonev)))
p<- 2 * (1 - pnorm(abs(z)))

result<-list(NRI=NRI, SE=SE, Z=z, p=p, NRI_event=NRI_ev, SE_event=SE_ev, Z_event=z_ev, p_event=p_ev, NRI_nonevent=NRI_nonev, SE_nonevent=SE_nonev, Z_nonevent=z_nonev, p_nonevent=p_nonev)
return(result)
}
