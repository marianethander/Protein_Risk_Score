#Maria Nethander 2014-03-26

IDI<-function(basemodel,riskmodel,events,nonevents){
#basemodel and riskmodel are the output from a glm()
#events and nonevents are vectors with the row numbers of events/nonevents in the data frame used in the glm for the base and risk models
  
predbase = predRisk(basemodel)
predrisk = predRisk(riskmodel)

#Mean risk for base model
ev_prob_base = predbase[events]
non_ev_prob_base = predbase[-events]
IDI_event_base = mean(ev_prob_base)
IDI_non_event_base = mean(non_ev_prob_base)

#Mean risk for risk model
ev_prob_risk = predrisk[events]
non_ev_prob_risk = predrisk[-events]
IDI_event_risk = mean(ev_prob_risk)
IDI_non_event_risk = mean(non_ev_prob_risk)

#IDI
IDI_ev<-IDI_event_risk-IDI_event_base
IDI_nonev<-IDI_non_event_base-IDI_non_event_risk
IDI<-IDI_ev+IDI_nonev

#SE
SE_ev<-sd(predrisk[events] - predbase[events])/sqrt(length(events))
SE_nonev<-sd(predrisk[nonevents] - predbase[nonevents])/sqrt(length(nonevents))
SE<-sqrt(SE_ev^2+SE_nonev^2)

#Z
z_ev<-IDI_ev/SE_ev
z_nonev<-IDI_nonev/SE_nonev
z<-IDI/SE

#p
p_ev = 2 * (1 - pnorm(abs(z_ev)))
p_nonev = 2 * (1 - pnorm(abs(z_nonev)))
p = 2 * (1 - pnorm(abs(z)))


result<-list(IDI=IDI, SE=SE, Z=z, p=p, IDI_event=IDI_ev, SE_event=SE_ev, Z_event=z_ev, p_event=p_ev, IDI_nonevent=IDI_nonev, SE_nonevent=SE_nonev, Z_nonevent=z_nonev, p_nonevent=p_nonev)
return(result)
}
