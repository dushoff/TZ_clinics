###Survivial Analysis

library(survival)

TZsurdat <- (Datetable %>% mutate(startdate = firstVisit - 1,
                                  hack1 = firstVisit -1, 
                                 hack20 = firstVisit - 20)
)

fit <- coxph(Surv(as.numeric(arv_date)-as.numeric(hack1),!is.na(arv_date))~1,data=TZsurdat)
fit2 <- coxph(Surv(as.numeric(arv_date)-as.numeric(hack20), !is.na(arv_date))~1, data=TZsurdat)

summary(fit)
summary(fit2)

##good ! it doesn't matter how many days we hack

##first do table 3 from Joseph's docx

ARTeligi <- function(cat,logic){
  data.frame(Factor = cat,
             "<2" = nrow(subset(TZsurdat,logic & TZsurdat$age < 1)),
             "2-5" = nrow(subset(TZsurdat,logic & TZsurdat$age >= 2 & TZsurdat$age <= 5)),
             "6-9" = nrow(subset(TZsurdat,logic & TZsurdat$age >= 6 & TZsurdat$age <= 9)),
             "10-14" = nrow(subset(TZsurdat,logic & TZsurdat$age >= 10 & TZsurdat$age <= 14)),
             Total = nrow(subset(TZsurdat,logic)))
  
}

time_ARTenrolment <- (rbind(
  ARTeligi("Enrolment", !is.na(TZsurdat$arv_date))
  ,ARTeligi("Within 1 month",(as.numeric(TZsurdat$arv_date)- as.numeric(TZsurdat$startdate)) <= 30)
  ,ARTeligi("Within 2 month",(as.numeric(TZsurdat$arv_date)- as.numeric(TZsurdat$startdate)) <= 60)
  ,ARTeligi("Within 3 month",(as.numeric(TZsurdat$arv_date)- as.numeric(TZsurdat$startdate)) <= 90)
  ,ARTeligi("Within 6 month",(as.numeric(TZsurdat$arv_date)- as.numeric(TZsurdat$startdate)) <= 180)
  ,ARTeligi("Within 12 month",(as.numeric(TZsurdat$arv_date)- as.numeric(TZsurdat$startdate)) <= 365)
  ,ARTeligi("> 12 month",(as.numeric(TZsurdat$arv_date)- as.numeric(TZsurdat$startdate)) > 365)
  ,ARTeligi("Not yet started", is.na(TZsurdat$arv_date))
))
time_ARTenrolment

SurARV <- survfit(Surv(as.numeric(arv_date)-as.numeric(startdate), !is.na(arv_date))~1, data=TZsurdat)

SurCD4 <- survfit(Surv(as.numeric(cd4_date)-as.numeric(startdate), !is.na(cd4_date))~1, data=TZsurdat)

SurEligible <- survfit(Surv(as.numeric(eligible_date)-as.numeric(startdate), !is.na(eligible_date))~1, data=TZsurdat)


plot(SurARV,col=1, xlab = "Time", ylab="Survival Probability",lty=1)
lines(SurCD4,col=2,lty=1)
lines(SurEligible,col=3,lty=1)
legend('topright',c("ARV","CD4","Eligibility"),col=c("black","red","green"),lty=1)


SurARV_sex<-update(SurARV,.~sex)
plot(SurARV_sex, xlab = "Time", ylab = "Survival Probability", lty=1,col=1:2,main="ARV Treatment")
legend('topright',c("Male","Female"),col=c("black","red"),lty=1)

SurEligible_sex <- update(SurEligible,.~sex)
plot(SurEligible_sex, xlab = "Time", ylab = "Survival Probability", lty=1:2, main="Eligiblility")
legend('topright',c("Male","Female"),col=c("black","red"),lty=1)

SurARV_whostage <- update(SurARV, . ~ base_whostage)
plot(SurARV_whostage , col=c(1:4),xlab = "Time", ylab = "Survival Probability", main="WHO Stage")
legend('topright',c("1","2","3","4"),col=c("black","red","green","blue"),lty=1)


mod <- coxph(Surv(as.numeric(arv_date)-as.numeric(hack1),!is.na(arv_date))~age + start_year,data=TZsurdat)
summary(mod)

