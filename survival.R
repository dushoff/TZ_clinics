###Survivial Analysis

library(survival)

zerohack <- (Datetable %>% mutate(startdate = firstVisit - 1,
                                  hack1 = firstVisit -1, 
                                 hack20 = firstVisit - 20)
)

Surhack1 <- (survfit(Surv(as.numeric(arv_date)-as.numeric(hack1),!is.na(arv_date))~1,data=zerohack))

Surhack20 <- (survfit(Surv(as.numeric(arv_date)-as.numeric(hack20),!is.na(arv_date))~1,data=zerohack))


plot(Surhack1, col=1)
lines(Surhack20, col=2)

##?!? plots don't overlap

fit <- coxph(Surv(as.numeric(arv_date)-as.numeric(hack1),!is.na(arv_date))~1,data=zerohack)
fit2 <- coxph(Surv(as.numeric(arv_date)-as.numeric(hack20), !is.na(arv_date))~1, data=zerohack)

plot(survfit(fit))
lines(SurARV,col=2)
lines(survfit(fit2),col=4)


##?!? still don't overlap??

##first do table 3 from Joseph's docx

ARTeligi <- function(cat,logic){
  data.frame(Factor = cat,
             "<2" = nrow(subset(zerohack,logic & zerohack$age < 1)),
             "2-5" = nrow(subset(zerohack,logic & zerohack$age >= 2 & zerohack$age <= 5)),
             "6-9" = nrow(subset(zerohack,logic & zerohack$age >= 6 & zerohack$age <= 9)),
             "10-14" = nrow(subset(zerohack,logic & zerohack$age >= 10 & zerohack$age <= 14)),
             Total = nrow(subset(zerohack,logic)))
  
}

time_ARTenrolment <- (rbind(
  ARTeligi("Enrolment", !is.na(zerohack$arv_date))
  ,ARTeligi("Within 1 month",(as.numeric(zerohack$arv_date)- as.numeric(zerohack$startdate)) <= 30)
  ,ARTeligi("Within 2 month",(as.numeric(zerohack$arv_date)- as.numeric(zerohack$startdate)) <= 60)
  ,ARTeligi("Within 3 month",(as.numeric(zerohack$arv_date)- as.numeric(zerohack$startdate)) <= 90)
  ,ARTeligi("Within 6 month",(as.numeric(zerohack$arv_date)- as.numeric(zerohack$startdate)) <= 180)
  ,ARTeligi("Within 12 month",(as.numeric(zerohack$arv_date)- as.numeric(zerohack$startdate)) <= 365)
  ,ARTeligi("> 12 month",(as.numeric(zerohack$arv_date)- as.numeric(zerohack$startdate)) > 365)
  ,ARTeligi("Not yet started", is.na(zerohack$arv_date))
))
time_ARTenrolment

SurARV <- Surhack1

SurCD4 <- survfit(Surv(as.numeric(cd4_date)-as.numeric(startdate), !is.na(cd4_date))~1, data=zerohack)

SurEligible <- survfit(Surv(as.numeric(eligible_date)-as.numeric(startdate), !is.na(eligible_date))~1, data=zerohack)


plot(SurARV,col=1, xlab = "Time", ylab="Survival Probability",lty=1)
lines(SurCD4,col=2,lty=1)
lines(SurEligible,col=3,lty=1)
legend('topright',c("ARV","CD4","Eligibility"),col=c("black","red","green"),lty=1)


SurARV_sex<-update(SurARV,.~sex)
plot(SurARV_sex, xlab = "Time", ylab = "Survival Probability", lty=1)

SurEligible_sex <- update(SurEligible,.~sex)
plot(SurEligible_sex, xlab = "Time", ylab = "Survival Probability", lty=1)

SurARV_whostage <- update(SurARV, . ~ base_whostage)
plot(SurARV_whostage)


