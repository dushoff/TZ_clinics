library(survival)
library(ggplot2)
library(dplyr)

## helper functions ----
## Modularize this, or get rid of it, or whatever
censoringDAT <- function(x){
  tempdf <- with(x, data.frame(
    time = time,
    events = n.event,
    nrisk = n.risk,
    nrisk2 = nrow(survTable)+first(n.event[n.event>0])-cumsum(n.event)
  ))
  
  cendf <- tempdf %>% transmute(
    time = time/year,
    surv = cumprod((nrisk-events)/nrisk),
    cumprob = 1-surv,
    censoring = "Yes"
  )
  
  uncendf <- tempdf %>% transmute(
    time = time/year,
    surv = cumprod((nrisk2-events)/nrisk2),
    cumprob = 1-surv,
    censoring = "No"
  )
  return(rbind(cendf,uncendf))
}

pairwise_comp <- function(x,e1,e2){
  e1df <- with(x, data.frame(
    time = time,
    prevalence = prev[,1],
    event = e1
  ))
  
  e2df <- with(x,data.frame(
    time = time,
    prevalence = prev[,2],
    event = e2
  ))
  r, or whatevereturn(rbind(e1df,e2df))
}

strataDAT <- function(x){
  tempdat <- with(summary(x),data.frame(
    time = time,
    nrisk = n.risk,
    events = n.event,
    strata = strata
  ))
  
  tempdf <- (tempdat 
             %>% group_by(strata)
             %>% mutate(
               surv = cumprod((nrisk-events)/nrisk),
               cumprob = 1 - surv,
               followUp = time/year
             )
  )
  return(tempdf)
}

### Linked----

Linked <- survfit(
  Surv(followTime, LTFU_status) ~ 1
  , data=survTable
)

LinkedDF <- censoringDAT(Linked)

print (ggplot(LinkedDF, aes(time,surv,colour=censoring))
	+ geom_line() 
	+ ggtitle("Proportion linked")
	+ ylab("Probability")
	+ theme_bw()
)

# Eligible(Yes or No) ----

Eligible <- survfit(
  Surv(eligibleTime, eligible_ever) ~ 1
  , data=survTable
)

EligibleDF <- censoringDAT(Eligible)

print(ggplot(EligibleDF, aes(time,cumprob,colour=censoring))
      + geom_line() 
      + ggtitle("Eligibility Through Time")
      + ylab("Probability")
      + theme_bw()
)

## ART (Yes or No) ----

ART <- survfit(
  Surv(arvFollowTime, arv_ever) ~ 1
  , data=survTable
)

ARTDF <- censoringDAT(ART)

print(ggplot(ARTDF, aes(time,cumprob,colour=censoring))
           + geom_line() 
           + ggtitle("ART Through Time (")
           + ylab("Probability")
           + xlab("Time in Years")
           + theme_bw()
)

## Linked by Gender ----
LinkedSex <- update(Linked,.~sex_first)

LinkedSexDF <- strataDAT(LinkedSex)

print(ggplot(LinkedSexDF, aes(followUp,surv,colour=strata))
          + geom_line() 
          + ggtitle("LTFU Through Time by gender")
          + ylab("Probability")
          + xlab("Year Lag")
          + theme_bw()
)

## Linked by Enrollment Year

LinkedYear <- update(Linked,.~enrolYear)

LinkedYearDF <- strataDAT(LinkedYear)

print(ggplot(LinkedYearDF, aes(followUp,surv,colour=strata))
      + geom_line() 
      + ggtitle("LTFU Through Time by Enrollment Year")
      + ylab("Probability")
      + xlab("Year Lag")
      + theme_bw()
)

## Linked by Agecat 1 ----

LinkedAgecatA <- update(Linked,.~agecatA)

LinkedAgecatA <- strataDAT(LinkedAgecatA)

print(ggplot(LinkedAgecatA, aes(followUp,surv,colour=strata))
      + geom_line() 
      + ggtitle("LTFU Through Time by AgecatA")
      + ylab("Probability")
      + xlab("Year Lag")
      + theme_bw()
)

## Linked by Agecat 2 ----

LinkedAgecatB <- update(Linked,.~agecatB)

LinkedAgecatB <- strataDAT(LinkedAgecatB)

print(ggplot(LinkedAgecatB, aes(followUp,surv,colour=strata))
      + geom_line() 
      + ggtitle("LTFU Through Time by AgecatB")
      + ylab("Probability")
      + xlab("Year Lag")
      + theme_bw()
)

## Linked by Health Facility ----

LinkedHF <- update(Linked,.~hf_type_first)

LinkedHF <- strataDAT(LinkedHF)

print(ggplot(LinkedHF, aes(followUp,surv,colour=strata))
      + geom_line() 
      + ggtitle("LTFU Through Time by HF")
      + ylab("Probability")
      + xlab("Year Lag")
      + theme_bw()
)

## ART by Gender ----
ARTSex <- update(ART,.~sex_first)

ARTSexDF <- strataDAT(ARTSex)

print(ggplot(ARTSexDF, aes(followUp,cumprob,colour=strata))
             + geom_line() 
             + ggtitle("ART Through Time by gender")
             + ylab("Probability")
             + xlab("Year Lag")
             + theme_bw()
)

## ART by Enrolment year ----
ARTYear <- update(ART,.~enrolYear)

ARTYearDF <- strataDAT(ARTYear)

print(ggplot(ARTYearDF, aes(followUp,cumprob,colour=strata))
              + geom_line() 
              + ggtitle("ART Through Time by Enrolment Year")
              + ylab("Probability")
              + xlab("Year Lag")
              + theme_bw()
)

## ART by Agecat 1 ----

ARTAgecatA <- update(ART,.~agecatA)

ARTAgecatA <- strataDAT(ARTAgecatA)

print(ggplot(ARTAgecatA, aes(followUp,cumprob,colour=strata))
      + geom_line() 
      + ggtitle("ART by AgecatA")
      + ylab("Probability")
      + xlab("Year Lag")
      + theme_bw()
)

## ART by Agecat 2 ----

ARTAgecatB <- update(ART,.~agecatB)

ARTAgecatB <- strataDAT(ARTAgecatB)

print(ggplot(ARTAgecatB, aes(followUp,cumprob,colour=strata))
      + geom_line() 
      + ggtitle("ART by AgecatB")
      + ylab("Probability")
      + xlab("Year Lag")
      + theme_bw()
)


## ART by Health Facility ----

ARTHF <- update(ART,.~hf_type_first)

ARTHF <- strataDAT(ARTHF)

print(ggplot(ARTHF, aes(followUp,cumprob,colour=strata))
      + geom_line() 
      + ggtitle("ART by HF")
      + ylab("Probability")
      + xlab("Year Lag")
      + theme_bw()
)

## Pairwise competing risk ----

DL_status <- with(survTable,ifelse(death_ever,2*as.numeric(death_ever),as.numeric(LTFU_status)))
DL_delay <- with(survTable, ifelse(death_ever,death_delay,followTime))
DL_status <- factor(DL_status,0:2,labels=c("censor","LTFU","death"))

DLcomp <- survfit(Surv(DL_delay,DL_status)~1,data=survTable)
DLcomp

DLdf <- pairwise_comp(summary(DLcomp),"LTFU","Dead")

print(ggplot(DLdf, aes(time,prevalence,colour=event))
      + geom_line() 
      + ggtitle("Competing risk DL Through Time")
      + ylab("Prevalence")
      + xlab("Day Lag")
      + theme_bw()
)

DA_status <- with(survTable,ifelse(death_ever,2*as.numeric(death_ever),as.numeric(arv_ever)))
DA_delay <- with(survTable,ifelse(death_ever,death_delay,arvFollowTime))
DA_status <- factor(DA_status,0:2,labels=c("censor","ART","death"))

DAcomp <- survfit(Surv(DA_delay,DA_status)~1,data=survTable)
## The numbers are not adding up FIXME
## testing <- survTable %>% select(c(arv_ever,arv_status_delay,arv_diff))

DAdf <- pairwise_comp(summary(DAcomp),"ART","Dead")

print(ggplot(DAdf, aes(time,prevalence,colour=event))
      + geom_line() 
      + ggtitle("Competing risk DA Through Time")
      + ylab("Prevalence")
      + xlab("Day Lag")
      + theme_bw()
)


AD_status <- with(survTable,ifelse(arv_ever,2*as.numeric(arv_ever),as.numeric(death_ever)))
AD_delay <- with(survTable,ifelse(arv_ever,arvFollowTime,death_delay))
AD_status <- factor(AD_status,0:2,labels=c("censor","death","ART"))

ADcomp <- survfit(Surv(AD_delay,AD_status)~1,data=survTable)
ADdf <- pairwise_comp(summary(ADcomp),"Dead","ART")

print(ggplot(ADdf, aes(time,prevalence,colour=event))
      + geom_line() 
      + ggtitle("Competing risk AD Through Time")
      + ylab("Prevalence")
      + xlab("Day Lag")
      + theme_bw()
)

AL_status <- with(survTable,ifelse(arv_ever,2*as.numeric(arv_ever),as.numeric(LTFU_status)))
AL_delay <- with(survTable, ifelse(arv_ever,arvFollowTime,followTime))
AL_status <- factor(AL_status,0:2,labels=c("censor","LTFU","ART"))

ALcomp <- survfit(Surv(AL_delay,AL_status)~1,data=survTable)
ALcomp

ALdf <- pairwise_comp(summary(ALcomp),"LTFU","ART")

print(ggplot(ALdf, aes(time,prevalence,colour=event))
      + geom_line() 
      + ggtitle("Competing risk AL Through Time")
      + ylab("Prevalence")
      + xlab("Day Lag")
      + theme_bw()
)

LA_status <- with(survTable,ifelse(LTFU_status,2*as.numeric(LTFU_status),as.numeric(arv_ever)))
LA_delay <- with(survTable, ifelse(LTFU_status,followTime,arvFollowTime))
LA_status <- factor(LA_status,0:2,labels=c("censor","ART","LTFU"))

LAcomp <- survfit(Surv(LA_delay,LA_status)~1,data=survTable)
LAcomp

LAdf <- pairwise_comp(summary(LAcomp),"LTFU","ART")

print(ggplot(LAdf, aes(time,prevalence,colour=event))
      + geom_line() 
      + ggtitle("Competing risk LA Through Time")
      + ylab("Prevalence")
      + xlab("Day Lag")
      + theme_bw()
)
