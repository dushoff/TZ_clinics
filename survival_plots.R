library(survival)
library(ggplot2)
library(dplyr)

## helper functions ----
survDF <- function(x){
  tempdat <- with(summary(x),data.frame(
    time = time,
    surv = surv,
    events = n.event
  ))
  ifelse(!is.null(summary(x)$strata),
         tempdat$strata<-summary(x)$strata,
         tempdat$strata<-1)
  
  tempdf <- (tempdat 
             %>% group_by(strata)
             %>% mutate(
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

LinkedDF <- survDF(Linked)

print (ggplot(LinkedDF, aes(time,surv))
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

EligibleDF <- survDF(Eligible)

print(ggplot(EligibleDF, aes(time,cumprob))
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

ARTDF <- survDF(ART)

print(ggplot(ARTDF, aes(time,cumprob))
           + geom_line() 
           + ggtitle("ART Through Time (")
           + ylab("Probability")
           + xlab("Time in Years")
           + theme_bw()
)

## Linked by Gender ----
LinkedSex <- update(Linked,.~sex_first)

LinkedSexDF <- survDF(LinkedSex)

print(ggplot(LinkedSexDF, aes(followUp,surv,colour=strata))
          + geom_line() 
          + ggtitle("LTFU Through Time by gender")
          + ylab("Probability")
          + xlab("Year Lag")
          + theme_bw()
)

## Linked by Enrollment Year

LinkedYear <- update(Linked,.~enrolYear)

LinkedYearDF <- survDF(LinkedYear)

print(ggplot(LinkedYearDF, aes(followUp,surv,colour=strata))
      + geom_line() 
      + ggtitle("LTFU Through Time by Enrollment Year")
      + ylab("Probability")
      + xlab("Year Lag")
      + theme_bw()
)

## Linked by Agecat 1 ----

LinkedAgecatA <- update(Linked,.~agecatA)

LinkedAgecatA <- survDF(LinkedAgecatA)

print(ggplot(LinkedAgecatA, aes(followUp,surv,colour=strata))
      + geom_line() 
      + ggtitle("LTFU Through Time by AgecatA")
      + ylab("Probability")
      + xlab("Year Lag")
      + theme_bw()
)

## Linked by Agecat 2 ----

LinkedAgecatB <- update(Linked,.~agecatB)

LinkedAgecatB <- survDF(LinkedAgecatB)

print(ggplot(LinkedAgecatB, aes(followUp,surv,colour=strata))
      + geom_line() 
      + ggtitle("LTFU Through Time by AgecatB")
      + ylab("Probability")
      + xlab("Year Lag")
      + theme_bw()
)

## Linked by Health Facility ----

LinkedHF <- update(Linked,.~hf_type_first)

LinkedHF <- survDF(LinkedHF)

print(ggplot(LinkedHF, aes(followUp,surv,colour=strata))
      + geom_line() 
      + ggtitle("LTFU Through Time by HF")
      + ylab("Probability")
      + xlab("Year Lag")
      + theme_bw()
)

## ART by Gender ----
ARTSex <- update(ART,.~sex_first)

ARTSexDF <- survDF(ARTSex)

print(ggplot(ARTSexDF, aes(followUp,cumprob,colour=strata))
             + geom_line() 
             + ggtitle("ART Through Time by gender")
             + ylab("Probability")
             + xlab("Year Lag")
             + theme_bw()
)

## ART by Enrolment year ----
ARTYear <- update(ART,.~enrolYear)

ARTYearDF <- survDF(ARTYear)

print(ggplot(ARTYearDF, aes(followUp,cumprob,colour=strata))
              + geom_line() 
              + ggtitle("ART Through Time by Enrolment Year")
              + ylab("Probability")
              + xlab("Year Lag")
              + theme_bw()
)

## ART by Agecat 1 ----

ARTAgecatA <- update(ART,.~agecatA)

ARTAgecatA <- survDF(ARTAgecatA)

print(ggplot(ARTAgecatA, aes(followUp,cumprob,colour=strata))
      + geom_line() 
      + ggtitle("ART by AgecatA")
      + ylab("Probability")
      + xlab("Year Lag")
      + theme_bw()
)

## ART by Agecat 2 ----

ARTAgecatB <- update(ART,.~agecatB)

ARTAgecatB <- survDF(ARTAgecatB)

print(ggplot(ARTAgecatB, aes(followUp,cumprob,colour=strata))
      + geom_line() 
      + ggtitle("ART by AgecatB")
      + ylab("Probability")
      + xlab("Year Lag")
      + theme_bw()
)


## ART by Health Facility ----

ARTHF <- update(ART,.~hf_type_first)

ARTHF <- survDF(ARTHF)

print(ggplot(ARTHF, aes(followUp,cumprob,colour=strata))
      + geom_line() 
      + ggtitle("ART by HF")
      + ylab("Probability")
      + xlab("Year Lag")
      + theme_bw()
)
