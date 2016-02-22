library(survival)
library(ggplot2)
library(dplyr)

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

### Linked and Alive----

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

# Eligible for ART (Yes or No) ----

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
  Surv(arvFollowTime, arv_ever, type= "right") ~ 1
  , data=survTable
)

ARTDF <- censoringDAT(ART)

print(ggplot(ARTDF, aes(time,cumprob,colour=censoring))
           + geom_line() 
           + ggtitle("ART Through Time")
           + ylab("Probability")
           + xlab("Time in Years")
           + theme_bw()
)

## Linked by Gender ----
LinkedSex <- update(Linked,.~sex_baseline)

LinkedSexDF <- strataDAT(LinkedSex)

print(ggplot(LinkedSexDF, aes(followUp,surv,colour=strata))
          + geom_line() 
          + ggtitle("LTFU Through Time by gender")
          + ylab("Probability")
          + xlab("Year Lag")
          + theme_bw()
)


## ART by Gender ----
ARTSex <- update(ART,.~sex_baseline)

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

