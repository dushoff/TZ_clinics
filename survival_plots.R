library("ggplot2"); theme_set(theme_bw())
scale_colour_discrete <- function(...,palette="Set1")
  scale_colour_brewer(...,palette=palette)
scale_fill_discrete <- function(...,palette="Set1")
  scale_fill_brewer(...,palette=palette)
zmargin <- theme(panel.margin=grid::unit(0,"lines"))
library(survival)
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
  Surv(lost_delay, lost_status)~ 1
  , data = survTable)

LinkedDF <- survDF(Linked)

print(Linked_plot<- ggplot(LinkedDF, aes(time,surv))
      + geom_line() 
      + ggtitle("Proportion Linked and Alive")
      + ylab("Proportion")
      + scale_x_continuous(limits = c(0,1500))
      + scale_y_continuous(limits = c(0,1))
)

## ART (Yes or No) ----

ART <- survfit(
  Surv(ART_delay, arv_ever) ~ 1
  , data=survTable
)

ARTDF <- survDF(ART)

print(ART_plot<- ggplot(ARTDF, aes(time,cumprob))
      + geom_line() 
      + ggtitle("Proportion Of getting onto ART")
      + ylab("Proportion")
      + scale_x_continuous(limits = c(0,1500))
      + scale_y_continuous(limits = c(0,1))
)

## Linked by Gender ----
LinkedSex <- update(Linked,.~sex_first)

LinkedSexDF <- survDF(LinkedSex)

print(LinkedSex_plot<- ggplot(LinkedSexDF, aes(time,surv,colour=strata))
          + geom_line() 
          + ggtitle("Linked and Alive by gender")
          + ylab("Proportion")
          + xlab("Day Lag")
          + scale_x_continuous(limits = c(0,1500))
          + scale_y_continuous(limits = c(0,1))
          
)

## Linked by Enrollment Year

LinkedYear <- update(Linked,.~enrolYear)

LinkedYearDF <- survDF(LinkedYear)

print(LinkedYear_plot<-ggplot(LinkedYearDF, aes(time,surv,colour=strata))
      + geom_line() 
      + ggtitle("Linked and Alive by Enrollment Year")
      + ylab("Proportion")
      + xlab("Day Lag")
      + scale_x_continuous(limits = c(0,1500))
      + scale_y_continuous(limits = c(0,1))
      
)

## Linked by Agecat 1 ----

LinkedAgecatA <- update(Linked,.~agecatA)

LinkedAgecatA <- survDF(LinkedAgecatA)

print(LinkedAgecatA_plot<-ggplot(LinkedAgecatA, aes(time,surv,colour=strata))
      + geom_line() 
      + ggtitle("Linked and Alive by AgecatA")
      + ylab("Proportion")
      + xlab("Day Lag")
      + scale_x_continuous(limits = c(0,1500))
      + scale_y_continuous(limits = c(0,1))
      
      
)

## Linked by Agecat 2 ----

LinkedAgecatB <- update(Linked,.~agecatB)

LinkedAgecatB <- survDF(LinkedAgecatB)

print(LinkedAgecatB_plot<-ggplot(LinkedAgecatB, aes(time,surv,colour=strata))
      + geom_line() 
      + ggtitle("Linked and Alive by AgecatB")
      + ylab("Proportion")
      + xlab("Day Lag")
      + scale_x_continuous(limits = c(0,1500))
      + scale_y_continuous(limits = c(0,1))
      
      
)

## Linked by Health Facility ----

LinkedHF <- update(Linked,.~hf_type_first)

LinkedHF <- survDF(LinkedHF)

print(LinkedHF_plot<-ggplot(LinkedHF, aes(time,surv,colour=strata))
      + geom_line() 
      + ggtitle("Linked and Alive by HF")
      + ylab("Proportion")
      + xlab("Day Lag")
      + scale_x_continuous(limits = c(0,1500))
      + scale_y_continuous(limits = c(0,1))
      
      
)

## ART by Gender ----
ARTSex <- update(ART,.~sex_first)

ARTSexDF <- survDF(ARTSex)

print(ARTSex_plot<-ggplot(ARTSexDF, aes(time,cumprob,colour=strata))
             + geom_line() 
             + ggtitle("ART by gender")
             + ylab("Proportion")
             + xlab("Day Lag")
             + scale_x_continuous(limits = c(0,1500))
             + scale_y_continuous(limits = c(0,1))
      
             
)

## ART by Enrolment year ----
ARTYear <- update(ART,.~enrolYear)

ARTYearDF <- survDF(ARTYear)

print(ARTYear_plot<-ggplot(ARTYearDF, aes(time,cumprob,colour=strata))
              + geom_line() 
              + ggtitle("ART by Enrolment Year")
              + ylab("Proportion")
              + xlab("Day Lag")
              
)

## ART by Agecat 1 ----

ARTAgecatA <- update(ART,.~agecatA)

ARTAgecatA <- survDF(ARTAgecatA)

print(ARTAgecatA_plot<-ggplot(ARTAgecatA, aes(time,cumprob,colour=strata))
      + geom_line() 
      + ggtitle("ART by AgecatA")
      + ylab("Proportion")
      + xlab("Day Lag")
      + scale_x_continuous(limits = c(0,1500))
      + scale_y_continuous(limits = c(0,1))
      
      
)

## ART by Agecat 2 ----

ARTAgecatB <- update(ART,.~agecatB)

ARTAgecatB <- survDF(ARTAgecatB)

print(ARTAgecatB_plot<-ggplot(ARTAgecatB, aes(time,cumprob,colour=strata))
      + geom_line() 
      + ggtitle("ART by AgecatB")
      + ylab("Proportion")
      + xlab("Day Lag")
      + scale_x_continuous(limits = c(0,1500))
      + scale_y_continuous(limits = c(0,1))
      
      
)


## ART by Health Facility ----

ARTHF <- update(ART,.~hf_type_first)

ARTHF <- survDF(ARTHF)

print(ARTHF_plot<-ggplot(ARTHF, aes(time,cumprob,colour=strata))
      + geom_line() 
      + ggtitle("ART by HF")
      + ylab("Proportion")
      + xlab("Day Lag")
      + scale_x_continuous(limits = c(0,1500))
      + scale_y_continuous(limits = c(0,1))
      
      
)
