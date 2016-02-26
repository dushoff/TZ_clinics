library("ggplot2"); theme_set(theme_bw())
scale_colour_discrete <- function(...,palette="Set1")
  scale_colour_brewer(...,palette=palette)
scale_fill_discrete <- function(...,palette="Set1")
  scale_fill_brewer(...,palette=palette)
zmargin <- theme(panel.margin=grid::unit(0,"lines"))
library(survival)
library(dplyr)
library(GGally)

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

### PLA----

PLA <- survfit(
  Surv(LA_delay, LA_status)~ 1
  , data = survTable)

print(PLA_plot<- ggsurv(PLA,plot.cens=FALSE)
      + geom_line() 
      + ggtitle("Proportion Linked and Alive")
      + ylab("Proportion")
      + xlab("Year Lag")
      + scale_x_continuous(limits = c(0,4.25))
      + scale_y_continuous(limits = c(0,1))
)

## ART (Yes or No) ----

ART <- survfit(
  Surv(arv_delay, arv_ever) ~ 1
  , data=survTable
)

ARTDF <- survDF(ART)

print(ART_plot<- ggplot(ARTDF, aes(time,cumprob))
      + geom_line() 
      + ggtitle("Proportion Of getting onto ART")
      + ylab("Proportion")
      + xlab("Year Lag")
      + scale_x_continuous(limits = c(0,4.25))
      + scale_y_continuous(limits = c(0,1))
)

## PLA by Gender ----
PLASex <- update(PLA,.~sex_first)

print(PLASex_plot<- ggsurv(PLASex,plot.cens=FALSE)
          + ggtitle("Linked and Alive by Sex")
          + ylab("Proportion")
          + xlab("Year Lag")
          + scale_x_continuous(limits = c(0,4.25))
          + scale_y_continuous(limits = c(0,1))
          
)

## PLA by Enrollment Year

PLAYear <- update(PLA,.~enrolYear)

print(PLAYear_plot<-ggsurv(PLAYear,plot.cens=FALSE)
      + ggtitle("Linked and Alive by Enrollment Year")
      + ylab("Proportion")
      + xlab("Year Lag")
      + scale_x_continuous(limits = c(0,4.25))
      + scale_y_continuous(limits = c(0,1))
      
)

## PLA by Agecat 1 ----

PLAAgecatA <- update(PLA,.~agecatA)

print(PLAAgecatA_plot<-ggsurv(PLAAgecatA, plot.cens=FALSE)
      + ggtitle("Linked and Alive by AgecatA")
      + ylab("Proportion")
      + xlab("Year Lag")
      + scale_x_continuous(limits = c(0,4.25))
      + scale_y_continuous(limits = c(0,1))
      
      
)

## PLA by Agecat 2 ----

PLAAgecatB <- update(PLA,.~agecatB)

print(PLAAgecatB_plot<-ggsurv(PLAAgecatB, plot.cens=FALSE)
      + ggtitle("Linked and Alive by AgecatB")
      + ylab("Proportion")
      + xlab("Year Lag")
      + scale_x_continuous(limits = c(0,4.25))
      + scale_y_continuous(limits = c(0,1))
      
      
)

## PLA by Health Facility ----

PLAHF <- update(PLA,.~hf_type_first)

print(PLAHF_plot<-ggsurv(PLAHF, plot.cens=FALSE)
      + ggtitle("Linked and Alive by HF")
      + ylab("Proportion")
      + xlab("Year Lag")
      + scale_x_continuous(limits = c(0,4.25))
      + scale_y_continuous(limits = c(0,1))
      
      
)

## ART by Gender ----
ARTSex <- update(ART,.~sex_first)

ARTSexDF <- survDF(ARTSex)

print(ARTSex_plot<-ggplot(ARTSexDF, aes(time,cumprob,colour=strata))
             + geom_line() 
             + ggtitle("ART by gender")
             + ylab("Proportion")
             + xlab("Year Lag")
             + scale_x_continuous(limits = c(0,4.25))
             + scale_y_continuous(limits = c(0,1))
      
             
)

## ART by Enrolment year ----
ARTYear <- update(ART,.~enrolYear)

ARTYearDF <- survDF(ARTYear)

print(ARTYear_plot<-ggplot(ARTYearDF, aes(time,cumprob,colour=strata))
              + geom_line() 
              + ggtitle("ART by Enrolment Year")
              + ylab("Proportion")
              + xlab("Year Lag")
              
)

## ART by Agecat 1 ----

ARTAgecatA <- update(ART,.~agecatA)

ARTAgecatA <- survDF(ARTAgecatA)

print(ARTAgecatA_plot<-ggplot(ARTAgecatA, aes(time,cumprob,colour=strata))
      + geom_line() 
      + ggtitle("ART by AgecatA")
      + ylab("Proportion")
      + xlab("Year Lag")
      + scale_x_continuous(limits = c(0,4.25))
      + scale_y_continuous(limits = c(0,1))
      
      
)

## ART by Agecat 2 ----

ARTAgecatB <- update(ART,.~agecatB)

ARTAgecatB <- survDF(ARTAgecatB)

print(ARTAgecatB_plot<-ggplot(ARTAgecatB, aes(time,cumprob,colour=strata))
      + geom_line() 
      + ggtitle("ART by AgecatB")
      + ylab("Proportion")
      + xlab("Year Lag")
      + scale_x_continuous(limits = c(0,4.25))
      + scale_y_continuous(limits = c(0,1))
      
      
)


## ART by Health Facility ----

ARTHF <- update(ART,.~hf_type_first)

ARTHF <- survDF(ARTHF)

print(ARTHF_plot<-ggplot(ARTHF, aes(time,cumprob,colour=strata))
      + geom_line() 
      + ggtitle("ART by HF")
      + ylab("Proportion")
      + xlab("Year Lag")
      + scale_x_continuous(limits = c(0,4.25))
      + scale_y_continuous(limits = c(0,1))
      
      
)
