###Survival Analysis
library(dplyr)
library(survival)
library(reshape2)
library(ggplot2)


TZsurdat <- (patient %>% 
  mutate(e_diff= eligible_delay + 1
    ,arv_diff = arv_status_delay + 1
    ,lastdate = ifelse(followUp<endDate,followUp,endDate)
    ,diff = ifelse(
      !is.na(arv_diff),
      arv_diff,
      lastdate - as.numeric(minDate))
  )
)
## We want avoid using survival objects for now until we actually need the analysis (coxs ph)
## For now, only use it for collaping data and extracting info back to dataframe
artsur <- survfit(Surv(diff, !is.na(arv_diff)) ~ 1 ,data=TZsurdat)
artsur

print(sum(artsur$n.event)/artsur$n)

art <- data.frame(
  time=artsur$time, 
  n = artsur$n, 
  artcount= artsur$n.event
)

art <- art %>% mutate(prob = artcount/n,
                      cumprob = cumsum(prob),
                      followUp = time/year)

### This is the cumulative probability plot of Enrolling in ART out of the population  
ggplot(art, aes(followUp,cumprob)) + geom_line() + theme_bw() + ggtitle('Cumulative Probability of Enrolling in ART (POPULATION)')


### Linked and Alive... This inlcudes enrolling in ART OR not ART but not LTFU
### Because if we just do LTFU, we have ART treated patients that are not coming in anymore 
linkedsur <- survfit(Surv(templast - startdate, (templast >= endDate) | !is.na(arv_date))~1,data=TZsurdat)
linkedsur

print(sum(linkedsur$n.event)/linkedsur$n)

linked <- data.frame(time=linkedsur$time, n= linkedsur$n, linkedcount= linkedsur$n.event)
linked <- linked %>% mutate(prob = linkedcount/n,
                      cumprob = cumsum(prob),
                      followUp = time/year)

ggplot(linked, aes(followUp,cumprob)) + geom_line() + theme_bw() + ggtitle('Cumulative Probability of Linked (POPULATION)')


###Now looking at year of ART enrolment

artyearsur <- update(artsur,.~start_year)
artyearsum <- summary(artyearsur)

artyear <- data.frame(time=artyearsum$time,
                      Year= artyearsum$strata,
                      yearcount = artyearsum$n.event,
                      n = sum(artyearsur$n),
                      nart = sum(artyearsur$n.event))

artyear <- artyear %>% group_by(Year) %>%  mutate(probpop = yearcount/n,
                                                  probart = yearcount/nart,
                              cumprobpop = cumsum(probpop),
                              cumprobart = cumsum(probart),
                              followUp = time/year)

ggplot(artyear, aes(followUp, cumprobpop, group=Year, colour=Year )) + geom_line() + theme_bw()+
  ggtitle('Cumulative Probability of ART Start by Enrolment Year (POPULATION)') + xlab("Follow-up in Years") + ylab("Probability")

ggplot(artyear, aes(followUp, cumprobart, group=Year, colour=Year )) + geom_line() + theme_bw()+
  ggtitle('Cumulative Probability of ART Start by Enrolment Year (ART)') + xlab("Follow-up in Years") + ylab("Probability")


linkedyearsur <- update(linkedsur,.~start_year)
linkedyearsum <- summary(linkedyearsur)

linkedyear <- data.frame(time=linkedyearsum$time,
                      Year= linkedyearsum$strata,
                      yearcount = linkedyearsum$n.event,
                      n = sum(linkedyearsur$n),
                      nlinked = sum(linkedyearsur$n.event))

linkedyear <- linkedyear %>% group_by(Year) %>%  mutate(probpop = yearcount/n,
                                                  problinked = yearcount/nlinked,
                                                  cumprobpop = cumsum(probpop),
                                                  cumproblinked = cumsum(problinked),
                                                  followUp = time/year)

ggplot(linkedyear, aes(followUp, cumprobpop, group=Year, colour=Year )) + geom_line() + theme_bw()+
  ggtitle('Cumulative Probability of Linked by Enrolment Year (POPULATION)') + xlab("Follow-up in Years") + ylab("Probability")

ggplot(linkedyear, aes(followUp, cumproblinked, group=Year, colour=Year )) + geom_line() + theme_bw()+
  ggtitle('Cumulative Probability of Linked by Enrolment Year (Linked)') + xlab("Follow-up in Years") + ylab("Probability")

# 
# testing <- merge(art,artyear, by="time")
# head(testing)
# testing <- testing %>% group_by(year) %>% 
#   mutate(ratio = event.per.year/event,
#          cumratio = cumsum(event.per.year)/cumsum(event))
# 
# head(testing)
# 
# ggplot(testing, aes(time, cumratio, group=year,colour=year)) + geom_line() + theme_bw()
# ##Linked and Alive
# 
# SurvEnrol <- survfit(Surv(templast - as.numeric(startdate), templast >= endDate) ~ 1,data=TZsurdat)
# plot(SurvEnrol, conf.int=FALSE, xlab = "Time", ylab = "Survival Probability", main = "Linked and Alive")
# 
# artyearmod
