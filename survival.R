###Survival Analysis
library(dplyr)
library(survival)
library(reshape2)
library(ggplot2)

TZsurdat <- (Datetable %>% mutate(startdate = firstVisit - 1))
                                  


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

SurvEnrol <- with(TZsurdat, survfit(Surv(
	as.numeric(lastVisit - startdate)
	, death == "No"
		& !is.na(LTFU_status)
		& LTFU_status == FALSE
) ~ 1))

plot(SurvEnrol, conf.int=FALSE, xlab = "Time", ylab = "Survival Probability", main = "Linked and Alive")
plot(SurvEnrol, xlim= c(500,1000),xlab = "Time", ylab = "Survival Probability", main = "Linked and Alive")
plot(SurvEnrol, xlim= c(1,10),xlab = "Time", ylab = "Survival Probability", main = "Linked and Alive")
plot(SurvEnrol, mark.time=FALSE, conf.int = FALSE,xlab = "Time", ylab = "Survival Probability", main = "Linked and Alive")

artmod <- survfit(Surv(as.numeric(TZsurdat$arv_date)-as.numeric(TZsurdat$startdate), !is.na(TZsurdat$arv_date))~1)
art <- data.frame(time= artmod$time, treated = artmod$n.event)
art <- art %>% mutate(cumprob = cumsum(treated))

print(ggplot(art,aes(x=time,y=cumprob)) +xlab("Time") + ylab("Cumulative Probability") +
  geom_line() + ggtitle("Proportion of ART") + theme_bw()
)

artagemod <- update(artmod,.~+TZsurdat$age)
artage <- data.frame(time=artagemod$time, treated = artagemod$n.event/artmod$n, age=0)

artage$age[1:cumsum(artagemod$strata)[15]] <- 14 
artage$age[1:cumsum(artagemod$strata)[14]] <- 13 
artage$age[1:cumsum(artagemod$strata)[13]] <- 12 
artage$age[1:cumsum(artagemod$strata)[12]] <- 11 
artage$age[1:cumsum(artagemod$strata)[11]] <- 10 
artage$age[1:cumsum(artagemod$strata)[10]] <- 9 
artage$age[1:cumsum(artagemod$strata)[9]] <- 8
artage$age[1:cumsum(artagemod$strata)[8]] <- 7
artage$age[1:cumsum(artagemod$strata)[7]] <- 6
artage$age[1:cumsum(artagemod$strata)[6]] <- 5
artage$age[1:cumsum(artagemod$strata)[5]] <- 4
artage$age[1:cumsum(artagemod$strata)[4]] <- 3
artage$age[1:cumsum(artagemod$strata)[3]] <- 2
artage$age[1:cumsum(artagemod$strata)[2]] <- 1
artage$age[1:cumsum(artagemod$strata)[1]] <- 0

artage <- (artage
	%>% group_by(age)
	%>% mutate(cumprob = cumsum(treated))
)

ggplot(artage,aes(x=time,y=cumprob,group=age,col=factor(age))) +xlab("Time") + ylab("Cumulative Probability") +
  geom_line() + ggtitle("Proportion of ART by age") + theme_bw()

artwhomod <- update(artmod,.~+TZsurdat$base_whostage)
artwho <- data.frame(time=artwhomod$time, treated = artwhomod$n.event/artmod$n, WHOstage=0)

artwho$WHOstage[1:cumsum(artwhomod$strata)[4]] <- 4 
artwho$WHOstage[1:cumsum(artwhomod$strata)[3]] <- 3  
artwho$WHOstage[1:cumsum(artwhomod$strata)[2]] <- 2  
artwho$WHOstage[1:cumsum(artwhomod$strata)[1]] <- 1 

artwho <- artwho %>% group_by(WHOstage) %>% mutate(cumprob = cumsum(treated))

ggplot(artwho,aes(x=time,y=cumprob,group=WHOstage,col=factor(WHOstage))) +xlab("Time") + ylab("Cumulative Probability") +
  geom_line() + ggtitle("Proportion WHO stage in ART") + theme_bw()


arthfmod <- update(artmod,.~+TZsurdat$base_facility)
arthf <- data.frame(time=arthfmod$time, treated = arthfmod$n.event/artmod$n, HF=0)
arthfmod$strata

arthf$HF[1:cumsum(arthfmod$strata)[4]] <- "Other" 
arthf$HF[1:cumsum(arthfmod$strata)[3]] <- "Hospital"  
arthf$HF[1:cumsum(arthfmod$strata)[2]] <- "Health Centre"  
arthf$HF[1:cumsum(arthfmod$strata)[1]] <- "Dispensary" 

arthf <- arthf %>% group_by(HF) %>% mutate(cumprob = cumsum(treated))

print(ggplot(arthf
	, aes(x=time,y=cumprob,group=HF,col=factor(HF))) 
	+xlab("Time") 
	+ ylab("Cumulative Probability") 
	+ geom_line() 
	+ ggtitle("Proportion HF in ART") 
	+ theme_bw()
)

artyearmod <- update(artmod,.~+TZsurdat$start_year)
artyear <- data.frame(time=artyearmod$time, treated = artyearmod$n.event/artmod$n, year=0)
artyearmod$strata

artyear$year[1:cumsum(artyearmod$strata)[4]] <- 2014 
artyear$year[1:cumsum(artyearmod$strata)[3]] <- 2013 
artyear$year[1:cumsum(artyearmod$strata)[2]] <- 2012  
artyear$year[1:cumsum(artyearmod$strata)[1]] <- 2011 

artyear <- artyear %>% group_by(year) %>% mutate(cumprob = cumsum(treated))

ggplot(artyear,aes(x=time,y=cumprob,group=year,col=factor(year))) +xlab("Time") + ylab("Cumulative Probability") +
  geom_line() + ggtitle("Proportion Enrolment Year in ART") + theme_bw()

print(percentagetable)
agebar <- percentagetable[3:6,]
agebar2 <- melt(agebar)
ggplot(agebar2, aes(x=variable,y=value,group=Factor,fill=Factor))+xlab("Enrolment Year")+
  ylab('Percentage')+geom_bar(stat = "identity")+theme_bw()+ggtitle('Age Categories by Enrolment Year')

cd4bar <- percentagetable[16:19,]
cd4bar2 <- melt(cd4bar)
ggplot(cd4bar2, aes(x=variable,y=value,group=Factor,fill=Factor))+xlab("Enrolment Year")+
  ylab('Percentage')+geom_bar(stat = "identity")+theme_bw()+ggtitle('CD4 Categories by Enrolment Year')

whobar <- percentagetable[11:15,]
whobar2 <- melt(whobar)
ggplot(whobar2, aes(x=variable,y=value,group=Factor,fill=Factor))+xlab("Enrolment Year")+
  ylab('Percentage')+geom_bar(stat = "identity")+theme_bw()+ggtitle('WHO Stage Categories by Enrolment Year')
