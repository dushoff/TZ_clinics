library(dplyr)
library(tidyr)

year <- (c_visits
	%>% select(patientid,arvstatuscode,visitdate,visitnum)
	%>% group_by(patientid)
	%>% mutate(startyear = as.numeric(format(min(visitdate),"%Y")))
)

arvyear <- (year
	%>% group_by(patientid)
	%>% filter(arvstatuscode == "Start ARV")
   %>% mutate(arvyear = as.numeric(format(min(visitdate),"%Y")))
)

tmpd <- (arvyear %>% select(c(startyear,arvyear)))

### Collapse columns into key/value pairs. JD does not like it.
gather_by_year <- (gather(tmpd[,2:3],startyear,arvyear)) 

all <- (year %>% filter(visitnum == 1) %>% select(startyear) %>% group_by(startyear))

allcount <- (count(all,startyear))   ## Is this counting all patients by their visit number one per startyear?
newtable <- (count(newdat2,startyear,arvyear) %>% ungroup %>% arrange(startyear))
yeartotal <- (count(newdat2,startyear) %>% ungroup %>% arrange(startyear))

newtable2 <- (matrix(NA,nrow=4,ncol=5))
newtable2[1,1] <- (newtable$n[1])      #are [1,1] these number stands for row number and column number for a table?
newtable2[1,2] <- (newtable$n[2])
newtable2[1,3] <- (newtable$n[3])
newtable2[1,4] <- (newtable$n[4])
newtable2[1,5] <- (allcount$n[1] - yeartotal$n[1])

newtable2[2,2] <- (newtable$n[5])
newtable2[2,3] <- (newtable$n[6])
newtable2[2,4] <- (newtable$n[7])
newtable2[2,5] <- (allcount$n[2] - yeartotal$n[2])

newtable2[3,3] <- (newtable$n[8])
newtable2[3,4] <- (newtable$n[9])
newtable2[3,5] <- (allcount$n[3] - yeartotal$n[3])

newtable2[4,4] <- (newtable$n[10])
newtable2[4,5] <- (allcount$n[4] - yeartotal$n[4])

rownames(newtable2) <- (c("2011","2012","2013","2014"))
colnames(newtable2) <- (c("2011","2012","2013","2014","NA"))

print(newtable2)

library(ggplot2)

g <- (ggplot(newtable, aes(x=arvyear, y=n, colour=factor(startyear),  group = startyear) ) + geom_line() + geom_point() + theme_bw())
print(g)
