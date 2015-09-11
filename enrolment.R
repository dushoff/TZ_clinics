
library(dplyr)


# load("~/tz_pediatric_hiv/c_visits.RData")

# Summarize variables (works well except for character variables)
print(summary(analysisFrame))

# Use factor to get a quick idea about the character variables
for (n in names(analysisFrame)){
	v <- analysisFrame[[n]]
	if (is.character(v)){
		print(n)
		print(summary(as.factor(v)))
	}
}

library(dplyr)

summarise(analysisFrame,
	age=mean(age, na.rm=FALSE)
)

##trying to find minimum visit per patient using dplyr
## NOT WORKING!!!!
mutate(analysisFrame,
	minVis=min(visitdate)
)    

categories<-c(2,5,10,15)        ##Tried categories for age at enrolment

analysisFrame$ageCat <- cut(analysisFrame$age, breaks=categories)

table(analysisFrame$tbstatuscode)

summary(analysisFrame$age)

summary(analysisFrame)

table(analysisFrame$visittypecode)

table(analysisFrame$sex, analysisFrame$status)

hist(analysisFrame$age, col = 'blue', xlab = 'Patients age')


###remove all missing cd4

cd4dat <- analysisFrame %>% filter(cd4>0) %>% group_by(patientid) %>% filter(row_number() == 1)   #should the dataframe be sorted before?

test <- left_join(cd4dat[,1],analysisFrame,by='patientid')

first <- test %>% group_by(patientid) %>% filter(row_number() == 1)

daylags <- cd4dat$visitdate - first$visitdate 

newdat <- data.frame(first,dday=daylags)

