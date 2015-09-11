cd4 <- subset(c_visits, !is.na(cd4))

print(c(
	visits=nrow(c_visits)
	, cd4visits=nrow(cd4)
))

print(c(
	patients=length(unique(c_visits$patientid))
	, cd4patients=length(unique(cd4$patientid))
))
