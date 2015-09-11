set.seed(227)
testSize <- 500

patients <- unique(c_visits$patientid)

patSample <- sample(patients, testSize)

c_visits <- subset(c_visits, patientid %in% patSample)

