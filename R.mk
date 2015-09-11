
Sources += sample.R
sample.Rout: children_visits.RData sample.R
	$(run-R)

sample.RData:

