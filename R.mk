
### Make a subsample for quicker comparison 

Sources += sample.R
sample.Rout: children_visits.RData sample.R
	$(run-R)

sample.RData:

### Keep far too many variables :-)

Sources += keep.R
keep.sample.RData keep.visits.RData:

keep.sample.Rout: sample.Rout keep.R
	$(run-R)

keep.visits.Rout: children_visits.Rout keep.R
	$(run-R)

