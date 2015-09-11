## The data file is not available on this public repo. You need to copy it into your working directory
children_visits.RData children_visits.Rout:

### Make a subsample for quicker comparison 

Sources += sample.R
sample.Rout: children_visits.RData sample.R
	$(run-R)

sample.RData:

### Decide what variables to keep for a simpler data frame
### JD wants less (we can always add more), but JN does not agree.

Sources += keep.R
keep.sample.RData keep.visits.RData:

keep.sample.Rout: sample.Rout keep.R
	$(run-R)

keep.visits.Rout: children_visits.Rout keep.R
	$(run-R)

### What do the cd4 data look like?
Sources += cd4look.R
cd4look.Rout.%.Rout: keep.%.Rout cd4look.R
	$(run-R)

### Analyze something about enrolment

Sources += enrolment.R
enrolment.sample.Rout: keep.sample.Rout enrolment.R
