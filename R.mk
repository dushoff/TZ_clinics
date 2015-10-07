## The data file is not available on this public repo. You need to copy it into your working directory

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

keep.visits.Rout: children_visits.RData keep.R
	$(run-R)

### What do the cd4 data look like?
Sources += cd4look.R
cd4look.visits.Rout: cd4look.R
cd4look.%.Rout: keep.%.Rout cd4look.R
	$(run-R)

### Eligibility logic
Sources += eligible.R
eligible.visits.Rout: eligible.R
eligible.%.Rout: keep.%.Rout eligible.R
	$(run-R)

### Make a patient data frame with first dates for:
###### enrolment, eligibility, cd4, ARV

Sources += firstdate.R
firstdate.visits.Rout: firstdate.R
firstdate.sample.Rout: firstdate.R
firstdate.%.Rout: eligible.%.Rout firstdate.R
	$(run-R)

#### Tables of Baselines and starting

Sources += baselinetable.R
baselinetable.%.Rout: firstdate.%.Rout baselinetable.R
	$(run-R)

#### Survival stuff

Sources += survival.R
survival.visits.Rout: survival.R
survival.sample.Rout: survival.R
survival.%.Rout: baselinetable.%.Rout survival.R
	$(run-R)

