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

### Eligibility logic
Sources += eligible.R
eligible.sample.Rout: eligible.R
eligible.%.Rout: keep.%.Rout dplyr.Rlib.R eligible.R
	$(run-R)

### Make a patient data frame with first dates for:
###### enrolment, eligibility, cd4, ARV, and baselines 

Sources += patientTable.R
patientTable.visits.Rout: patientTable.R
patientTable.sample.Rout: patientTable.R
patientTable.%.Rout: eligible.%.Rout patientTable.R
	$(run-R)

#### Tables of Baselines and starting
#### Summary tables

Sources += baselinetables.R
baselinetables.sample.Rout: baselinetables.R
baselinetables.%.Rout: patientTable.%.Rout baselinetables.R
	$(run-R)

#### Survival stuff

Sources += survival.R rgb.R
survival.visits.Rout: survival.R
survival.sample.Rout: survival.R
survival.%.Rout: patientTable.%.Rout dplyr.Rlib.R rgb.R survival.R
	$(run-R)

Sources += survival_plots.R 
survival_plots.%.Rout:	    survival.%.Rout survival_plots.R
			    $(run-R)
