### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: sample.Rout 

##################################################################


# make files

Sources = Makefile .gitignore

## Not a source on this public repo!!!!!

c_visits.RData c_visits.Rout:
	/bin/cp ../TZ_pediatric_HIV/$@ .

Sources += sample.R
sample.Rout: c_visits.RData sample.R
	$(run-R)

sample.RData:

######################################################################

ms = ../makestuff

-include $(ms)/local.mk
-include local.mk
-include $(ms)/git.mk

-include $(ms)/visual.mk
# -include $(ms)/linux.mk

-include $(ms)/RR.mk
# -include oldlatex.mk
