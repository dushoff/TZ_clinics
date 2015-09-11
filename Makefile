### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: cd4look.Rout.visits.Rout 

cd4look.visits.Rout: keep.visits.Rout cd4look.R

enrolment.sample.Rout: keep.sample.Rout enrolment.R
	$(run-R)

##################################################################

# make files

Sources = Makefile .gitignore

Sources += R.mk
include R.mk

-include crib.mk

######################################################################

ms = ../makestuff

-include $(ms)/local.mk
-include local.mk
-include $(ms)/git.mk

-include $(ms)/visual.mk
# -include $(ms)/linux.mk

-include $(ms)/RR.mk
# -include oldlatex.mk
