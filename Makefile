### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: sample.Rout 

##################################################################

# make files

Sources = Makefile .gitignore

## Not a source on this public repo!!!!!

Sources += R.mk
include R.mk

######################################################################

ms = ../makestuff

-include $(ms)/local.mk
-include local.mk
-include $(ms)/git.mk

-include $(ms)/visual.mk
# -include $(ms)/linux.mk

-include $(ms)/RR.mk
# -include oldlatex.mk
