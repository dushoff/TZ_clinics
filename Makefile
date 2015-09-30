### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: firstdate.sample.Rout 

##################################################################

tables.Rout: keep.visits.Rout tables.R

# make files

Sources = Makefile .gitignore README.md

Outside = children_visits.RData

Sources += R.mk
include R.mk

-include crib.mk

######################################################################

Sources += Eligibility.mkd

ms = ../makestuff
repo = https://github.com/dushoff
-include $(ms)/local.mk
-include local.mk
-include $(ms)/git.mk

-include $(ms)/visual.mk

-include $(ms)/RR.mk
# -include oldlatex.mk

Makefile: $(ms)

$(ms): 
	cd $(dir $(ms)) && git clone $(repo)/$(notdir $(ms)).git
