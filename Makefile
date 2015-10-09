### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: patientTable.sample.Rout 

##################################################################

ms = ../makestuff
-include $(ms)/git.def

test:
	echo $(parallel)

tables.Rout: keep.visits.Rout tables.R

# make files

Sources = Makefile .gitignore README.md

Outside = children_visits.RData
dushoff_update:
	/bin/cp /home/dushoff/Dropbox/ICI3D/children_visits.RData .
Sources += R.mk
include R.mk

-include crib.mk

######################################################################

Sources += Eligibility.mkd

-include $(ms)/local.mk
-include local.mk
-include $(ms)/git.mk

-include $(ms)/visual.mk

-include $(ms)/wrapR.mk
# -include oldlatex.mk

### Makestuff

Makefile: wrapR.makestuff

repo = https://github.com/dushoff
%.makestuff:
	-cd $(dir $(ms)) && mv -f $(notdir $(ms)) .$(notdir $(ms))
	cd $(dir $(ms)) && git clone $(repo)/$(notdir $(ms)).git
	-cd $(dir $(ms)) && rm -rf $(ms) .$(notdir $(ms))
	touch $@

$(ms): 
	cd $(dir $(ms)) && git clone $(repo)/$(notdir $(ms)).git
