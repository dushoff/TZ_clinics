### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: survival.sample.Rout 

##################################################################

ms = ../makestuff
msrepo = https://github.com/dushoff

-include $(ms)/git.def
-include ../local/local.mk

test:
	echo $(parallel)

parallel = makestuff

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

-include ../local.mk
-include local.mk
-include $(ms)/git.mk

-include $(ms)/visual.mk

-include $(ms)/wrapR.mk
# -include oldlatex.mk

### Makestuff

Makefile: libR.makestuff

%.makestuff:
	-cd $(dir $(ms)) && mv -f $(notdir $(ms)) .$(notdir $(ms))
	cd $(dir $(ms)) && git clone $(msrepo)/$(notdir $(ms)).git
	-cd $(dir $(ms)) && rm -rf .$(notdir $(ms))
	touch $@

$(ms): 
	cd $(dir $(ms)) && git clone $(msrepo)/$(notdir $(ms)).git
