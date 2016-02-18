### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: survival_plots.sample.Rout

##################################################################

tables.Rout: keep.visits.Rout tables.R

# make files

Sources = Makefile .gitignore README.md stuff.mk
include stuff.mk

Outside = children_visits.RData
dushoff_update:
	/bin/cp /home/dushoff/Dropbox/ICI3D/children_visits.RData .
Sources += R.mk
include R.mk

-include crib.mk

survival_overview.html:	survival_overview.Rmd
			Rscript -e "library(\"rmarkdown\"); render(\"$<\")"

######################################################################

Sources += Eligibility.mkd notes.txt todo.mkd

-include $(ms)/git.mk

-include $(ms)/visual.mk
-include $(ms)/wrapR.mk
# -include oldlatex.mk
