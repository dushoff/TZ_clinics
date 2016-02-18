msrepo = https://github.com/dushoff
gitroot = ./
Drop = ~/Dropbox

-include local.mk
-include $(gitroot)/local.mk
export ms = $(gitroot)/makestuff
-include $(ms)/os.mk
# include $(ms)/linux.mk

Makefile: $(ms) $(subdirs)

$(ms):
	cd $(dir $(ms)) && git clone $(msrepo)/$(notdir $(ms)).git
