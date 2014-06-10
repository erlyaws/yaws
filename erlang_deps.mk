incdep = include $(DEPDIR)/$(1)
$(eval $(shell $(MKDIR_P) $(DEPDIR)))
$(foreach module,$(MODULES), $(shell touch $(DEPDIR)/$(module:%.erl=%.Pbeam)))
$(foreach module,$(MODULES), $(eval $(call incdep,$(module:%.erl=%.Pbeam))))


# Local Variables:
#    tab-width: 8
# End:
