incdep = include $(DEPDIR)/$(1)
$(foreach module,$(MODULES), $(shell $(MKDIR_P) $(shell dirname $(DEPDIR)/$(module))))
$(foreach module,$(MODULES), $(shell touch $(DEPDIR)/$(module:%.erl=%.Pbeam)))
$(foreach module,$(MODULES), $(eval $(call incdep,$(module:%.erl=%.Pbeam))))


# Local Variables:
#    tab-width: 8
# End:
