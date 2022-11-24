################################################################################
# Standard settings

# Further options to CXXFLAGS get added in ../common/Makefile-Vars_Funcs,
# i.e. openmp flag depending on NO_OPENMP and debug and optimization flags
# depending on RELASE, as well as -MMD for dependency handling
CXXFLAGS   := -Wall -Wextra -g -std=c++11
LDLIBS     := -licui18n -licuuc -lm -lstdc++


################################################################################
# Variables and functions shared with other Makefiles, as well as pattern rules
# (instead of suffix rules) for building .o objects from .cpp files:

include ../common/Makefile-Vars_Funcs


################################################################################
# Executables and sources

PREPROC          := ndlpreproc
VIEW_EVENTS      := view_events
UNIT_TESTER      := tester

# Recursive definition for ALL_TARGETS necessary because of add_targets_from_var
ALL_TARGETS       = $(PREPROC) $(VIEW_EVENTS) $(UNIT_TESTER)
DEFAULT_TARGETS  := $(filter-out $(UNIT_TESTER), $(ALL_TARGETS))


# Sources used in several target objects
COMMON_SRCS      := $(addprefix $(COMMON_DIR)/,\
                      serialization.cpp helper.cpp Exception.cpp)

# The objects which make up the individual target objects
PREPROC_SRCS     := main.cpp ndlpreproc.cpp $(COMMON_SRCS)
VIEW_EVENTS_SRCS := view_events_console.cpp $(COMMON_DIR)/view_events.cpp \
                      $(COMMON_SRCS)
UNIT_TESTER_SRCS := $(wildcard $(COMMON_DIR)/test*.cpp) \
                      $(wildcard test*.cpp) $(COMMON_DIR)/view_events.cpp \
                      ndlpreproc.cpp $(COMMON_SRCS)

# Objects only used in R package
R_SRCS          := ndlOutBuffer.cpp

# The sort function is used to make remove duplicate entries from the list
ALL_SRCS        := $(sort $(PREPROC_SRCS) $(VIEW_EVENTS_SRCS) \
                     $(UNIT_TESTER_SRCS) $(R_SRCS))


################################################################################
# Targets

.PHONY: all release clean cleanR distclean test install

all: $(addprefix $(BUILD_DIR)/,$(DEFAULT_TARGETS))

release:
	$(Q)$(MAKE) RELEASE=1 all $(if $(NO_CHECK),,test)

# add make targets for all executables, i.e. add target $(BUILD_DIR)/$(TARGET)
# for every variable within ALL_TARGETS.
$(call add_targets_from_var, ALL_TARGETS)

# clean up executables and object files and test output created from c++ builds
clean:
	$(Q)$(RM) $(call get_output_path,$(ALL_SRCS) $(ALL_TARGETS),$(DEBUG_DIR))
	$(Q)$(RM) $(call get_output_path,$(ALL_SRCS) $(ALL_TARGETS),$(RELEASE_DIR))
	$(Q)$(RM) -r $(TEST_TMP_DIR)/*

# remove objects that get created when R package is build
cleanR:
	$(Q)$(RM) $(patsubst %.cpp,%.o,$(ALL_SRCS))
	$(Q)$(RM) -r $(TEST_TMP_DIR)/*

# clean up everything that might have gotten created
distclean: clean cleanR
	$(Q)$(RM) $(addsuffix *, $(patsubst %.cpp,%.d,$(sort $(ALL_SRCS))))
	$(Q)$(RM) .deps/*.P .deps/common/*.P


# run unit tests
test: $(BUILD_DIR)/$(UNIT_TESTER)
	$(Q)$(RM) -r $(TEST_TMP_DIR)/*
	$(Q)./$< -i


# installing to $(BINDIR)
install: release
	@echo "Installing into $(BINDIR)\\n>>>$(if $(filter /usr%,$(BINDIR)),\
		Please make sure you have sufficient rights to install to this location \
				\\n or set BINDIR to a different path. For example: \
				make BINDIR=~/bin install,\
			Please make sure you have the path to this directory in your PATH) <<<\\n"
	$(INSTALL) -v $(addprefix $(RELEASE_DIR)/,$(DEFAULT_TARGETS)) $(BINDIR)


# Include automatically generated dependencies (%.d target rules)
#-include $(patsubst %.cpp,$(DEP_DIR)/%.$(BUILD_TYPE).P,$(sort $(ALL_SRCS)))
-include $(patsubst %.o,%.$(BUILD_TYPE).P,\
	$(call get_output_path,$(sort $(ALL_SRCS)),$(DEP_DIR)))


# vim: set noexpandtab ts=2 sw=2:
