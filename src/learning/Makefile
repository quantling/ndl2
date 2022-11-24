################################################################################
# Options
#
# In addition to the standard settings below, the following variables can be set
#   VERBOSE    if defined all commmands in the make targets will be printed
#   BINDIR     directory path into which make install installs the executables
#   LIBDIR     directory path into which make install installs the libraries
#   NO_OPENMP  build without OpenMP. Mostly useful for debugging
#   RELEASE    if defined a release build is made; already set with make release


################################################################################
# Standard settings

# Further options to CXXFLAGS get added in ../common/Makefile-Vars_Funcs,
# i.e. openmp flag depending on NO_OPENMP and debug and optimization flags
# depending on RELASE, as well as -MMD for dependency handling
CXXFLAGS := -Wall -Wextra -g -std=c++11 -fPIC
LDLIBS   := -lboost_filesystem -lboost_system -lboost_regex -licuuc -lm -lstdc++


################################################################################
# Variables and functions shared with other Makefiles, as well as pattern rules
# (instead of suffix rules) for building .o objects from .cpp files:

include ../common/Makefile-Vars_Funcs


################################################################################
# Executables / shared libraries and sources

RESCORLA_WAGNER     := iterative_rescorla_wagner
CREATE_SAMPLE_EVENT := create_sample_event
OUTPUT_MATRIX_FILE  := output_matrix_file_to_ascii
COUNT_EVENTS        := count_events
SPLIT_EVENTS        := split_events
SHARED_LIBRARY      := libiterative_rescorla_wagner.so
UNIT_TESTER         := tester

# Recursive definition for ALL_TARGETS necessary because of add_targets_from_var
ALL_TARGETS          = $(RESCORLA_WAGNER) $(CREATE_SAMPLE_EVENT) \
                         $(OUTPUT_MATRIX_FILE) $(COUNT_EVENTS) $(SPLIT_EVENTS) \
                         $(SHARED_LIBRARY) $(UNIT_TESTER)
DEFAULT_TARGETS     := $(filter-out $(UNIT_TESTER), $(ALL_TARGETS))


# Sources used in several target objects
COMMON_SRCS              := $(addprefix $(COMMON_DIR)/,\
                              serialization.cpp helper.cpp Exception.cpp)
LEARNING_COMMON_SRCS     := rescorla_wagner.cpp util.cpp file_management.cpp \
                              events_manager.cpp matrix_io.cpp trace_setup.cpp \
                              $(COMMON_SRCS)

# The objects which make up the individual target objects
RESCORLA_WAGNER_SRCS     := main.cpp $(LEARNING_COMMON_SRCS)
CREATE_SAMPLE_EVENT_SRCS := create_sample_event.cpp $(LEARNING_COMMON_SRCS)
OUTPUT_MATRIX_FILE_SRCS  := output_matrix_file_to_ascii.cpp \
                              $(LEARNING_COMMON_SRCS)
COUNT_EVENTS_SRCS        := count_events.cpp $(COMMON_SRCS)
SPLIT_EVENTS_SRCS        := split_events.cpp $(COMMON_SRCS)
SHARED_LIBRARY_SRCS      := $(LEARNING_COMMON_SRCS)
UNIT_TESTER_SRCS         := $(wildcard $(COMMON_DIR)/test*.cpp) \
                              $(wildcard test*.cpp) $(LEARNING_COMMON_SRCS) \
                              $(COMMON_DIR)/view_events.cpp

# Objects only used in R package
R_SRCS                   := r_wrapper.cpp

# The sort function is used to make remove duplicate entries from the list
ALL_SRCS := $(sort $(RESCORLA_WAGNER_SRCS) $(CREATE_SAMPLE_EVENT_SRCS) \
              $(OUTPUT_MATRIX_FILE_SRCS) $(COUNT_EVENTS_SRCS) \
              $(SPLIT_EVENTS_SRCS) $(SHARED_LIBRARY_SRCS) \
              $(UNIT_TESTER_SRCS) $(R_SRCS))

################################################################################
# Targets
#
.PHONY: all release clean cleanR distclean test testAll install

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
	$(Q)$(RM) $(SHARED_LIBRARY)
	$(Q)$(RM) -r $(TEST_TMP_DIR)/*

# remove objects that get created when R package is build
cleanR:
	$(Q)$(RM) $(patsubst %.cpp,%.o,$(ALL_SRCS))
	$(Q)$(RM) -r $(TEST_TMP_DIR)/*

# clean up everything that might have gotten created
distclean: clean cleanR
	$(Q)$(RM) $(addsuffix *, $(patsubst %.cpp,%.d,$(sort $(ALL_SRCS))))
	$(Q)$(RM) .deps/*.P .deps/common/*.P

test: all $(BUILD_DIR)/$(UNIT_TESTER)
	$(Q)$(RM) -r $(TEST_TMP_DIR)/*
	$(Q)$(BUILD_DIR)/tester -i
	./test_handmade_matrix.pl $(BUILD_TYPE)

testAll: test
	./run_largescale_tests.pl $(BUILD_TYPE) tiniest.corpus
# corpus not included in ndl2 git repository because it is too big (> 100 MB)
#	./run_largescale_tests.pl larger.txt.split --no-matrices


install: release
	@echo "Installing into $(BINDIR)\\n>>>$(if $(filter /usr%,$(BINDIR)),\
		Please make sure you have sufficient rights to install to this location \
				\\n or set BINDIR to a different path. For example: \
				make BINDIR=~/bin install,\
			Please make sure you have the path to this directory in your PATH) <<<\\n"
	$(INSTALL) -v $(addprefix $(RELEASE_DIR)/,$(filter-out $(SHARED_LIBRARY), \
			$(DEFAULT_TARGETS))) $(BINDIR)
	@echo "\\n\\nInstalling into $(LIBDIR)\\n>>>$(if $(filter /usr%,$(LIBDIR)),\
		Please make sure you have sufficient rights to install to this location \
				\\n or set LIBDIR to a different path. For example: \
				make LIBDIR=~/lib install,\
			Please make sure you have this path in your LD_LIBRARY_PATH) <<<\\n"
	$(INSTALL) -v $(RELEASE_DIR)/$(SHARED_LIBRARY) $(LIBDIR)/


# Include automatically generated dependencies (%.d target rules)
-include $(patsubst %.o,%.$(BUILD_TYPE).P,\
	$(call get_output_path,$(sort $(ALL_SRCS)),$(DEP_DIR)))


# vim: set noexpandtab ts=2 sw=2:
