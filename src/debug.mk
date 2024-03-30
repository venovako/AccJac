ifdef NDEBUG
FCFLAGS += -DNDEBUG=$(NDEBUG)
CFLAGS += -DNDEBUG=$(NDEBUG)
endif # NDEBUG
ifdef PROFILE
FCFLAGS += -DPVN_PROFILE=$(PROFILE)u -fno-inline -finstrument-functions
endif # PROFILE
