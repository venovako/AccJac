AR=ar
ARFLAGS=rsv
FC=$(COMPILER_PREFIX)flang$(COMPILER_SUFFIX)
FCFLAGS=-cpp -DUSE_IEEE_INTRINSIC=IEEE_FMA
ifdef NDEBUG
FCFLAGS += -O$(NDEBUG)
else # !NDEBUG
FCFLAGS += -O0 -g
endif # ?NDEBUG
ifndef MARCH
MARCH=native
endif # !MARCH
FCFLAGS += -march=$(MARCH) -fPIC -fno-omit-frame-pointer -ffp-contract=fast -fhonor-infinities -fhonor-nans -fimplicit-none -fstack-arrays
ifeq ($(ARCH),Darwin)
FCFLAGS += -fintegrated-as
endif # Darwin
ifdef MPFR
FCFLAGS += -DMPFR="\"$(MPFR)\""
endif # MPFR
ifdef GMP
FCFLAGS += -DGMP="\"$(GMP)\""
endif # GMP
