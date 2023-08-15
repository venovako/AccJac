AR=ar
ARFLAGS=rsv
FC=$(COMPILER_PREFIX)gfortran$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-Og -ggdb3
endif # ?NDEBUG
FCFLAGS += -march=native -fPIC -fexceptions -fno-omit-frame-pointer -frecursive -fstack-arrays -fvect-cost-model=unlimited -ffp-contract=fast -fprotect-parens
ifdef NDEBUG
FCFLAGS += -fno-math-errno
else # !NDEBUG
FCFLAGS += -fcheck=all -finit-local-zero -finit-real=snan -finit-derived
endif # ?NDEBUG
