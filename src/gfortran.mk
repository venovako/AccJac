AR=ar
ARFLAGS=rsv
FC=$(COMPILER_PREFIX)gfortran$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-Og -g
endif # ?NDEBUG
FCFLAGS += -march=native -fPIC -fopenmp -fexceptions -fno-omit-frame-pointer -ffp-contract=fast -fprotect-parens -fstack-arrays -fvect-cost-model=unlimited
ifdef NDEBUG
FCFLAGS += -fopt-info-optimized-vec
else # !NDEBUG
FCFLAGS += -fcheck=all -finit-local-zero -finit-real=snan -finit-derived
endif # ?NDEBUG
