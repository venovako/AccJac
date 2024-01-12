AR=ar
ARFLAGS=rsv
FC=$(COMPILER_PREFIX)gfortran$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-Og -ggdb3
endif # ?NDEBUG
FCFLAGS += -pedantic -Wall -Wextra -Wno-c-binding-type -Wno-compare-reals -Wno-uninitialized
ifeq ($(ARCH),ppc64le)
FCFLAGS += -mcpu=native -mtraceback=full
else # !ppc64le
FCFLAGS += -march=native
endif # ?ppc64le
FCFLAGS += -fPIC -fexceptions -fno-omit-frame-pointer -frecursive -fstack-arrays -fvect-cost-model=unlimited -ffp-contract=fast -fprotect-parens
ifdef NDEBUG
FCFLAGS += -fno-math-errno
else # !NDEBUG
FCFLAGS += -fcheck=all -finit-local-zero -finit-real=snan -finit-derived
endif # ?NDEBUG
