AR=ar
ARFLAGS=rsv
FC=$(COMPILER_PREFIX)gfortran$(COMPILER_SUFFIX)
CC=$(COMPILER_PREFIX)gcc$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
CFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-Og -ggdb3
CFLAGS=-Og -ggdb3
endif # ?NDEBUG
ifeq ($(ARCH),ppc64le)
FCFLAGS += -mcpu=native -mtraceback=full
CFLAGS += -mcpu=native -mtraceback=full
else # !ppc64le
FCFLAGS += -march=native
CFLAGS += -march=native
endif # ?ppc64le
FCFLAGS += -fPIC -fexceptions -fno-omit-frame-pointer -fvect-cost-model=unlimited -ffp-contract=fast
CFLAGS += -fPIC -fexceptions -fno-omit-frame-pointer -fvect-cost-model=unlimited -ffp-contract=fast
ifdef NDEBUG
FCFLAGS += -fno-math-errno
CFLAGS += -fno-math-errno
else # !NDEBUG
FCFLAGS += -fcheck=all -finit-local-zero -finit-real=snan -finit-derived
endif # ?NDEBUG
FCFLAGS += -fprotect-parens -frecursive -fstack-arrays -pedantic -Wall -Wextra -Wno-c-binding-type -Wno-compare-reals -Wno-uninitialized
ifdef MPFR
CFLAGS += -I$(MPFR)/include
endif # MPFR
ifdef GMP
CFLAGS += -I$(GMP)/include
endif # GMP
