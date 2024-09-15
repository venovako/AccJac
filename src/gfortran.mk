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
ifndef CPU
CPU=native
endif # !CPU
ifeq ($(ARCH),ppc64le)
FCFLAGS += -mcpu=$(CPU) -mpower8-fusion -mtraceback=full
CFLAGS += -mcpu=$(CPU) -mpower8-fusion -mtraceback=full
else # !ppc64le
FCFLAGS += -march=$(CPU)
CFLAGS += -march=$(CPU)
endif # ?ppc64le
FCFLAGS += -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -fvect-cost-model=unlimited -ffp-contract=fast -rdynamic -static-libgcc
CFLAGS += -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -fvect-cost-model=unlimited -ffp-contract=fast -rdynamic -static-libgcc
ifdef NDEBUG
FCFLAGS += -fno-math-errno
CFLAGS += -fno-math-errno
else # !NDEBUG
FCFLAGS += -fcheck=all -finit-local-zero -finit-real=snan -finit-derived
endif # ?NDEBUG
FCFLAGS += -ffree-line-length-none -fprotect-parens -frecursive -fstack-arrays -Wall -Wextra -Wno-c-binding-type -Wno-compare-reals -Wno-function-elimination -Wno-uninitialized
ifdef MPFR
FCFLAGS += -DMPFR="\"$(MPFR)\""
CFLAGS += -I$(MPFR)/include
endif # MPFR
ifdef GMP
FCFLAGS += -DGMP="\"$(GMP)\""
CFLAGS += -I$(GMP)/include
endif # GMP
ifdef LAPACK
FCFLAGS += -DLAPACK="\"$(LAPACK)\""
endif # LAPACK
