AR=ar
ARFLAGS=rsv
FC=$(COMPILER_PREFIX)gfortran$(COMPILER_SUFFIX)
FCFLAGS=-cpp
ifdef NDEBUG
FCFLAGS += -O$(NDEBUG)
else # !NDEBUG
FCFLAGS += -Og -ggdb3
endif # ?NDEBUG
ifndef MARCH
MARCH=native
endif # !MARCH
ifeq ($(ARCH),ppc64le)
FCFLAGS += -mcpu=$(MARCH) -mpower8-fusion -mtraceback=full
else # !ppc64le
FCFLAGS += -march=$(MARCH)
endif # ?ppc64le
FCFLAGS += -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -fvect-cost-model=unlimited -ffp-contract=fast
ifdef NDEBUG
FCFLAGS += -fno-math-errno
else # !NDEBUG
FCFLAGS += -fcheck=all -finit-local-zero -finit-real=snan -finit-derived
endif # ?NDEBUG
FCFLAGS += -ffree-line-length-none -fprotect-parens -frecursive -fstack-arrays -Wall -Wextra -Wno-c-binding-type -Wno-compare-reals -Wno-function-elimination -Wno-uninitialized
# -DCARITH_PVN="cma"
ifdef MPFR
FCFLAGS += -DMPFR="\"$(MPFR)\""
endif # MPFR
ifdef GMP
FCFLAGS += -DGMP="\"$(GMP)\""
endif # GMP
