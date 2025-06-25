AR=ar
ARFLAGS=rsv
FC=$(COMPILER_PREFIX)gfortran$(COMPILER_SUFFIX)
FCFLAGS=-cpp -DUSE_IEEE_INTRINSIC=IEEE_FMA
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
ifeq ($(findstring 86,$(ARCH)),86)
FCFLAGS += -DEXTENDED=10
endif # 86
endif # ?ppc64le
FCFLAGS += -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -fvect-cost-model=unlimited -ffp-contract=fast
ifdef NDEBUG
FCFLAGS += -fno-math-errno
else # !NDEBUG
FCFLAGS += -fcheck=all -finit-local-zero -finit-real=snan -finit-derived
endif # ?NDEBUG
ifndef THR
THR=frecursive
endif # !THR
FCFLAGS += -ffree-line-length-none -fprotect-parens -$(THR) -fstack-arrays -Wall -Wextra -Wno-c-binding-type -Wno-compare-reals -Wno-function-elimination -Wno-uninitialized
ifdef MPFR
FCFLAGS += -DMPFR="\"$(MPFR)\""
endif # MPFR
ifdef GMP
FCFLAGS += -DGMP="\"$(GMP)\""
endif # GMP
