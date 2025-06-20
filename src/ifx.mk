AR=ar
ARFLAGS=rsv
FC=$(COMPILER_PREFIX)ifx$(COMPILER_SUFFIX)
FCFLAGS=-fpp #-DUSE_IEEE_INTRINSIC=IEEE_FMA
ifdef NDEBUG
FCFLAGS += -O$(NDEBUG)
else # !NDEBUG
FCFLAGS += -O0 -g
endif # ?NDEBUG
ifndef MARCH
MARCH=Host
# common-avx512 for KNLs
endif # !MARCH
FCFLAGS += -x$(MARCH) -mprefer-vector-width=512 -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -fp-model=precise -fp-speculation=safe -fimf-precision=high -fma -fprotect-parens -no-ftz -qopenmp -standard-semantics -traceback -vec-threshold0
# -recursive
ifdef NDEBUG
FCFLAGS += -fno-math-errno -qopt-report=3
ifndef PROFILE
FCFLAGS += -inline-level=2
endif # !PROFILE
else # !NDEBUG
FCFLAGS += -debug -check all
endif # ?NDEBUG
ifdef MPFR
FCFLAGS += -DMPFR="\"$(MPFR)\""
endif # MPFR
ifdef GMP
FCFLAGS += -DGMP="\"$(GMP)\""
endif # GMP
