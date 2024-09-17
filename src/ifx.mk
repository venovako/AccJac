AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=$(COMPILER_PREFIX)ifx$(COMPILER_SUFFIX)
CC=$(COMPILER_PREFIX)icx$(COMPILER_SUFFIX)
FCFLAGS=-fpp
CFLAGS=-std=gnu18
ifdef NDEBUG
FCFLAGS += -O$(NDEBUG)
CFLAGS += -O$(NDEBUG)
else # !NDEBUG
FCFLAGS += -O0 -g
CFLAGS += -O0 -g
endif # ?NDEBUG
ifndef CPU
CPU=Host
# common-avx512 for KNLs
endif # !CPU
FCFLAGS += -x$(CPU) -mprefer-vector-width=512 -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -fp-model=precise -fp-speculation=safe -fimf-precision=high -fma -fprotect-parens -no-ftz -recursive -standard-semantics -traceback -vec-threshold0
CFLAGS += -x$(CPU) -mprefer-vector-width=512 -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -fp-model=precise -fp-speculation=safe -fimf-precision=high -fma -fprotect-parens -no-ftz -traceback -vec-threshold0
ifdef NDEBUG
FCFLAGS += -fno-math-errno -qopt-report=3
CFLAGS += -fno-math-errno -qopt-report=3
ifndef PROFILE
FCFLAGS += -inline-level=2
CFLAGS += -inline-level=2
endif # !PROFILE
else # !NDEBUG
FCFLAGS += -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -check all
CFLAGS += -debug extended -debug inline-debug-info -debug pubnames
endif # ?NDEBUG
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
