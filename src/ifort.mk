AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=$(COMPILER_PREFIX)ifort$(COMPILER_SUFFIX)
CC=$(COMPILER_PREFIX)icc$(COMPILER_SUFFIX)
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
# COMMON-AVX512 for KNLs
endif # !CPU
FCFLAGS += -x$(CPU) -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -qopt-multi-version-aggressive -qopt-zmm-usage=high -fp-model precise -fma -fprotect-parens -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt -qsimd-honor-fp-model -qsimd-serialize-fp-reduction -recursive -standard-semantics -traceback -vec-threshold0
CFLAGS += -x$(CPU) -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -qopt-multi-version-aggressive -qopt-zmm-usage=high -fp-model precise -fma -fprotect-parens -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt -qsimd-honor-fp-model -qsimd-serialize-fp-reduction -traceback -vec-threshold0
ifdef NDEBUG
FCFLAGS += -fno-math-errno -qopt-report=5 -diag-disable=10397
CFLAGS += -fno-math-errno -qopt-report=5 -diag-disable=10397
ifndef PROFILE
FCFLAGS += -inline-level=2
CFLAGS += -inline-level=2
endif # !PROFILE
else # !NDEBUG
FCFLAGS += -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -check all -fp-stack-check
CFLAGS += -debug extended -debug inline-debug-info -debug pubnames
endif # ?NDEBUG
FCFLAGS += -diag-disable=8293
CFLAGS += -diag-disable=10441
ifdef MPFR
FCFLAGS += -DMPFR="\"$(MPFR)\""
CFLAGS += -I$(MPFR)/include
endif # MPFR
ifdef GMP
FCFLAGS += -DGMP="\"$(GMP)\""
CFLAGS += -I$(GMP)/include
endif # GMP
