AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=$(COMPILER_PREFIX)ifort$(COMPILER_SUFFIX)
FCFLAGS=-fpp #-DUSE_IEEE_INTRINSIC=IEEE_FMA
ifdef NDEBUG
FCFLAGS += -O$(NDEBUG)
else # !NDEBUG
FCFLAGS += -O0 -g
endif # ?NDEBUG
ifndef MARCH
MARCH=Host
# COMMON-AVX512 for KNLs
endif # !MARCH
FCFLAGS += -x$(MARCH) -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -qopt-multi-version-aggressive -qopt-zmm-usage=high -fp-model precise -fma -fprotect-parens -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt -qsimd-honor-fp-model -qsimd-serialize-fp-reduction -recursive -standard-semantics -traceback -vec-threshold0
ifdef NDEBUG
FCFLAGS += -fno-math-errno -qopt-report=5 -diag-disable=10397
ifndef PROFILE
FCFLAGS += -inline-level=2
endif # !PROFILE
else # !NDEBUG
FCFLAGS += -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -check all -fp-stack-check
endif # ?NDEBUG
ifdef MPFR
FCFLAGS += -DMPFR="\"$(MPFR)\""
endif # MPFR
ifdef GMP
FCFLAGS += -DGMP="\"$(GMP)\""
endif # GMP
