AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=$(COMPILER_PREFIX)ifort$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0 -g
endif # ?NDEBUG
FCFLAGS += -xHost -qopenmp -qopt-multi-version-aggressive -qopt-zmm-usage=high -fPIC -fexceptions -fno-omit-frame-pointer -fp-model precise -fprotect-parens -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt -standard-semantics -traceback -vec-threshold0 -qsimd-honor-fp-model -qsimd-serialize-fp-reduction -rdynamic
ifdef NDEBUG
FCFLAGS += -qopt-report=5 -inline-level=2
else # !NDEBUG
FCFLAGS += -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -check all -fp-stack-check
endif # ?NDEBUG
ifeq ($(OS),Linux)
ifndef NDEBUG
FCFLAGS += -debug parallel
endif # !NDEBUG
FCFLAGS += -static-libgcc
endif # Linux
