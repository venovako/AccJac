AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=$(COMPILER_PREFIX)ifx$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0 -g
endif # ?NDEBUG
FCFLAGS += -xHost -qopenmp -fPIC -fexceptions -fno-omit-frame-pointer -fp-model precise -fprotect-parens -fma -no-ftz -standard-semantics -traceback -vec-threshold0
ifdef NDEBUG
FCFLAGS += -inline-level=2 -qopt-report=3
else # !NDEBUG
FCFLAGS += -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -check all
ifeq ($(OS),Linux)
FCFLAGS += -debug parallel
endif # Linux
endif # ?NDEBUG
