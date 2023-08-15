AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=$(COMPILER_PREFIX)ifx$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0 -g
endif # ?NDEBUG
FCFLAGS += -xHost -fPIC -fexceptions -fno-omit-frame-pointer -standard-semantics -recursive -traceback -vec-threshold0 -fp-model precise -fma -fprotect-parens -no-ftz
ifdef NDEBUG
FCFLAGS += -inline-level=2 -qopt-report=3
else # !NDEBUG
FCFLAGS += -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -check all
endif # ?NDEBUG
