include system.mk
include $(COMPILER).mk
include $(ARCH).mk
include $(ABI).mk
include $(OS).mk
include debug.mk
MKFS=GNUmakefile system.mk $(COMPILER).mk $(ARCH).mk $(ABI).mk $(OS).mk debug.mk
ifneq ($(wildcard local.mk),)
include local.mk
MKFS += local.mk
endif # local.mk

OBJS=\
cjaev2.o\
djaev2.o\
sjaev2.o\
wjaev2.o\
xjaev2.o\
zjaev2.o\
cjiev2.o\
djiev2.o\
sjiev2.o\
wjiev2.o\
xjiev2.o\
zjiev2.o

EXES=\
ctest.exe\
dtest.exe\
stest.exe\
wtest.exe\
xtest.exe\
ztest.exe

LDFLAGS=-rdynamic -L. -ljaev2 -L../../libpvn/src -lpvn $(CR_OBJS) $(LAPACK)
ifeq ($(COMPILER),gfortran)
ifeq ($(OS),Linux)
LDFLAGS += -L${ONEAPI_ROOT}/compiler/latest/linux/compiler/lib/intel64 -limf
else # Darwin
LDFLAGS += -L${ONEAPI_ROOT}/compiler/latest/mac/compiler/lib -limf
endif # ?Linux
else # Intel Fortran
ifeq ($(OS),Linux)
LDFLAGS += -static-libgcc
endif # Linux
endif # !gfortran

.PHONY: all help clean

all: libjaev2.a $(EXES)

libjaev2.a: $(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

help:
	@echo $(MAKE) "[COMPILER=gfortran|ifort|ifx] [COMPILER_PREFIX=...] [COMPILER_SUFFIX=...] [ABI=lp64|ilp64] [NDEBUG=optimization_level] [CR_MATH=dir] [all|help|clean]"

%.o : %.f90
	$(FC) $(FCFLAGS) -c $< -o $@

%.exe : %.F90
	$(FC) $(FCFLAGS) $< -o $@ $(LDFLAGS)

clean:
	-$(DEL) $(OBJS)
	-$(DEL) libjaev2.a
	-$(DEL) $(EXES)
	-$(DEL) *.dSYM
	-$(DEL) *.optrpt
	-$(DEL) *.opt.yaml
