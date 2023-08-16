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
qjaev2.o\
sjaev2.o\
yjaev2.o\
zjaev2.o\
cjiev2.o\
djiev2.o\
qjiev2.o\
sjiev2.o\
yjiev2.o\
zjiev2.o

EXES=\
ctest.exe\
dtest.exe\
stest.exe\
ztest.exe

LDFLAGS=-rdynamic
ifeq ($(COMPILER),gfortran)
OBJS += \
wjaev2.o\
xjaev2.o\
wjiev2.o\
xjiev2.o
EXES += \
wtest.exe\
xtest.exe
else # !gfortran
ifeq ($(OS),Linux)
LDFLAGS += -static-libgcc
endif # Linux
endif # ?gfortran
LDFLAGS += -L. -ljaev2 -L../../libpvn/src -lpvn $(CR_OBJS) $(LAPACK)

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
