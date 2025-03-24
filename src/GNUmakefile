include system.mk
ifdef MPFR
ifndef GMP
GMP=/usr
endif # !GMP
else # !MPFR
ifdef GMP
MPFR=/usr
endif # GMP
endif # ?MPFR
include $(COMPILER).mk
include debug.mk
ifneq ($(ABI),lp64)
include $(ABI).mk
endif # !lp64
MKFS=GNUmakefile system.mk $(COMPILER).mk debug.mk
ifneq ($(ABI),lp64)
MKFS += $(ABI).mk
endif # !lp64
ifneq ($(wildcard local.mk),)
include local.mk
MKFS += local.mk
endif # local.mk

OBJL=\
qlaev2.o\
ylaev2.o
ifndef LAPACK
OBJL += \
claev2.o\
dlaev2.o\
slaev2.o\
zlaev2.o
endif # !LAPACK

OBJ2=\
cjaev2.o\
cjiev2.o\
djaev2.o\
djiev2.o\
qjaev2.o\
qjiev2.o\
sjaev2.o\
sjiev2.o\
yjaev2.o\
yjiev2.o\
zjaev2.o\
zjiev2.o\
cljv2.o\
dljv2.o\
sljv2.o\
zljv2.o

ifdef MPFR
OBJ2 += \
mtest.obj
endif # MPFR

LIB2=libjaev2.a

EXE2=\
ctest.exe\
dtest.exe\
qtest.exe\
stest.exe\
ytest.exe\
ztest.exe\
cjv2t.exe\
djv2t.exe\
sjv2t.exe\
zjv2t.exe

ifeq ($(COMPILER),gfortran)
ifeq ($(findstring 86,$(ARCH)),86)
OBJL += \
wlaev2.o\
xlaev2.o

OBJ2 += \
wjaev2.o\
wjiev2.o\
xjaev2.o\
xjiev2.o\
wljv2.o\
xljv2.o

EXE2 += \
wtest.exe\
xtest.exe\
wjv2t.exe\
xjv2t.exe
endif # x86_64 or i86pc
endif # gfortran

OBJD=\
cjaevd.o\
zjaevd.o

LIBD=libjaevd.a

EXED=\
cevdj.exe\
zevdj.exe

LDFLAGS=-rdynamic -static-libgcc
ifeq ($(COMPILER),gfortran)
LDFLAGS += -static-libgfortran -static-libquadmath
endif # gfortran
LDFLAGS += -L. -ljaevd -ljaev2 -L../../libpvn/src -lpvn $(LAPACK)
ifdef MPFR
LDFLAGS += -L$(MPFR)/lib -lmpfr
endif # MPFR
ifdef GMP
LDFLAGS += -L$(GMP)/lib -lgmp
endif # GMP
ifdef ANIMATE
FCFLAGS += -DANIMATE=$(ANIMATE)
ifneq ($(COMPILER),gfortran)
LDFLAGS += $(realpath $(shell gcc -print-file-name=libquadmath.a))
endif # !gfortran
endif # ANIMATE
LDFLAGS += -ldl -lm

.PHONY: all help clean

all: $(LIB2) $(LIBD) $(EXE2) $(EXED)

$(LIB2): $(OBJ2) $(OBJL)
	$(AR) $(ARFLAGS) $@ $(OBJ2) $(OBJL)

$(LIBD): $(OBJD)
	$(AR) $(ARFLAGS) $@ $(OBJD)

help:
	@echo $(MAKE) "[COMPILER=gfortran|ifx] [COMPILER_PREFIX=...] [COMPILER_SUFFIX=...] [MARCH=...] [ABI=lp64|ilp64] [NDEBUG=optimization_level] [LAPACK=...] [MPFR=...] [GMP=...] [PROFILE=...] [ANIMATE=ppe] [all|help|clean]"

mtest.obj: mtest.c $(MKFS)
	$(CC) $(CFLAGS) -c $< -o $@

%.o : %.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c $< -o $@

%.exe : %.F90 $(MKFS)
	$(FC) $(FCFLAGS) $< -o $@ $(LDFLAGS)

clean:
	-$(DEL) *.o
	-$(DEL) *.a
	-$(DEL) *.exe
	-$(DEL) *.obj
	-$(DEL) *.dSYM
	-$(DEL) *.optrpt
	-$(DEL) *.opt.yaml
