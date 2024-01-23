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
include $(ARCH).mk
include debug.mk
ifneq ($(ABI),lp64)
include $(ABI).mk
endif # !lp64
MKFS=GNUmakefile system.mk $(COMPILER).mk $(ARCH).mk debug.mk
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
ifndef MKL
OBJL += \
claev2.o\
dlaev2.o\
slaev2.o\
zlaev2.o
endif # !MKL

OBJ2=\
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
ztest.exe

OBJD=\
cjaevd.o\
zjaevd.o

LIBD=libjaevd.a

EXED=\
cevdj.exe\
zevdj.exe

LDFLAGS=-rdynamic
ifeq ($(OS),Linux)
LDFLAGS += -static-libgcc
endif # Linux
LDFLAGS += -L. -ljaevd -ljaev2 -L../../libpvn/src -lpvn $(CR_OBJS) $(MKL)
ifdef MPFR
LDFLAGS += -L$(MPFR)/lib -lmpfr
endif # MPFR
ifdef GMP
LDFLAGS += -L$(GMP)/lib -lgmp
endif # GMP
ifeq ($(ARCH),ppc64le)
LDFLAGS += -Wl,--defsym=pvn_qljev2_=pvn_xljev2_ -Wl,--defsym=pvn_yljev2_=pvn_wljev2_
endif # ppc64le

.PHONY: all help clean

all: $(LIB2) $(LIBD) $(EXE2) $(EXED)

$(LIB2): $(OBJ2) $(OBJL)
	$(AR) $(ARFLAGS) $@ $(OBJ2) $(OBJL)

$(LIBD): $(OBJD)
	$(AR) $(ARFLAGS) $@ $(OBJD)

help:
	@echo $(MAKE) "[COMPILER=gfortran|ifx|ifort] [COMPILER_PREFIX=...] [COMPILER_SUFFIX=...] [ABI=lp64|ilp64] [NDEBUG=optimization_level] [CR_MATH=dir] [MKL=...] [MPFR=...] [GMP=...] [all|help|clean]"

mtest.obj: mtest.c $(MKFS)
	$(CC) $(CFLAGS) -c $< -o $@

%.o : %.f90
	$(FC) $(FCFLAGS) -c $< -o $@

%.exe : %.F90
	$(FC) $(FCFLAGS) $< -o $@ $(LDFLAGS)

clean:
	-$(DEL) *.o
	-$(DEL) *.a
	-$(DEL) *.exe
	-$(DEL) *.obj
	-$(DEL) *.dSYM
	-$(DEL) *.optrpt
	-$(DEL) *.opt.yaml
