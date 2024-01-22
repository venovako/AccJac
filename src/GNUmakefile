include system.mk
include $(COMPILER).mk
include $(ARCH).mk
include $(ABI).mk
include debug.mk
MKFS=GNUmakefile system.mk $(COMPILER).mk $(ARCH).mk $(ABI).mk debug.mk
ifneq ($(wildcard local.mk),)
include local.mk
MKFS += local.mk
endif # local.mk

ifndef MKL
OBJL=\
claev2.o\
dlaev2.o\
slaev2.o\
zlaev2.o
endif # !MKL

OBJ2=\
cjaev2.o\
djaev2.o\
qjaev2.o\
qlaev2.o\
sjaev2.o\
yjaev2.o\
ylaev2.o\
zjaev2.o\
cjiev2.o\
djiev2.o\
qjiev2.o\
sjiev2.o\
yjiev2.o\
zjiev2.o

LIB2=libjaev2.a

EXE2=\
ctest.exe\
dtest.exe\
qtest.exe\
stest.exe\
ytest.exe\
ztest.exe

ifeq ($(COMPILER),gfortran)
ifneq ($(ARCH),ppc64le)
OBJ2 += \
wjaev2.o\
wlaev2.o\
xjaev2.o\
xlaev2.o\
wjiev2.o\
xjiev2.o

EXE2 += \
wtest.exe\
xtest.exe
endif # !ppc64le
else # !gfortran

ifneq ($(COMPILER),gfortran)
OBJD=\
cjaevd.o\
zjaevd.o

LIBD=libjaevd.a

EXED=\
cevdj.exe\
zevdj.exe
endif # !gfortran

LDFLAGS=-rdynamic
ifeq ($(OS),Linux)
LDFLAGS += -static-libgcc
endif # Linux
endif # ?gfortran
LDFLAGS += -L.
ifneq ($(COMPILER),gfortran)
LDFLAGS += -ljaevd
endif # !gfortran
LDFLAGS += -ljaev2 -L../../libpvn/src -lpvn $(CR_OBJS) $(MKL)
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
	@echo $(MAKE) "[COMPILER=gfortran|ifx|ifort] [COMPILER_PREFIX=...] [COMPILER_SUFFIX=...] [ABI=lp64|ilp64] [NDEBUG=optimization_level] [CR_MATH=dir] [MKL=...] [all|help|clean]"

%.o : %.f90
	$(FC) $(FCFLAGS) -c $< -o $@

%.exe : %.F90
	$(FC) $(FCFLAGS) $< -o $@ $(LDFLAGS)

clean:
	-$(DEL) *.o
	-$(DEL) *.a
	-$(DEL) *.exe
	-$(DEL) *.dSYM
	-$(DEL) *.optrpt
	-$(DEL) *.opt.yaml
