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

OBJD=\
cjaevd.o\
zjaevd.o

LIB2=libjaev2.a
LIBD=libjaevd.a

EXE2=\
ctest.exe\
dtest.exe\
stest.exe\
ztest.exe

EXED=\
cevdj.exe\
zevdj.exe

LDFLAGS=-rdynamic
ifeq ($(COMPILER),gfortran)
ifneq ($(ARCH),ppc64le)
OBJ2 += \
wjaev2.o\
xjaev2.o\
wjiev2.o\
xjiev2.o
EXE2 += \
wtest.exe\
xtest.exe
endif # !ppc64le
else # !gfortran
ifeq ($(OS),Linux)
LDFLAGS += -static-libgcc
endif # Linux
endif # ?gfortran
LDFLAGS += -L. -ljaevd -ljaev2 -L../../libpvn/src -lpvn $(CR_OBJS) $(LAPACK)
ifeq ($(ARCH),ppc64le)
LDFLAGS += -Wl,--defsym=pvn_qljev2_=pvn_xljev2_ -Wl,--defsym=pvn_yljev2_=pvn_wljev2_
endif # ppc64le

.PHONY: all help clean

all: $(LIB2) $(LIBD) $(EXE2) $(EXED)

$(LIB2): $(OBJ2)
	$(AR) $(ARFLAGS) $@ $(OBJ2)

$(LIBD): $(OBJD)
	$(AR) $(ARFLAGS) $@ $(OBJD)

help:
	@echo $(MAKE) "[COMPILER=gfortran|ifort|ifx] [COMPILER_PREFIX=...] [COMPILER_SUFFIX=...] [ABI=lp64|ilp64] [NDEBUG=optimization_level] [CR_MATH=dir] [all|help|clean]"

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
