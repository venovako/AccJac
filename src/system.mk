ifndef COMPILER
COMPILER=gfortran
endif # !COMPILER
ifndef ARCH
ARCH=$(shell uname -m)
endif # !ARCH
ifndef ABI
ABI=lp64
endif # !ABI
ifndef OS
OS=$(shell uname)
endif # !OS
ifndef DEL
DEL=rm -frv
endif # !DEL
