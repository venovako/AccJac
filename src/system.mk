ifndef ABI
ABI=lp64
endif # !ABI
OS=$(shell uname)
ARCH=$(shell uname -m)
DEL=rm -frv
