ifeq ($(COMPILER),gf)
FCFLAGS=-fdefault-integer-8
else # !gfortran
FCFLAGS=-i8
endif # ?gfortran
CPPFLAGS += -DMKL_ILP64=$(ABI)
