ifeq ($(COMPILER),gfortran)
FCFLAGS += -fdefault-integer-8 -DMKL_ILP64=$(ABI)
else # !gfortran
FCFLAGS += -i8 -DMKL_ILP64=$(ABI)
endif # ?gfortran
