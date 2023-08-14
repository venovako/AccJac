ifdef CR_MATH
FCFLAGS += -DCR_MATH=$(CR_MATH)
ifdef NDEBUG
# modified routines that do not reference errno
CRF_MATH=$(CR_MATH)/src/binary32/hypot/hypotf_noerrno.o
CRD_MATH=$(CR_MATH)/src/binary64/hypot/hypot_noerrno.o
else # !NDEBUG
CRF_MATH=$(CR_MATH)/src/binary32/hypot/hypotf.o
CRD_MATH=$(CR_MATH)/src/binary64/hypot/hypot.o
endif # ?NDEBUG
endif # CR_MATH
