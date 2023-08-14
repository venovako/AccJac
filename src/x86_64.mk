ifdef CR_MATH
FCFLAGS += -DCR_MATH=$(CR_MATH)
ifdef NDEBUG
# modified routines that do not reference errno
CRF_MATH=$(CR_MATH)/src/binary32/hypot/hypotf_noerrno.o $(CR_MATH)/src/binary32/rsqrt/rsqrtf_noerrno.o
CRD_MATH=$(CR_MATH)/src/binary64/hypot/hypot_noerrno.o $(CR_MATH)/src/binary64/rsqrt/rsqrt_noerrno.o
else # !NDEBUG
CRF_MATH=$(CR_MATH)/src/binary32/hypot/hypotf.o $(CR_MATH)/src/binary32/rsqrt/rsqrtf.o
CRD_MATH=$(CR_MATH)/src/binary64/hypot/hypot.o $(CR_MATH)/src/binary64/rsqrt/rsqrt.o
endif # ?NDEBUG
CR_OBJS=$(CRF_MATH) $(CRD_MATH)
endif # CR_MATH
