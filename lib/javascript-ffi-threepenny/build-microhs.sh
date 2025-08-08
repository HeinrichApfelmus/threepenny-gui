#!/bin/sh

# Use variables to avoid adding stuff to target.conf for now
export CC=emcc
export MHSCCFLAGS="-O3 -sEXPORTED_FUNCTIONS=_apply_sp,_main -sEXPORTED_RUNTIME_METHODS=stringToNewUTF8 -sALLOW_MEMORY_GROWTH -sTOTAL_STACK=5MB -sSINGLE_FILE -DUSE_SYSTEM_RAW -Wno-address-of-packed-member"
export MHSCCLIBS="-lm"
export MHSCONF="unix"

mhs -tenvironment -i./src Foreign.TestJavaScript -oTest.js
# mhs -ddump-combinator -i./src Foreign.TestJavaScript
