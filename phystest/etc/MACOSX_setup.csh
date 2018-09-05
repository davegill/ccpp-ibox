#!/bin/tcsh

echo "Setting environment variables for SCM-CCPP on MACOSX with clang/gfortran"

ml gnu
ml mpich

setenv CC gcc
setenv CXX g++
setenv F77 mpif77
setenv F90 mpif90
setenv FC mpif90
setenv CPP "mpif90 -E -x f95-cpp-input"

#setenv LDFLAGS "-L/usr/local/opt/zlib/lib -L/usr/local/opt/llvm/lib"
#setenv CPPFLAGS "-I/usr/local/opt/zlib/include -I/usr/local/opt/llvm/include"
#setenv CFLAGS "-I/usr/local/opt/zlib/include -I/usr/local/opt/llvm/include"
#setenv CXXFLAGS "-I/usr/local/opt/zlib/include -I/usr/local/opt/llvm/include"
#setenv FFLAGS "-I/usr/local/opt/zlib/include -I/usr/local/opt/llvm/include"
#setenv FCFLAGS " -I/usr/local/opt/zlib/include -I/usr/local/opt/llvm/include"

#if (! $?PATH) then
#  setenv PATH "/usr/local/opt/llvm/bin"
#else
#  setenv PATH "/usr/local/opt/llvm/bin:$PATH"
#endif
#if (! $?LD_LIBRARY_PATH) then
#  setenv LD_LIBRARY_PATH "/usr/local/opt/zlib/lib:/usr/local/opt/llvm/lib"
#else
#  setenv LD_LIBRARY_PATH "/usr/local/opt/zlib/lib:/usr/local/opt/llvm/lib:$LD_LIBRARY_PATH"
#endif
#if (! $?DYLD_LIBRARY_PATH) then
#  setenv DYLD_LIBRARY_PATH "/usr/local/opt/zlib/lib:/usr/local/opt/llvm/lib"
#else
#  setenv DYLD_LIBRARY_PATH "/usr/local/opt/zlib/lib:/usr/local/opt/llvm/lib:$DYLD_LIBRARY_PATH"
#endif

#setenv NETCDF /usr/local
