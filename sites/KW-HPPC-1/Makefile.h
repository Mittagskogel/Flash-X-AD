# FLASH makefile definitions for x86-64 Linux (GNU compilers)

#----------------------------------------------------------------------------
# Set the AMReX library path -- manual installation for multiple variants
#----------------------------------------------------------------------------
ifeq ("$(USEOPENMP)","1")
  AMREX_PATH=${FLASHX_AMREX_OMP${NDIM}D_DIR}
else
  AMREX_PATH=${FLASHX_AMREX${NDIM}D_DIR}
endif

#MILHOJA_PATH=${FLASHX_MILHOJA${NDIM}D_DIR}
#MILHOJA_PATH=/nfs/gce/projects/FLASH5/kweide/localDbg-milhoja_grid/${NDIM}dcpu
MILHOJA_PATH=/nfs/gce/projects/FLASH5/kweide/local-milhoja/${NDIM}d

#----------------------------------------------------------------------------
# Set the HDF5/MPI library paths -- these need to be updated for your system
#----------------------------------------------------------------------------

MPI_PATH   = /usr/local/mpich2/
HYPRE_PATH = 
HDF5_PATH  = /usr/local/hdf5
HYPRE_PATH = /usr/local/hypre

ZLIB_PATH  =

PAPI_PATH  =
PAPI_FLAGS =

NCMPI_PATH = /usr/local/netcdf
MPE_PATH   =

#----------------------------------------------------------------------------
# Compiler and linker commands
#
#   Use the parallel HDF5 wrappers which use the mpiXX compiler wrappers 
#   -- these will automatically load the proper libraries and include files.
#----------------------------------------------------------------------------
FCOMP   = h5pfc
CCOMP   = h5pcc
CPPCOMP = mpicxx -std=c++17
LINK    = h5pfc -std=c++17

# pre-processor flag
PP      = -D

#----------------------------------------------------------------------------
# Compilation flags
#
#  Three sets of compilation/linking flags are defined: one for optimized
#  code, one for testing, and one for debugging.  The default is to use the
#  _OPT version.  Specifying -debug to setup will pick the _DEBUG version,
#  these should enable bounds checking.  Specifying _TEST is used for
#  flash_test, and is set for quick code generation, and (sometimes)
#  profiling.  The Makefile generated by setup will assign the generic token
#  (ex. FFLAGS) to the proper set of flags (ex. FFLAGS_OPT).
#----------------------------------------------------------------------------

OPENMP = -fopenmp

FFLAGS_OPT = -c -O2 -fdefault-real-8 -fdefault-double-8 -Wuninitialized
FFLAGS_DEBUG = -ggdb -c -O0 -fdefault-real-8 -fdefault-double-8 \
	-Wall -Wextra -Wno-unused -Waliasing \
	-Wno-unused-dummy-argument \
	-Wsurprising -Wconversion -Wunderflow \
	-ffpe-trap=invalid,zero,overflow -fbounds-check \
	-fimplicit-none -fstack-protector-all \
	-fbacktrace
FFLAGS_TEST = -ggdb -c -fdefault-real-8 -fdefault-double-8 \
	-ffree-line-length-none

FFLAGS_MPI = -DHAVE_MPI_MODULE
FFLAGS_HYPRE = -I${HYPRE_PATH}/include
CFLAGS_HYPRE = -I${HYPRE_PATH}/include
FFLAGS_AMREX = -I${AMREX_PATH}/include
FFLAGS_OACC  = -fopenacc
FFLAGS_MILHOJA = -I${MILHOJA_PATH}/include -fexceptions
FFLAGS_MILHOJA_AMREX = -I${MILHOJA_PATH}/include -fexceptions ${FFLAGS_AMREX}

F90FLAGS =

#The macro _FORTIFY_SOURCE adds some lightweight checks for buffer
#overflows at both compile time and run time (only active at -O1 or higher)
#http://gcc.gnu.org/ml/gcc-patches/2004-09/msg02055.html
CFLAGS_OPT = -c -O2 -Wuninitialized -D_FORTIFY_SOURCE=2
CFLAGS_DEBUG = -ggdb -c -O0 -Wno-div-by-zero -Wundef \
	-Wconversion -Wstrict-prototypes -Wunreachable-code \
	-Wall -Wextra -Winit-self -ftree-vrp -Wfloat-equal \
	-Wunsafe-loop-optimizations -Wpadded -fstack-protector-all 
CFLAGS_TEST = -c

CFLAGS_HDF5 = -DH5_USE_18_API -I$(HDF5_PATH)/include
CFLAGS_NCMPI = -I$(LIB_NCMPI)/include
CFLAGS_AMREX = -I${AMREX_PATH}/include
CFLAGS_OACC  = -fopenacc -DSUPPRESS_ACC_ROUTINE_FOR_METH_IN_APP
CFLAGS_MILHOJA = -I${MILHOJA_PATH}/include
CFLAGS_MILHOJA_AMREX = -I${MILHOJA_PATH}/include ${CFLAGS_AMREX}



#----------------------------------------------------------------------------
# Linker flags
#
#  There is a seperate version of the linker flags for each of the _OPT,
#  _DEBUG, and _TEST cases.
#----------------------------------------------------------------------------

LFLAGS_OPT   = -o
LFLAGS_DEBUG = -g -O0 -o
LFLAGS_TEST  = -o

#----------------------------------------------------------------------------
# Library specific linking
#
#  If a Flash-X source directory has a 'LIBRARY xxx' line in its Config file, we need to
#  create a macro in this Makefile.h for LIB_xxx, which will be added to the
#  link line when Flash-X is built.  This allows us to switch between different
#  (incompatible) libraries.  We also create a _OPT, _DEBUG, and _TEST
#  library macro to add any performance-minded libraries (like fast math),
#  depending on how Flash-X was setup.
#
#  Mostly handled by loading modules with h5pXX wrappers.
#----------------------------------------------------------------------------

LIB_OPT   =
LIB_DEBUG =
LIB_TEST  =

LIB_OACC  = -fopenacc
LIB_HDF5  = 
LIB_PAPI  =
LIB_MATH  =
LIB_MPI   = 
LIB_MPE   =
LIB_HYPRE = -L$(HYPRE_PATH)/lib -lHYPRE
LIB_AMREX = -L${AMREX_PATH}/lib -lamrex -lpthread
LIB_BITTREE =
LIB_STDCXX = -lstdc++
LIB_LAPACK= -llapack -lblas
LIB_MA28 = -L$(MA28_PATH) -lma28
# setup tool presently lists AMReX before Milhoja.  Since Milhoja depends on
# AMReX, we have to manually list AMReX afterward so that the linker finds
# the dependencies.
LIB_MILHOJA = -L${MILHOJA_PATH}/lib -lmilhoja -lpthread
LIB_MILHOJA_AMREX = -L${MILHOJA_PATH}/lib -lmilhoja -lpthread ${LIB_AMREX}

# Uncomment the following line to use electic fence memory debugger.
# Need the following environmental variable (see env.sh):
# export EF_ALLOW_MALLOC_0=1
#CONFIG_LIB = -L/usr/lib64 -lefence

#----------------------------------------------------------------------------
# Additional machine-dependent object files
#
#  Add any machine specific files here -- they will be compiled and linked
#  when Flash-X is built.
#----------------------------------------------------------------------------

MACHOBJ =

#----------------------------------------------------------------------------
# Additional commands
#----------------------------------------------------------------------------

MV = mv -f
AR = ar -r
RM = rm -f
CD = cd
RL = ranlib
ECHO = echo

