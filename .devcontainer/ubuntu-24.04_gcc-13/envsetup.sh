#!/bin/bash

# Spack setup
. ${SPACK_ROOT}/share/spack/setup-env.sh
spack load openmpi
spack load esmf
spack load hdf5
spack load netcdf-c
spack load netcdf-fortran
spack load parallelio
spack load parallel-netcdf

# CDEPS environment variables
export CC=mpicc
export FC=mpifort
export CXX=mpicxx
export CPPFLAGS="-I/usr/include -I/usr/local/include"
export LDFLAGS="-L/usr/lib/$(dpkg-architecture -qDEB_HOST_MULTIARCH)"
export FFLAGS="-DCPRGNU -g -Wall -ffree-form -ffree-line-length-none -fallow-argument-mismatch"
export ESMF_VERSION=v$(spack find --format "{version}" esmf)
export ParallelIO_VERSION=pio$(spack find --format "{version}" parallelio)
export PIO=$PIO_ROOT
export CDEPS_CMAKE_FLAGS="-Wno-dev -DCMAKE_BUILD_TYPE=DEBUG -DWERROR=ON"

# Print Welcome Message
echo "Welcome to the CDEPS Development Container!"
echo "*** ${DEVCONTAINER_NAME} ***"
echo ""
echo "The following packages have been pre-loaded:"
spack find --loaded --format "{name}@{version}"
echo ""
echo "Build CDEPS using the following steps:"
echo "1. Create directory: mkdir -p <build_directory> && cd <build_directory>"
echo "2. Generate build files: cmake <source_directory> \${CDEPS_CMAKE_FLAGS}"
echo "3. Build CDEPS: make -j 4 VERBOSE=1"
echo ""
