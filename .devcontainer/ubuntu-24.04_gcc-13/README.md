 # CDEPS Ubuntu 24.04 Development Container

This Dev Container provides a reproducible CDEPS development environment based
on Ubuntu 24.04, GCC / GFortran 13, an MPI library, and other packages needed
to build CDEPS.

## Prerequisites

- Docker or another Docker-compatible container runtime
- Visual Studio Code
- The **Dev Containers** extension for Visual Studio Code

Open the CDEPS repository in Visual Studio Code and run **Dev Containers:
Reopen in Container**. The image is built from `Dockerfile` in this directory,
and the repository is mounted at `/home/cdepsdev/CDEPS`.

## Installed Toolchain

The image includes the following build tools and packages:

| Software | Version |
| --- | --- |
| GCC / G++ / GNU Fortran | 13 |
| Spack | 1.2.1 |
| OpenMPI | 4.1.6 |
| ESMF | 8.9.0 |
| HDF5 | 1.14.6 |
| NetCDF-C | 4.10.0 |
| NetCDF-Fortran | 4.6.2 |
| ParallelIO | 2.6.6 |
| Parallel-netCDF | 1.14.1 |

Spack is installed at `/home/cdepsdev/spack`.

## Environment

The bash login shell sources `.envsetup.sh` automatically. This initializes
Spack and loads the software stack, then sets environment variables needed
for the CDEPS build:

- CC
- FC
- CXX
- CPPFLAGS
- LDFLAGS
- FFLAGS
- ESMF_VERSION
- ParallelIO_VERSION
- PIO
- CDEPS_CMAKE_FLAGS

To reinitialize the environment in an existing shell, run:

```bash
source ~/.envsetup.sh
```

## Build CDEPS

Use an out-of-source build directory. From the repository root inside the
container:

Replace CDEPS_BUILD_DIRECTORY and CDEPS_SOURCE_DIRECTORY with their
respective locations.

```bash
mkdir -p CDEPS_BUILD_DIRECTORY
cd CDEPS_BUILD_DIRECTORY
cmake CDEPS_SOURCE_DIRECTORY ${CDEPS_CMAKE_FLAGS}
make -j4 VERBOSE=1
```

To disable the bundled FoX XML library, configure with:

```bash
cmake /home/cdepsdev/CDEPS ${CDEPS_CMAKE_FLAGS} -DDISABLE_FoX=ON
```