# - Try to find FoX
#
# This can be controled by setting FoX_PATH or FoX_<lang>_PATH Cmake variables,
# where <lang> is the COMPONENT language one needs.
#
# Once done, this will define:
#
#  FoX_<lang>_FOUND   (BOOL) - system has FoX
#  FoX_<lang>_IS_SHARED (BOOL) - whether the library is shared/dynamic
#  FoX_<lang>_INCLUDE_DIR (PATH) - Location of the header files and modules
#  FoX_<lang>_LIBRARY     (File) - Path to the <lang> library files
#  FoX_<lang>_LIBRARIES   (List) - link these to use FoX
#
# Available COMPONENTS are: Fortran
#
include (LibFind)
include (LibCheck)

# Define FoX Fortran Component
define_package_component(FoX DEFAULT
                         COMPONENT Fortran
                         INCLUDE_NAMES fox_common.mod
                         LIBRARY_NAMES FoX_common)

# Search for list of valid components requested
find_valid_components(FoX)

#==============================================================================
# SEARCH FOR VALIDATED COMPONENTS
foreach (pcomp IN LISTS FoX_FIND_VALID_COMPONENTS)
    set(FoX_Fortran_INCLUDE_DIR $ENV{FoX}/finclude)
    set(FoX_Fortran_LIBRARY $ENV{FoX}/lib)
    initialize_paths(FoX_${pcomp}_PATHS
                     INCLUDE_DIRECTORIES $ENV{FoX}/finclude
		     LIBRARIES $ENV{FoX}/lib)
    find_package_component(FoX COMPONENT ${pcomp})
endforeach ()
if(FoX_Fortran_FOUND)
message(" Found FoX: ${FoX_Fortran_INCLUDE_DIR}")
endif()