! !MODULE: shr_file_mod.F90 --- Module to handle various file utilily functions.
!
! !DESCRIPTION:
! A minimal copy of shr_file_mod from cesm, only shr_file_setlogunit is needed
!
! !REVISION HISTORY:
!   2006-05-08 E. Kluzek, Add in shr_file_mod and getUnit, freeUnif methods.
!   2000-??-?? B. Kauffman, original version circa 2000
!
! !INTERFACE: ------------------------------------------------------------------

MODULE shr_file_mod

  ! !USES:

  use shr_kind_mod, only : shr_kind_in
  use shr_log_mod, only: s_logunit => shr_log_Unit

  IMPLICIT none

  PRIVATE           ! By default everything is private to this module

  ! !PUBLIC TYPES:

  ! no public types

  ! !PUBLIC MEMBER FUNCTIONS:

  public :: shr_file_setLogUnit   ! Reset the log unit number
  !===============================================================================
CONTAINS
  !===============================================================================
  !BOP ===========================================================================
  !
  ! !IROUTINE: shr_file_setLogUnit -- Set the Log I/O Unit number
  !
  ! !INTERFACE: ------------------------------------------------------------------

  SUBROUTINE shr_file_setLogUnit(unit)

    implicit none

    ! !INPUT/OUTPUT PARAMETERS:

    integer(SHR_KIND_IN),intent(in) :: unit     ! new unit number

    !EOP

    !--- formats ---
    character(*),parameter :: subName = '(shr_file_setLogUnit) '
    character(*),parameter :: F00   = "('(shr_file_setLogUnit) ',4a)"

    !-------------------------------------------------------------------------------
    ! Notes: Caller must be sure it's a valid unit number
    !-------------------------------------------------------------------------------
#if DEBUG
    if (s_loglev > 2 .and. s_logunit-unit /= 0) then
       write(s_logunit,*) subName,': reset log unit number from/to ',s_logunit, unit
       write(     unit,*) subName,': reset log unit number from/to ',s_logunit, unit
    endif
#endif
    s_logunit = unit

  END SUBROUTINE shr_file_setLogUnit

END MODULE shr_file_mod
