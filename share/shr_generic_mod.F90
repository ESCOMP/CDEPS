module shr_generic_mod

  use ESMF
  use NUOPC
  use shr_strdata_mod, only : shr_strdata_type, shr_strdata_perstream, &
                               shr_strdata_get_stream_count, shr_strdata_get_stream, &
                               shr_strdata_advance, shr_strdata_get_stream_pointer
  use dshr_state_mod,  only : dshr_state_getfldptr

  implicit none
  private

  public :: datamode_generic_advertise
  public :: datamode_generic_advance

contains

  ! =======================================================================
  ! ROUTINE: datamode_generic_advertise
  ! PURPOSE: Dynamically loop through the stream configuration (XML or ESMF)
  !          and advertise exactly the fields the user explicitly requested.
  ! =======================================================================
  subroutine datamode_generic_advertise(exportState, sdat, rc)
    type(ESMF_State),       intent(inout) :: exportState
    type(shr_strdata_type), intent(inout) :: sdat
    integer,                intent(out), optional  :: rc

    integer :: n, i, num_streams
    character(len=ESMF_MAXSTR) :: fieldName
    type(shr_strdata_perstream), pointer :: stream_ptr

    if (present(rc)) rc = ESMF_SUCCESS

    ! Retrieve the total number of streams defined in the user's config
    num_streams = shr_strdata_get_stream_count(sdat)

    do n = 1, num_streams
       ! Get the pointer to the specific stream object
       stream_ptr => shr_strdata_get_stream(sdat, n)
       
       ! Loop over the array of model field names parsed from the stream config
       if (associated(stream_ptr%fldlist_model)) then
          do i = 1, size(stream_ptr%fldlist_model)
             fieldName = trim(stream_ptr%fldlist_model(i))
             
             call NUOPC_StateAdvertise(exportState, StandardName=trim(fieldName), rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU)) return
          end do
       endif
    end do

  end subroutine datamode_generic_advertise

  ! =======================================================================
  ! ROUTINE: datamode_generic_advance
  ! PURPOSE: Advances the stream reader, interpolates data in time, and 
  !          dynamically copies data from the internal stream buffers 
  !          directly to the NUOPC export state with ZERO math or physics.
  ! =======================================================================
  subroutine datamode_generic_advance(exportState, sdat, year, month, day, sec, rc)
    type(ESMF_State),       intent(inout) :: exportState
    type(shr_strdata_type), intent(inout) :: sdat
    integer,                intent(in)    :: year, month, day, sec
    integer,                intent(out), optional  :: rc

    integer :: n, i, num_streams
    character(len=ESMF_MAXSTR) :: fieldName
    type(shr_strdata_perstream), pointer :: stream_ptr
    
    ! Pointers for the explicit pass-through copy
    real(ESMF_KIND_R8), pointer :: strm_ptr(:)
    real(ESMF_KIND_R8), pointer :: state_ptr(:)

    if (present(rc)) rc = ESMF_SUCCESS

    ! 1. Advance the stream (Handles NetCDF I/O and time interpolation)
    call shr_strdata_advance(sdat, year, month, day, sec, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU)) return

    ! 2. Dynamically fetch pointers and copy data to the NUOPC Export State
    num_streams = shr_strdata_get_stream_count(sdat)

    do n = 1, num_streams
       stream_ptr => shr_strdata_get_stream(sdat, n)
       
       if (associated(stream_ptr%fldlist_model)) then
          do i = 1, size(stream_ptr%fldlist_model)
             fieldName = trim(stream_ptr%fldlist_model(i))
             
             ! Nullify pointers to ensure clean association
             strm_ptr => null()
             state_ptr => null()

             ! Fetch pointer to the internal shr_strdata memory buffer
             call shr_strdata_get_stream_pointer(sdat, fieldName, strm_ptr, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU)) return

             ! Fetch pointer to the NUOPC Export State ESMF_Field memory
             call dshr_state_getfldptr(exportState, fieldName, fldptr1=state_ptr, &
                                       allowNullReturn=.true., rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU)) return

             ! 3. The Core Pass-Through Logic: Direct Copy
             ! If CMEPS accepted the field, the state pointer will be associated.
             if (associated(strm_ptr) .and. associated(state_ptr)) then
                state_ptr(:) = strm_ptr(:)
             endif
             
          end do
       endif
    end do

  end subroutine datamode_generic_advance

end module shr_generic_mod
