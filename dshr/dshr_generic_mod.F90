module dshr_generic_mod

  use ESMF            , only : ESMF_SUCCESS, ESMF_State, &
                               ESMF_LogWrite, ESMF_LOGMSG_INFO, ESMF_LOGMSG_WARNING
  use NUOPC           , only : NUOPC_Advertise
  use shr_kind_mod    , only : r8=>shr_kind_r8, cl=>shr_kind_cl
  use dshr_fldlist_mod, only : fldlist_type, dshr_fldlist_add
  use dshr_strdata_mod, only : shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_methods_mod, only : dshr_state_getfldptr, chkerr

  implicit none
  private

  public :: datamode_generic_advertise
  public :: datamode_generic_init_pointers
  public :: datamode_generic_advance
  public :: datamode_generic_clean

  ! -----------------------------------------------------------------------
  ! Derived type to cache the pointer pairs dynamically for the Advance loop
  ! -----------------------------------------------------------------------
  type :: ptr_map_type
     real(r8), pointer :: strm_ptr(:)  => null()
     real(r8), pointer :: state_ptr(:) => null()
  end type ptr_map_type

  ! Module-level cache array
  type(ptr_map_type), allocatable :: ptr_cache(:)

  character(len=*) , parameter :: u_FILE_u = &
       __FILE__

contains

  ! =======================================================================
  subroutine datamode_generic_advertise(exportState, fldsExport, sdat, flds_scalar_name, rc)
    type(ESMF_State)      , intent(inout) :: exportState
    type(fldList_type),     pointer       :: fldsExport
    type(shr_strdata_type), intent(in)    :: sdat
    character(len=*)      , intent(in)    :: flds_scalar_name
    integer               , intent(out)   :: rc

    integer :: i, n
    character(len=CL) :: fieldName
    type(fldlist_type), pointer :: fldList

    if (present(rc)) rc = ESMF_SUCCESS

    ! Natively access the array of stream objects inside sdat
    if (associated(sdat%stream)) then
       do i = 1, size(sdat%stream)
          if (sdat%stream(i)%nvars > 0 .and. allocated(sdat%stream(i)%varlist)) then
             
             ! Extract the 'nameinmodel' string directly from the varlist
             do n = 1, sdat%stream(i)%nvars
                fieldName = trim(sdat%stream(i)%varlist(n)%nameinmodel)
                
                ! Add to the CDEPS list (which handles the NUOPC advertise)
                call dshr_fldlist_add(fldsExport, trim(fieldName))
             end do
             
          endif
       end do
    endif
    ! Handle cpl_scalars
    call dshr_fldList_add(fldsExport, trim(flds_scalar_name))

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(datm_generic_advertise): '//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine datamode_generic_advertise

  ! =======================================================================
  subroutine datamode_generic_init_pointers(exportState, sdat, rc)
    type(ESMF_State),       intent(inout) :: exportState
    type(shr_strdata_type), intent(in)    :: sdat
    integer,                intent(out)   :: rc

    integer :: i, n, total_vars, cache_idx
    character(len=CL) :: fieldName
    character(len=CL) :: logMsg

    rc = ESMF_SUCCESS

    ! 1. Count the total number of fields to allocate the cache
    total_vars = 0
    if (associated(sdat%stream)) then
       do i = 1, size(sdat%stream)
          if (allocated(sdat%stream(i)%varlist)) then
             total_vars = total_vars + sdat%stream(i)%nvars
          endif
       end do
    endif

    ! Allocate the module-level pointer cache
    if (allocated(ptr_cache)) deallocate(ptr_cache)
    if (total_vars > 0) allocate(ptr_cache(total_vars))

    ! Populate the cache and log fieldName diagnostics
    cache_idx = 1
    if (associated(sdat%stream)) then
       do i = 1, size(sdat%stream)
          if (allocated(sdat%stream(i)%varlist)) then
             do n = 1, sdat%stream(i)%nvars
                fieldName = trim(sdat%stream(i)%varlist(n)%nameinmodel)
                
                ! Look up stream array pointer
                call shr_strdata_get_stream_pointer(sdat, fieldName, &
                                                    ptr_cache(cache_idx)%strm_ptr, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                ! Look up NUOPC export array pointer (allow null if CMEPS didn't connect it)
                call dshr_state_getfldptr(exportState, fieldName, &
                                          fldptr1=ptr_cache(cache_idx)%state_ptr, &
                                          allowNullReturn=.true., rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                ! Diagnostic Logging
                if (.not. associated(ptr_cache(cache_idx)%state_ptr)) then
                   write(logMsg, '(A,A,A)') "GENERIC Datamode INFO: field '", trim(fieldName), &
                                            "' ignored. Not requested by CMEPS mediator."
                   call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
                endif
                
                if (.not. associated(ptr_cache(cache_idx)%strm_ptr)) then
                   write(logMsg, '(A,A,A)') "GENERIC Datamode WARNING: field '", trim(fieldName), &
                                            "' missing from internal stream buffer."
                   call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_WARNING)
                endif

                cache_idx = cache_idx + 1
             end do
          endif
       end do
    endif

  end subroutine datamode_generic_init_pointers


  ! =======================================================================
  subroutine datamode_generic_advance(rc)
    integer, intent(out), optional :: rc

    integer :: i

    if (present(rc)) rc = ESMF_SUCCESS

    if (allocated(ptr_cache)) then
       do i = 1, size(ptr_cache)
          if (associated(ptr_cache(i)%strm_ptr) .and. associated(ptr_cache(i)%state_ptr)) then
             ptr_cache(i)%state_ptr(:) = ptr_cache(i)%strm_ptr(:)
          endif
       end do
    endif

  end subroutine datamode_generic_advance

  ! =======================================================================
  subroutine datamode_generic_clean(rc)
    integer, intent(out), optional :: rc

    if (present(rc)) rc = ESMF_SUCCESS

    if (allocated(ptr_cache)) then
       deallocate(ptr_cache)
    endif

  end subroutine datamode_generic_clean

end module dshr_generic_mod
