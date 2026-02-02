module docn_datamode_multilev_cplhist_mod

  use ESMF             , only : ESMF_State, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_log_mod      , only : shr_log_error
  use shr_const_mod    , only : shr_const_TkFrz, shr_const_pi, shr_const_ocn_ref_sal, shr_const_spval
  use shr_sys_mod      , only : shr_sys_abort
  use dshr_methods_mod , only : dshr_state_getfldptr, dshr_fldbun_getfldptr, chkerr
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
  use dshr_strdata_mod , only : shr_strdata_get_stream_pointer, shr_strdata_type

  implicit none
  private

  public :: docn_datamode_multilev_cplhist_advertise
  public :: docn_datamode_multilev_cplhist_init_pointers
  public :: docn_datamode_multilev_cplhist_advance

  ! export state pointers
  real(r8), pointer :: So_omask(:)     => null()    ! real ocean fraction sent to mediator
  real(r8), pointer :: So_t_depth(:,:) => null()
  real(r8), pointer :: So_s_depth(:,:) => null()

  ! stream field pointers
  type, public :: stream_pointer_type
     real(r8), pointer :: ptr(:)
  end type stream_pointer_type
  type(stream_pointer_type), allocatable :: strm_So_t_depth(:)
  type(stream_pointer_type), allocatable :: strm_So_s_depth(:)

  ! number of multi-level ocean fields
  integer, parameter :: nlev_export = 30

  character(len=*) , parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine docn_datamode_multilev_cplhist_advertise(exportState, fldsexport, flds_scalar_name, rc)

    ! input/output variables
    type(esmf_State)   , intent(inout) :: exportState
    type(fldlist_type) , pointer       :: fldsexport
    character(len=*)   , intent(in)    :: flds_scalar_name
    integer            , intent(out)   :: rc

    ! local variables
    type(fldlist_type), pointer :: fldList
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Advertise export fields
    call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
    call dshr_fldList_add(fldsExport, 'So_omask')
    call dshr_fldList_add(fldsExport, 'So_t_depth', ungridded_lbound=1, ungridded_ubound=nlev_export)
    call dshr_fldList_add(fldsExport, 'So_s_depth', ungridded_lbound=1, ungridded_ubound=nlev_export)

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(docn_comp_advertise): Fr_ocn'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine docn_datamode_multilev_cplhist_advertise

  !===============================================================================
  subroutine docn_datamode_multilev_cplhist_init_pointers(exportState, sdat, ocn_fraction, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    real(r8)               , intent(in)    :: ocn_fraction(:)
    integer                , intent(out)   :: rc

    ! local variables
    integer          :: ilev
    character(len=2) :: num_str
    character(CS)    :: strm_fld
    character(len=*), parameter :: subname='(docn_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! We are assuming a different stream field for each vertical level,
    ! whereas export field contains an ungridded dimension for each vertical level

    ! Set export state pointers
    call dshr_state_getfldptr(exportState, fldname='So_omask', fldptr1=So_omask, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='So_t_depth', fldptr2=So_t_depth, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='So_s_depth', fldptr2=So_s_depth, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Set stream field pointers
    allocate(strm_So_t_depth(1:nlev_export))
    allocate(strm_So_s_depth(1:nlev_export))
    do ilev = 1,nlev_export
       write(num_str, '(i0)') ilev
       strm_fld = 'So_t_depth' // trim(num_str)
       call shr_strdata_get_stream_pointer( sdat, trim(strm_fld), strm_So_t_depth(ilev)%ptr, &
            requirePointer=.true., &
            errmsg=subname//'ERROR: '//trim(strm_fld)//&
            ' must be associated for docn multiplev_cplhist datamode', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       strm_fld = 'So_s_depth' // trim(num_str)
       call shr_strdata_get_stream_pointer( sdat, trim(strm_fld), strm_So_s_depth(ilev)%ptr, &
            requirePointer=.true., &
            errmsg=subname//'ERROR: '//trim(strm_fld)//&
            ' must be associated for docn multiplev_cplhist datamode', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end do

    ! Set export state ocean fraction (So_omask)
    So_omask(:) = ocn_fraction(:)

  end subroutine docn_datamode_multilev_cplhist_init_pointers

  !===============================================================================
  subroutine docn_datamode_multilev_cplhist_advance(exportState, rc)

    ! input/output variables
    type(ESMF_State) , intent(inout) :: exportState
    integer          , intent(out)   :: rc

    ! local variables
    integer :: ilev,ig
    character(len=*), parameter :: subname='(docn_datamode_multilev): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    do ilev = 1,size(So_t_depth,dim=1)
       do ig = 1,size(So_t_depth,dim=2)
          if (strm_So_t_depth(ilev)%ptr(ig) == 0._r8) then
             So_t_depth(ilev,ig) = 1.e30_r8
          else
             So_t_depth(ilev,ig) = strm_So_t_depth(ilev)%ptr(ig)
          end if
       end do
    end do

    do ilev = 1,size(So_s_depth,dim=1)
       do ig = 1,size(So_s_depth,dim=2)
          if (strm_So_s_depth(ilev)%ptr(ig) == 0._r8) then
             So_s_depth(ilev,ig) = 1.e30_r8
          else
             So_s_depth(ilev,ig) = strm_So_s_depth(ilev)%ptr(ig)
          end if
       end do
    end do

  end subroutine docn_datamode_multilev_cplhist_advance

end module docn_datamode_multilev_cplhist_mod
