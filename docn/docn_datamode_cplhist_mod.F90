module docn_datamode_cplhist_mod

  use ESMF             , only : ESMF_State, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_const_mod    , only : shr_const_TkFrz, shr_const_pi, shr_const_ocn_ref_sal
  use dshr_methods_mod , only : dshr_state_getfldptr, dshr_fldbun_getfldptr, chkerr
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
  use dshr_strdata_mod , only : shr_strdata_type

  implicit none
  private ! except

  public  :: docn_datamode_cplhist_advertise
  public  :: docn_datamode_cplhist_init_pointers
  public  :: docn_datamode_cplhist_advance

  ! export fields
  real(r8), pointer :: So_omask(:)  => null()    ! real ocean fraction sent to mediator
  real(r8), pointer :: So_t(:)      => null()
  real(r8), pointer :: So_u(:)      => null()
  real(r8), pointer :: So_v(:)      => null()
  real(r8), pointer :: So_bldepth(:) => null()

  real(r8) , parameter :: tkfrz   = shr_const_tkfrz       ! freezing point, fresh water (kelvin)
  real(r8) , parameter :: ocnsalt = shr_const_ocn_ref_sal ! ocean reference salinity

  character(*) , parameter :: nullstr = 'null'
  character(*) , parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine docn_datamode_cplhist_advertise(exportState, fldsexport, flds_scalar_name, rc)

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
    call dshr_fldList_add(fldsExport, 'So_omask'            )
    call dshr_fldList_add(fldsExport, 'So_t'                )
    call dshr_fldList_add(fldsExport, 'So_u'                )
    call dshr_fldList_add(fldsExport, 'So_v'                )
    call dshr_fldList_add(fldsExport, 'So_bldepth'          )

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(docn_comp_advertise): Fr_ocn'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine docn_datamode_cplhist_advertise

  !===============================================================================
  subroutine docn_datamode_cplhist_init_pointers(exportState, ocn_fraction, rc)

    ! input/output variables
    type(ESMF_State) , intent(inout) :: exportState
    real(r8)         , intent(in)    :: ocn_fraction(:)
    integer          , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(docn_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! initialize pointers to export fields
    call dshr_state_getfldptr(exportState, 'So_omask' , fldptr1=So_omask , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_t'     , fldptr1=So_t     , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_u'     , fldptr1=So_u     , allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_v'     , fldptr1=So_v     , allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_bldepth', fldptr1=So_bldepth, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    So_u(:) = 0.0_r8
    So_v(:) = 0.0_r8
    So_t(:) = TkFrz
    So_bldepth(:) = 0.0_r8

    ! Set export state ocean fraction (So_omask)
    So_omask(:) = ocn_fraction(:)

  end subroutine docn_datamode_cplhist_init_pointers

  !===============================================================================
  subroutine docn_datamode_cplhist_advance(rc)

    ! input/output variables
    integer, intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(docn_datamode_cplhist_advance): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    So_t(:) = So_t(:) + TkFrz

  end subroutine docn_datamode_cplhist_advance

end module docn_datamode_cplhist_mod
