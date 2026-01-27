module docn_datamode_cplhist_mod

  use ESMF             , only : ESMF_State, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8
  use shr_log_mod      , only : shr_log_error
  use shr_const_mod    , only : shr_const_TkFrz, shr_const_pi, shr_const_ocn_ref_sal, shr_const_spval
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer

  implicit none
  private

  public  :: docn_datamode_cplhist_advertise
  public  :: docn_datamode_cplhist_init_pointers
  public  :: docn_datamode_cplhist_advance

  ! export field pointers
  real(r8), pointer :: So_omask(:)  => null()    ! real ocean fraction sent to mediator
  real(r8), pointer :: So_t(:)      => null()
  real(r8), pointer :: So_u(:)      => null()
  real(r8), pointer :: So_v(:)      => null()
  real(r8), pointer :: So_bldepth(:) => null()

  ! stream field pointers
  real(r8), pointer :: strm_So_bldepth(:) => null()
  real(r8), pointer :: strm_So_t(:)       => null()
  real(r8), pointer :: strm_So_u(:)       => null()
  real(r8), pointer :: strm_So_v(:)       => null()

  real(r8) , parameter :: tkfrz   = shr_const_tkfrz       ! freezing point, fresh water (kelvin)
  real(r8) , parameter :: ocnsalt = shr_const_ocn_ref_sal ! ocean reference salinity

  character(len=*) , parameter :: nullstr = 'null'
  character(len=*) , parameter :: u_FILE_u = &
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
  subroutine docn_datamode_cplhist_init_pointers(exportState, sdat, ocn_fraction, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    real(r8)               , intent(in)    :: ocn_fraction(:)
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(docn_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! initialize pointers to required export fields
    call dshr_state_getfldptr(exportState, 'So_omask' , fldptr1=So_omask , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_t'     , fldptr1=So_t     , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! initialize pointers to optional export fields
    call dshr_state_getfldptr(exportState, 'So_u'     , fldptr1=So_u     , allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_v'     , fldptr1=So_v     , allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_bldepth', fldptr1=So_bldepth, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! initialize pointers to required stream fields
    call shr_strdata_get_stream_pointer( sdat, 'So_t', strm_So_t, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_So_t must be associated for docn cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! initialize pointers to optional stream fields
    call shr_strdata_get_stream_pointer( sdat, 'So_u', strm_So_u, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'So_v', strm_So_v, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'So_bldepth', strm_So_bldepth, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Allocation depends on exchanged fields, so check before filling arrays with values here
    So_t(:) = TkFrz
    if (associated(So_u)) So_u(:) = 0.0_r8
    if (associated(So_v)) So_v(:) = 0.0_r8
    if (associated(So_bldepth)) So_bldepth(:) = 0.0_r8

    ! Set export state ocean fraction (So_omask)
    So_omask(:) = ocn_fraction(:)

  end subroutine docn_datamode_cplhist_init_pointers

  !===============================================================================
  subroutine docn_datamode_cplhist_advance(sst_constant_value, rc)

    ! input/output variables
    real(r8), optional, intent(in) :: sst_constant_value
    integer, intent(out)   :: rc

    ! local variables
    logical                     :: units_CToK = .true. ! true => convert SST in C to K
    character(len=*), parameter :: subname='(docn_datamode_cplhist_advance): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    if (associated(So_u)) then
       if (associated(strm_So_u)) then
          So_u(:) = strm_So_u(:)
       else
          So_u(:) = shr_const_spval
       end if
    end if
    if (associated(So_v)) then
       if (associated(strm_So_v)) then
          So_v(:) = strm_So_v(:)
       else
          So_v(:) = shr_const_spval
       end if
    end if
    if (associated(So_bldepth)) then
       if (associated(strm_So_bldepth)) then
          So_bldepth(:) = strm_So_bldepth(:)
       else
          So_bldepth(:) = shr_const_spval
       end if
    end if

    ! If need unit conversion for So_t (C-->K),
    ! use existing nml variable sst_constant_value to signal units of input
    ! i.e., 0-->Celsius, 273.15-->K

    if (present(sst_constant_value)) then
      if (sst_constant_value > 230.0_r8) then !interpret input SST in K
        units_CToK = .false. !in K already, don't convert
      endif
    endif

    So_t(:) = strm_So_t(:)
    if (units_CToK) then
      So_t(:) = So_t(:) + TkFrz
    endif

  end subroutine docn_datamode_cplhist_advance

end module docn_datamode_cplhist_mod
