module dice_datamode_cplhist_mod

  ! The dice cplhist datamode is only used by UFS currently and does not have
  ! a corresponding entry in the cime_config/stream_defintition.xml file

  use ESMF             , only : ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
  use ESMF             , only : ESMF_TraceRegionEnter, ESMF_TraceRegionExit
  use ESMF             , only : ESMF_State, ESMF_Field
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, cl=>shr_kind_cl
  use shr_const_mod    , only : shr_const_TkFrzsw, shr_const_spval
  use dshr_methods_mod , only : dshr_state_getfldptr, dshr_fldbun_getfldptr, chkerr
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
  use dshr_mod         , only : dshr_restart_read, dshr_restart_write
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer

  implicit none
  private

  public  :: dice_datamode_cplhist_advertise
  public  :: dice_datamode_cplhist_init_pointers
  public  :: dice_datamode_cplhist_advance
  public  :: dice_datamode_cplhist_restart_read
  public  :: dice_datamode_cplhist_restart_write

  ! ice to atm in CMEPS/mediator/esmFldsExchange_ufs_mod.F90

  ! export field pointers
  real(r8), pointer :: Si_ifrac(:)  => null()
  real(r8), pointer :: Si_imask(:)  => null()
  real(r8), pointer :: Faii_taux(:) => null()
  real(r8), pointer :: Faii_tauy(:) => null()
  real(r8), pointer :: Faii_lat(:)  => null()
  real(r8), pointer :: Faii_sen(:)  => null()
  real(r8), pointer :: Faii_lwup(:) => null()
  real(r8), pointer :: Faii_evap(:) => null()
  real(r8), pointer :: Si_vice(:)   => null()
  real(r8), pointer :: Si_vsno(:)   => null()
  real(r8), pointer :: Si_t(:)      => null()
  real(r8), pointer :: Si_avsdr(:)  => null()
  real(r8), pointer :: Si_avsdf(:)  => null()
  real(r8), pointer :: Si_anidr(:)  => null()
  real(r8), pointer :: Si_anidf(:)  => null()

  ! stream field pointers
  real(r8), pointer :: strm_Si_ifrac(:)  => null()
  real(r8), pointer :: strm_Si_imask(:)  => null()
  real(r8), pointer :: strm_Faii_taux(:) => null()
  real(r8), pointer :: strm_Faii_tauy(:) => null()
  real(r8), pointer :: strm_Faii_lat(:)  => null()
  real(r8), pointer :: strm_Faii_sen(:)  => null()
  real(r8), pointer :: strm_Faii_lwup(:) => null()
  real(r8), pointer :: strm_Faii_evap(:) => null()
  real(r8), pointer :: strm_Si_vice(:)   => null()
  real(r8), pointer :: strm_Si_vsno(:)   => null()
  real(r8), pointer :: strm_Si_t(:)      => null()
  real(r8), pointer :: strm_Si_avsdr(:)  => null()
  real(r8), pointer :: strm_Si_avsdf(:)  => null()
  real(r8), pointer :: strm_Si_anidr(:)  => null()
  real(r8), pointer :: strm_Si_anidf(:)  => null()

  character(len=*) , parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine dice_datamode_cplhist_advertise(exportState, fldsexport, flds_scalar_name, rc)

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
    call dshr_fldList_add(fldsExport, 'Si_ifrac'            )
    call dshr_fldList_add(fldsExport, 'Si_imask'            )
    call dshr_fldList_add(fldsExport, 'Faii_taux'           )
    call dshr_fldList_add(fldsExport, 'Faii_tauy'           )
    call dshr_fldList_add(fldsExport, 'Faii_lat'            )
    call dshr_fldList_add(fldsExport, 'Faii_sen'            )
    call dshr_fldList_add(fldsExport, 'Faii_lwup'           )
    call dshr_fldList_add(fldsExport, 'Faii_evap'           )
    call dshr_fldList_add(fldsExport, 'Si_vice'             )
    call dshr_fldList_add(fldsExport, 'Si_vsno'             )
    call dshr_fldList_add(fldsExport, 'Si_t'                )
    call dshr_fldList_add(fldsExport, 'Si_avsdr'            )
    call dshr_fldList_add(fldsExport, 'Si_avsdf'            )
    call dshr_fldList_add(fldsExport, 'Si_anidr'            )
    call dshr_fldList_add(fldsExport, 'Si_anidf'            )

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(dice_comp_advertise): Fr_ice'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine dice_datamode_cplhist_advertise

  !===============================================================================
  subroutine dice_datamode_cplhist_init_pointers(exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(dice_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! initialize pointers to export fields
    call dshr_state_getfldptr(exportState,'Si_ifrac',fldptr1=Si_ifrac, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState,'Si_imask', fldptr1=Si_imask, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState,'Faii_taux', fldptr1=Faii_taux, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faii_tauy', fldptr1=Faii_tauy, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faii_lat', fldptr1=Faii_lat, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faii_sen', fldptr1=Faii_sen, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faii_lwup', fldptr1=Faii_lwup, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faii_evap', fldptr1=Faii_evap, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Si_vice', fldptr1=Si_vice, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Si_vsno', fldptr1=Si_vsno, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Si_t', fldptr1=Si_t, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Si_avsdr', fldptr1=Si_avsdr, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Si_avsdf', fldptr1=Si_avsdf, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Si_anidr', fldptr1=Si_anidr, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Si_anidf', fldptr1=Si_anidf, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Set required stream pointer fields
    call shr_strdata_get_stream_pointer(sdat,'Si_ifrac', strm_Si_ifrac, &
         errmsg=subname//'ERROR: strm_Si_ifrac must be associated for dice cplhist datamode', rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat,'Si_imask', strm_Si_imask, &
         errmsg=subname//'ERROR: strm_Si_imask must be associated for dice cplhist datamode', rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Set optional stream pointer fields
    call shr_strdata_get_stream_pointer(sdat,'Faii_taux', strm_Faii_taux, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faii_tauy', strm_Faii_tauy, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faii_lat', strm_Faii_lat, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faii_sen', strm_Faii_sen, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faii_lwup', strm_Faii_lwup, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faii_evap', strm_Faii_evap, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Si_vice', strm_Si_vice, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Si_vsno', strm_Si_vsno, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Si_avsdr', strm_Si_avsdr, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Si_avsdf', strm_Si_avsdf, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Si_anidr', strm_Si_anidr, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Si_anidf', strm_Si_anidf, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Si_t', strm_Si_t, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

  end subroutine dice_datamode_cplhist_init_pointers

  !===============================================================================
  subroutine dice_datamode_cplhist_advance(sdat, rc)

    ! input/output variables
    type(shr_strdata_type) , intent(in)    :: sdat
    integer,                 intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(dice_datamode_cplhist_advance): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    Si_imask(:) = strm_Si_imask(:)
    Si_ifrac(:) = strm_Si_ifrac(:)

    if (associated(Faii_taux)) then
       if (associated(strm_Faii_taux)) then
          Faii_taux(:) = strm_Faii_taux(:)
       else
          Faii_taux(:) = shr_const_spval
       end if
    end if
    if (associated(Faii_tauy)) then
       if (associated(strm_Faii_tauy)) then
          Faii_tauy(:) = strm_Faii_tauy(:)
       else
          Faii_tauy(:) = shr_const_spval
       end if
    end if
    if (associated(Faii_lat)) then
       if (associated(strm_Faii_lat)) then
          Faii_lat(:) = strm_Faii_lat(:)
       else
          Faii_lat(:) = shr_const_spval
       end if
    end if
    if (associated(Faii_sen)) then
       if (associated(strm_Faii_sen)) then
          Faii_sen(:) = strm_Faii_sen(:)
       else
          Faii_sen(:) = shr_const_spval
       end if
    end if
    if (associated(Faii_lwup)) then
       if (associated(strm_Faii_lwup)) then
          Faii_lwup(:) = strm_Faii_lwup(:)
       else
          Faii_lwup(:) = shr_const_spval
       end if
    end if
    if (associated(Si_vice)) then
       if (associated(strm_Si_vice)) then
          Si_vice(:) = strm_Si_vice(:)
       else
          Si_vice(:) = shr_const_spval
       end if
    end if
    if (associated(Si_vsno)) then
       if (associated(strm_Si_vsno)) then
          Si_vsno(:) = strm_Si_vsno(:)
       else
          Si_vsno(:) = shr_const_spval
       end if
    end if
    if (associated(Si_avsdr)) then
       if (associated(strm_Si_avsdr)) then
          Si_avsdr(:) = strm_Si_avsdr(:)
       else
          Si_avsdr(:) = shr_const_spval
       end if
    end if
    if (associated(Si_avsdf)) then
       if (associated(strm_Si_avsdf)) then
          Si_avsdf(:) = strm_Si_avsdf(:)
       else
          Si_avsdf(:) = shr_const_spval
       end if
    end if
    if (associated(Si_anidr)) then
       if (associated(strm_Si_anidr)) then
          Si_anidr(:) = strm_Si_anidr(:)
       else
          Si_anidr(:) = shr_const_spval
       end if
    end if
    if (associated(Si_anidf)) then
       if (associated(strm_Si_anidf)) then
          Si_anidf(:) = strm_Si_anidf(:)
       else
          Si_anidf(:) = shr_const_spval
       end if
    end if
    if (associated(Si_t)) then
       if (associated(strm_Si_t)) then
          Si_t(:) = strm_Si_t(:)
       else
          Si_t(:) = shr_const_spval
       end if
    end if

    ! Unit conversions, calculations,....  Where aice=0, Si_t=0K (as
    ! missing value). Interpolation in time between ice that comes or
    ! goes then has issues
    where (Si_t < 10) Si_t = shr_const_TkFrzsw

  end subroutine dice_datamode_cplhist_advance

  !===============================================================================
  subroutine dice_datamode_cplhist_restart_write(rpfile, case_name, inst_suffix, ymd, tod, &
       logunit, my_task, sdat)

    ! input/output variables
    character(len=*)            , intent(in)    :: rpfile
    character(len=*)            , intent(in)    :: case_name
    character(len=*)            , intent(in)    :: inst_suffix
    integer                     , intent(in)    :: ymd       ! model date
    integer                     , intent(in)    :: tod       ! model sec into model date
    integer                     , intent(in)    :: logunit
    integer                     , intent(in)    :: my_task
    type(shr_strdata_type)      , intent(inout) :: sdat

    ! local variables
    integer :: rc
    !-------------------------------------------------------------------------------

    call dshr_restart_write(rpfile, case_name, 'dice', inst_suffix, ymd, tod, &
         logunit, my_task, sdat, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine dice_datamode_cplhist_restart_write

  !===============================================================================
  subroutine dice_datamode_cplhist_restart_read(rest_filem, rpfile, logunit, my_task, mpicom, sdat)

    ! input/output arguments
    character(len=*)            , intent(inout) :: rest_filem
    character(len=*)            , intent(inout) :: rpfile
    integer                     , intent(in)    :: logunit
    integer                     , intent(in)    :: my_task
    integer                     , intent(in)    :: mpicom
    type(shr_strdata_type)      , intent(inout) :: sdat

    ! local variables
    integer :: rc
    !-------------------------------------------------------------------------------

    call dshr_restart_read(rest_filem, rpfile, logunit, my_task, mpicom, sdat, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine dice_datamode_cplhist_restart_read

end module dice_datamode_cplhist_mod
