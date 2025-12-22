module datm_datamode_gefs_mod

  use ESMF             , only : ESMF_SUCCESS, ESMF_LogWrite, ESMF_LOGMSG_INFO, ESMF_MAXSTR
  use ESMF             , only : ESMF_State, ESMF_StateGet, ESMF_Field
  use ESMF             , only : ESMF_TraceRegionEnter, ESMF_TraceRegionExit, ESMF_GridCompGet
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_precip_mod   , only : shr_precip_partition_rain_snow_ramp
  use shr_const_mod    , only : shr_const_tkfrz, shr_const_rhofw, shr_const_rdair
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_strdata_mod , only : shr_strdata_type
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
  use dshr_dfield_mod  , only : dfield_type, dshr_dfield_add, dshr_dfield_copy

  implicit none
  private ! except

  public  :: datm_datamode_gefs_advertise
  public  :: datm_datamode_gefs_init_pointers
  public  :: datm_datamode_gefs_advance

  ! export state data
  real(r8), pointer :: Sa_z(:)              => null()
  real(r8), pointer :: Sa_u(:)              => null()
  real(r8), pointer :: Sa_v(:)              => null()
  real(r8), pointer :: Sa_tbot(:)           => null()
  real(r8), pointer :: Sa_shum(:)           => null()
  real(r8), pointer :: Sa_pbot(:)           => null()
  real(r8), pointer :: Sa_u10m(:)           => null()
  real(r8), pointer :: Sa_v10m(:)           => null()
  real(r8), pointer :: Sa_t2m(:)            => null()
  real(r8), pointer :: Sa_q2m(:)            => null()
  real(r8), pointer :: Sa_pslv(:)           => null()
  real(r8), pointer :: Faxa_lwdn(:)         => null()
  real(r8), pointer :: Faxa_rain(:)         => null()
  real(r8), pointer :: Faxa_snow(:)         => null()
  real(r8), pointer :: Faxa_swndr(:)        => null()
  real(r8), pointer :: Faxa_swndf(:)        => null()
  real(r8), pointer :: Faxa_swvdr(:)        => null()
  real(r8), pointer :: Faxa_swvdf(:)        => null()

  ! stream data
  real(r8), pointer :: strm_mask(:)         => null()

  real(r8) :: tbotmax ! units detector
  real(r8) :: maskmax ! units detector

  real(r8) , parameter :: tKFrz    = SHR_CONST_TKFRZ
  real(r8) , parameter :: rdair    = SHR_CONST_RDAIR ! dry air gas constant ~ J/K/kg
  real(r8) , parameter :: rhofw    = SHR_CONST_RHOFW ! density of fresh water ~ kg/m^3

  type(dfield_type)  , pointer :: dfields    => null()

  character(*), parameter :: nullstr = 'undefined'
  character(*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine datm_datamode_gefs_advertise(exportState, fldsexport, flds_scalar_name, rc)

    ! input/output variables
    type(esmf_State)   , intent(inout) :: exportState
    type(fldlist_type) , pointer       :: fldsexport
    character(len=*)   , intent(in)    :: flds_scalar_name
    integer            , intent(out)   :: rc

    ! local variables
    integer                         :: n
    type(fldlist_type), pointer :: fldList
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
    call dshr_fldList_add(fldsExport, 'Sa_z'       )
    call dshr_fldList_add(fldsExport, 'Sa_u'       )
    call dshr_fldList_add(fldsExport, 'Sa_v'       )
    call dshr_fldList_add(fldsExport, 'Sa_tbot'    )
    call dshr_fldList_add(fldsExport, 'Sa_pbot'    )
    call dshr_fldList_add(fldsExport, 'Sa_shum'    )
    call dshr_fldList_add(fldsExport, 'Sa_u10m'    )
    call dshr_fldList_add(fldsExport, 'Sa_v10m'    )
    call dshr_fldList_add(fldsExport, 'Sa_t2m'     )
    call dshr_fldList_add(fldsExport, 'Sa_q2m'     )
    call dshr_fldList_add(fldsExport, 'Sa_pslv'    )
    call dshr_fldList_add(fldsExport, 'Faxa_rain'  )
    call dshr_fldList_add(fldsExport, 'Faxa_snow'  )
    call dshr_fldList_add(fldsExport, 'Faxa_swndr' )
    call dshr_fldList_add(fldsExport, 'Faxa_swvdr' )
    call dshr_fldList_add(fldsExport, 'Faxa_swndf' )
    call dshr_fldList_add(fldsExport, 'Faxa_swvdf' )
    call dshr_fldList_add(fldsExport, 'Faxa_lwdn'  )

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(datm_comp_advertise): Fr_atm '//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine datm_datamode_gefs_advertise

  !===============================================================================
  subroutine datm_datamode_gefs_init_pointers(exportState, sdat, logunit, mainproc, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(in)    :: logunit 
    logical                , intent(in)    :: mainproc
    integer                , intent(out)   :: rc

    ! local variables
    integer                         :: n 
    integer                         :: fieldcount
    type(ESMF_Field)                :: lfield
    character(ESMF_MAXSTR) ,pointer :: lfieldnames(:)
    character(len=*), parameter :: subname='(datm_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Initialize dfields arrays for export fields with no ungridded dimension
    ! and that have a corresponding stream field
    call ESMF_StateGet(exportState, itemCount=fieldCount, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    allocate(lfieldnames(fieldCount))
    call ESMF_StateGet(exportState, itemNameList=lfieldnames, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    do n = 1, fieldCount
       call ESMF_LogWrite(trim(subname)//': field name = '//trim(lfieldnames(n)), ESMF_LOGMSG_INFO)
       call ESMF_StateGet(exportState, itemName=trim(lfieldnames(n)), field=lfield, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       call dshr_dfield_add( dfields, sdat, trim(lfieldnames(n)), trim(lfieldnames(n)), &
            exportState, logunit, mainproc, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end do

    ! initialize pointers for module level stream arrays
    call shr_strdata_get_stream_pointer( sdat, 'Sa_mask'   , strm_mask , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! get export state pointers
    call dshr_state_getfldptr(exportState, 'Sa_z'       , fldptr1=Sa_z       , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_u'       , fldptr1=Sa_u       , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_v'       , fldptr1=Sa_v       , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_tbot'    , fldptr1=Sa_tbot    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_pbot'    , fldptr1=Sa_pbot    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_shum'    , fldptr1=Sa_shum    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_u10m'    , fldptr1=Sa_u10m    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_v10m'    , fldptr1=Sa_v10m    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_t2m'    , fldptr1=Sa_t2m    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_q2m'    , fldptr1=Sa_q2m    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_pslv'    , fldptr1=Sa_pslv    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_rain'  , fldptr1=Faxa_rain  , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_snow' , fldptr1=Faxa_snow, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swvdr' , fldptr1=Faxa_swvdr , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swvdf' , fldptr1=Faxa_swvdf , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swndr' , fldptr1=Faxa_swndr , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swndf' , fldptr1=Faxa_swndf , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_lwdn'  , fldptr1=Faxa_lwdn  , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine datm_datamode_gefs_init_pointers

  !===============================================================================
  subroutine datm_datamode_gefs_advance(exportstate, sdat, mainproc, logunit, mpicom, &
       target_ymd, target_tod, model_calendar, rc)

    use ESMF, only: ESMF_VMGetCurrent, ESMF_VMAllReduce, ESMF_REDUCE_MAX, ESMF_VM

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    logical                , intent(in)    :: mainproc
    integer                , intent(in)    :: logunit
    integer                , intent(in)    :: mpicom
    integer                , intent(in)    :: target_ymd
    integer                , intent(in)    :: target_tod
    character(len=*)       , intent(in)    :: model_calendar
    integer                , intent(out)   :: rc

    ! local variables
    logical  :: first_time = .true.
    integer  :: n                   ! indices
    integer  :: lsize               ! size of attr vect
    real(r8) :: rtmp(2)
    type(ESMF_VM) :: vm
    character(len=*), parameter :: subname='(datm_datamode_gefs_advance): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    lsize = size(strm_mask)

    if (first_time) then
       call ESMF_VMGetCurrent(vm, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       ! determine tbotmax (see below for use)
       rtmp(1) = maxval(Sa_tbot(:))
       call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       tbotmax = rtmp(2)

       if (mainproc) write(logunit,*) trim(subname),' tbotmax = ',tbotmax

       ! determine maskmax (see below for use)
       rtmp(1) = maxval(strm_mask(:))
       call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       maskmax = rtmp(2)
       if (mainproc) write(logunit,*) trim(subname),' maskmax = ',maskmax

       ! reset first_time
       first_time = .false.
    end if

    ! copy all fields from streams to export state as default
    ! This automatically will update the fields in the export state
    call ESMF_TraceRegionEnter('datm_gefs_dfield_copy')
    call dshr_dfield_copy(dfields, sdat, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TraceRegionExit('datm_gefs_dfield_copy')

    do n = 1, lsize
       !--- temperature ---
       if (tbotmax < 50.0_r8) Sa_tbot(n) = Sa_tbot(n) + tkFrz
       ! Limit very cold forcing to 180K
       Sa_tbot(n) = max(180._r8, Sa_tbot(n))
    end do

  end subroutine datm_datamode_gefs_advance

end module datm_datamode_gefs_mod
