module datm_datamode_cfsr_mod

  use ESMF
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_sys_mod      , only : shr_sys_abort
  use shr_precip_mod   , only : shr_precip_partition_rain_snow_ramp
  use shr_mpi_mod      , only : shr_mpi_max
  use shr_const_mod    , only : shr_const_tkfrz, shr_const_rhofw, shr_const_rdair
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_mod         , only : dshr_restart_read, dshr_restart_write
  use dshr_strdata_mod , only : shr_strdata_type
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add

  implicit none
  private ! except

  public  :: datm_datamode_cfsr_advertise
  public  :: datm_datamode_cfsr_init_pointers
  public  :: datm_datamode_cfsr_advance
  public  :: datm_datamode_cfsr_restart_write
  public  :: datm_datamode_cfsr_restart_read
!  private :: datm_eSat  ! determine saturation vapor pressure

  ! export state data
  real(r8), pointer :: Sa_z(:)              => null()
  real(r8), pointer :: Sa_u(:)              => null()
  real(r8), pointer :: Sa_v(:)              => null()
  real(r8), pointer :: Sa_tbot(:)           => null()
  real(r8), pointer :: Sa_shum(:)           => null()
  real(r8), pointer :: Sa_pbot(:)           => null()
  real(r8), pointer :: Sa_u10m(:)           => null()
  real(r8), pointer :: Sa_v10m(:)           => null()
  real(r8), pointer :: Sa_t2m(:)           => null()
  real(r8), pointer :: Sa_q2m(:)           => null()
  real(r8), pointer :: Sa_pslv(:)           => null()
  real(r8), pointer :: Faxa_lwdn(:)         => null()
  real(r8), pointer :: Faxa_rain(:)         => null()
  real(r8), pointer :: Faxa_snow(:)        => null()
  real(r8), pointer :: Faxa_swndr(:)        => null()
  real(r8), pointer :: Faxa_swndf(:)        => null()
  real(r8), pointer :: Faxa_swvdr(:)        => null()
  real(r8), pointer :: Faxa_swvdf(:)        => null()

  ! stream data
  real(r8), pointer :: strm_mask(:)      => null()

  real(r8) :: tbotmax ! units detector
  real(r8) :: maskmax ! units detector

  real(r8) , parameter :: tKFrz    = SHR_CONST_TKFRZ
  real(r8) , parameter :: rdair    = SHR_CONST_RDAIR ! dry air gas constant ~ J/K/kg
  real(r8) , parameter :: rhofw    = SHR_CONST_RHOFW ! density of fresh water ~ kg/m^3
  
  character(*), parameter :: nullstr = 'undefined'
  character(*), parameter :: rpfile  = 'rpointer.atm'
  character(*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine datm_datamode_cfsr_advertise(exportState, fldsexport, &
       flds_scalar_name, rc)

    ! input/output variables
    type(esmf_State)   , intent(inout) :: exportState
    type(fldlist_type) , pointer       :: fldsexport
    character(len=*)   , intent(in)    :: flds_scalar_name
    integer            , intent(out)   :: rc

    ! local variables
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
    call dshr_fldList_add(fldsExport, 'Sa_t2m'    )
    call dshr_fldList_add(fldsExport, 'Sa_q2m'    )
    call dshr_fldList_add(fldsExport, 'Sa_pslv'    )
    call dshr_fldList_add(fldsExport, 'Faxa_rain'  )
    call dshr_fldList_add(fldsExport, 'Faxa_snow' )
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

  end subroutine datm_datamode_cfsr_advertise

  !===============================================================================
  subroutine datm_datamode_cfsr_init_pointers(exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(datm_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

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

  end subroutine datm_datamode_cfsr_init_pointers

  !===============================================================================
  subroutine datm_datamode_cfsr_advance(exportstate, masterproc, logunit, mpicom, target_ymd, target_tod, model_calendar, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    logical                , intent(in)    :: masterproc
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
    real(r8) :: rtmp
    real(r8) :: tbot, pbot
!    real(r8) :: vp
    real(r8) :: e, qsat
    character(len=*), parameter :: subname='(datm_datamode_cfsr_advance): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    lsize = size(strm_mask)

    if (first_time) then
       ! determine tbotmax (see below for use)
       rtmp = maxval(Sa_tbot(:))
       call shr_mpi_max(rtmp, tbotmax, mpicom, 'datm_tbot', all=.true.)
       if (masterproc) write(logunit,*) trim(subname),' tbotmax = ',tbotmax

       ! determine maskmax (see below for use)
       rtmp = maxval(strm_mask(:))
       call shr_mpi_max(rtmp, maskmax, mpicom, 'datm_mask', all=.true.)
       if (masterproc) write(logunit,*) trim(subname),' maskmax = ',maskmax

       ! reset first_time
       first_time = .false.
    end if

    do n = 1, lsize
       !--- bottom layer height ---
!       Sa_z(n) = 10.0_r8

       !--- calculate wind speed ---
!       if (associated(Sa_wspd)) then
!         Sa_wspd(n) = sqrt(Sa_u(n)*Sa_u(n)+Sa_v(n)*Sa_v(n)) 
!       end if

       !--- temperature ---
       if (tbotmax < 50.0_r8) Sa_tbot(n) = Sa_tbot(n) + tkFrz
       ! Limit very cold forcing to 180K
       Sa_tbot(n) = max(180._r8, Sa_tbot(n))
!       Sa_ptem(n) = Sa_tbot(n)
    end do

    !----------------------------------------------------------
    ! unit conversions (temporal resolution is hourly)
    !----------------------------------------------------------

    ! convert J/m^2 to W/m^2
    !Faxa_lwdn(:) = Faxa_lwdn(:)/3600.0_r8
!    Faxa_lwdn(:) = Faxa_lwdn(:)/21600.0_r8
!    if (associated(Faxa_lwnet)) then
!      Faxa_lwnet(:) = Faxa_lwnet(:)/3600.0_r8
!    end if
!    Faxa_swdn(:) = Faxa_swdn(:)/3600.0_r8
!    Faxa_swdn(:) = Faxa_swdn(:)/21600.0_r8
    !Faxa_swvdr(:) = Faxa_swvdr(:)/3600.0_r8
    !Faxa_swndr(:) = Faxa_swndr(:)/3600.0_r8
    !Faxa_swvdf(:) = Faxa_swvdf(:)/3600.0_r8
    !Faxa_swndf(:) = Faxa_swndf(:)/3600.0_r8
!    Faxa_swvdr(:) = Faxa_swvdr(:)/21600.0_r8
!    Faxa_swndr(:) = Faxa_swndr(:)/21600.0_r8
!    Faxa_swvdf(:) = Faxa_swvdf(:)/21600.0_r8
!    Faxa_swndf(:) = Faxa_swndf(:)/21600.0_r8
!    Faxa_swnet(:) = Faxa_swnet(:)/3600.0_r8
!    Faxa_sen(:) = Faxa_sen(:)/3600.0_r8
!    Faxa_lat(:) = Faxa_lat(:)/3600.0_r8

    ! convert m to kg/m^2/s
    !Faxa_rain(:) = Faxa_rain(:)/3600.0_r8*rhofw
!    Faxa_rain(:) = Faxa_rain(:)/21600.0_r8*rhofw
!    Faxa_rainc(:) = Faxa_rainc(:)/3600.0_r8*rhofw
!    Faxa_rainl(:) = Faxa_rainl(:)/3600.0_r8*rhofw
!    Faxa_snowc(:) = Faxa_snowc(:)/3600.0_r8*rhofw
    !Faxa_snow(:) = Faxa_snow(:)/3600.0_r8*rhofw
!    Faxa_snow(:) = Faxa_snow(:)/21600.0_r8*rhofw

    ! convert N/m^2 s to N/m^2
!    Faxa_taux(:) = Faxa_taux(:)/3600.0_r8
!    Faxa_tauy(:) = Faxa_tauy(:)/3600.0_r8
!    Faxa_taux(:) = Faxa_taux(:)/21600.0_r8
!    Faxa_tauy(:) = Faxa_tauy(:)/21600.0_r8

  end subroutine datm_datamode_cfsr_advance

  !===============================================================================
  subroutine datm_datamode_cfsr_restart_write(case_name, inst_suffix, ymd, tod, &
       logunit, my_task, sdat)
    
    ! input/output variables
    character(len=*)            , intent(in)    :: case_name
    character(len=*)            , intent(in)    :: inst_suffix
    integer                     , intent(in)    :: ymd       ! model date
    integer                     , intent(in)    :: tod       ! model sec into model date
    integer                     , intent(in)    :: logunit
    integer                     , intent(in)    :: my_task
    type(shr_strdata_type)      , intent(inout) :: sdat
    !-------------------------------------------------------------------------------

    call dshr_restart_write(rpfile, case_name, 'datm', inst_suffix, ymd, tod, &
         logunit, my_task, sdat)

  end subroutine datm_datamode_cfsr_restart_write

  !===============================================================================
  subroutine datm_datamode_cfsr_restart_read(rest_filem, inst_suffix, logunit, my_task, mpicom, sdat)

    ! input/output arguments
    character(len=*)            , intent(inout) :: rest_filem
    character(len=*)            , intent(in)    :: inst_suffix
    integer                     , intent(in)    :: logunit
    integer                     , intent(in)    :: my_task
    integer                     , intent(in)    :: mpicom
    type(shr_strdata_type)      , intent(inout) :: sdat
    !-------------------------------------------------------------------------------

    call dshr_restart_read(rest_filem, rpfile, inst_suffix, nullstr, logunit, my_task, mpicom, sdat)

  end subroutine datm_datamode_cfsr_restart_read

end module datm_datamode_cfsr_mod
