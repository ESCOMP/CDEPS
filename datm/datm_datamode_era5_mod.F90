module datm_datamode_era5_mod

  use ESMF             , only : ESMF_State, ESMF_SUCCESS, ESMF_LogWrite, ESMF_LOGMSG_INFO
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

  public  :: datm_datamode_era5_advertise
  public  :: datm_datamode_era5_init_pointers
  public  :: datm_datamode_era5_advance
  public  :: datm_datamode_era5_restart_write
  public  :: datm_datamode_era5_restart_read
  private :: datm_eSat  ! determine saturation vapor pressure

  ! export state data
  real(r8), pointer :: Sa_z(:)              => null()
  real(r8), pointer :: Sa_u10m(:)           => null()
  real(r8), pointer :: Sa_v10m(:)           => null()
  real(r8), pointer :: Sa_wspd10m(:)        => null()
  real(r8), pointer :: Sa_t2m(:)            => null()
  real(r8), pointer :: Sa_tskn(:)           => null()
  real(r8), pointer :: Sa_q2m(:)            => null()
  real(r8), pointer :: Sa_pslv(:)           => null()
  real(r8), pointer :: Faxa_rain(:)         => null()
  real(r8), pointer :: Faxa_rainc(:)        => null()
  real(r8), pointer :: Faxa_rainl(:)        => null()
  real(r8), pointer :: Faxa_snowc(:)        => null()
  real(r8), pointer :: Faxa_snowl(:)        => null()
  real(r8), pointer :: Faxa_swndr(:)        => null()
  real(r8), pointer :: Faxa_swndf(:)        => null()
  real(r8), pointer :: Faxa_swvdr(:)        => null()
  real(r8), pointer :: Faxa_swvdf(:)        => null()
  real(r8), pointer :: Faxa_swdn(:)         => null()
  real(r8), pointer :: Faxa_swnet(:)        => null()
  real(r8), pointer :: Faxa_lwdn(:)         => null()
  real(r8), pointer :: Faxa_lwnet(:)        => null()
  real(r8), pointer :: Faxa_sen(:)          => null()
  real(r8), pointer :: Faxa_lat(:)          => null()
  real(r8), pointer :: Faxa_taux(:)         => null()
  real(r8), pointer :: Faxa_tauy(:)         => null()

  ! stream data
  real(r8), pointer :: strm_tdew(:)         => null()

  real(r8) :: t2max  ! units detector
  real(r8) :: td2max ! units detector

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

  subroutine datm_datamode_era5_advertise(exportState, fldsexport, &
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
    call dshr_fldList_add(fldsExport, 'Sa_u10m'    )
    call dshr_fldList_add(fldsExport, 'Sa_v10m'    )
    call dshr_fldList_add(fldsExport, 'Sa_wspd10m' )
    call dshr_fldList_add(fldsExport, 'Sa_t2m'     )
    call dshr_fldList_add(fldsExport, 'Sa_tskn'    )
    call dshr_fldList_add(fldsExport, 'Sa_q2m'     )
    call dshr_fldList_add(fldsExport, 'Sa_pslv'    )
    call dshr_fldList_add(fldsExport, 'Faxa_rain'  )
    call dshr_fldList_add(fldsExport, 'Faxa_rainc' )
    call dshr_fldList_add(fldsExport, 'Faxa_rainl' )
    call dshr_fldList_add(fldsExport, 'Faxa_snowc' )
    call dshr_fldList_add(fldsExport, 'Faxa_snowl' )
    call dshr_fldList_add(fldsExport, 'Faxa_swndr' )
    call dshr_fldList_add(fldsExport, 'Faxa_swvdr' )
    call dshr_fldList_add(fldsExport, 'Faxa_swndf' )
    call dshr_fldList_add(fldsExport, 'Faxa_swvdf' )
    call dshr_fldList_add(fldsExport, 'Faxa_swdn'  )
    call dshr_fldList_add(fldsExport, 'Faxa_swnet' )
    call dshr_fldList_add(fldsExport, 'Faxa_lwdn'  )
    call dshr_fldList_add(fldsExport, 'Faxa_lwnet' )
    call dshr_fldList_add(fldsExport, 'Faxa_sen'   )
    call dshr_fldList_add(fldsExport, 'Faxa_lat'   )
    call dshr_fldList_add(fldsExport, 'Faxa_taux'  )
    call dshr_fldList_add(fldsExport, 'Faxa_tauy'  )

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(datm_comp_advertise): Fr_atm '//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine datm_datamode_era5_advertise

  !===============================================================================
  subroutine datm_datamode_era5_init_pointers(exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(datm_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! initialize pointers for module level stream arrays
    call shr_strdata_get_stream_pointer( sdat, 'Sa_tdew'   , strm_tdew , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! get export state pointers
    call dshr_state_getfldptr(exportState, 'Sa_z'       , fldptr1=Sa_z       , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_u10m'    , fldptr1=Sa_u10m    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_v10m'    , fldptr1=Sa_v10m    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_wspd10m' , fldptr1=Sa_wspd10m , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_t2m'     , fldptr1=Sa_t2m     , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_tskn'    , fldptr1=Sa_tskn    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_q2m'     , fldptr1=Sa_q2m     , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_pslv'    , fldptr1=Sa_pslv    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_rain'  , fldptr1=Faxa_rain  , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_rainc' , fldptr1=Faxa_rainc , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_rainl' , fldptr1=Faxa_rainl , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_snowc' , fldptr1=Faxa_snowc , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_snowl' , fldptr1=Faxa_snowl , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swvdr' , fldptr1=Faxa_swvdr , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swvdf' , fldptr1=Faxa_swvdf , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swndr' , fldptr1=Faxa_swndr , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swndf' , fldptr1=Faxa_swndf , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swdn'  , fldptr1=Faxa_swdn  , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swnet' , fldptr1=Faxa_swnet , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_lwdn'  , fldptr1=Faxa_lwdn  , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_lwnet' , fldptr1=Faxa_lwnet , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_sen'   , fldptr1=Faxa_sen   , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_lat'   , fldptr1=Faxa_lat   , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_taux'  , fldptr1=Faxa_taux  , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_tauy'  , fldptr1=Faxa_tauy  , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine datm_datamode_era5_init_pointers

  !===============================================================================
  subroutine datm_datamode_era5_advance(exportstate, masterproc, logunit, mpicom, target_ymd, target_tod, model_calendar, rc)

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
    real(r8) :: t2, pslv
    real(r8) :: vp
    real(r8) :: e, qsat
    character(len=*), parameter :: subname='(datm_datamode_era5_advance): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    lsize = size(strm_tdew)

    if (first_time) then
       ! determine t2max (see below for use)
       if (associated(Sa_t2m)) then
         rtmp = maxval(Sa_t2m(:))
         call shr_mpi_max(rtmp, t2max, mpicom, 'datm_t2m', all=.true.)
         if (masterproc) write(logunit,*) trim(subname),' t2max = ',t2max
       end if

       ! determine tdewmax (see below for use)
       rtmp = maxval(strm_tdew(:))
       call shr_mpi_max(rtmp, td2max, mpicom, 'datm_td2m', all=.true.)
       if (masterproc) write(logunit,*) trim(subname),' td2max = ',td2max

       ! reset first_time
       first_time = .false.
    end if

    do n = 1, lsize
       !--- bottom layer height ---
       if (associated(Sa_z)) then
         Sa_z(n) = 10.0_r8
       end if

       !--- calculate wind speed ---
       if (associated(Sa_wspd10m)) then
         Sa_wspd10m(n) = sqrt(Sa_u10m(n)*Sa_u10m(n)+Sa_v10m(n)*Sa_v10m(n))
       end if

       !--- specific humidity at 2m ---
       if (associated(Sa_t2m) .and. associated(Sa_pslv) .and. associated(Sa_q2m)) then
         t2 = Sa_t2m(n)
         pslv = Sa_pslv(n)
         if (td2max < 50.0_r8) strm_tdew(n) = strm_tdew(n) + tkFrz
         e = datm_eSat(strm_tdew(n), t2)
         qsat = (0.622_r8 * e)/(pslv - 0.378_r8 * e)
         Sa_q2m(n) = qsat
       end if
    end do

    !----------------------------------------------------------
    ! shortwave bands
    !----------------------------------------------------------

    !--- shortwave radiation (Faxa_* basically holds albedo) ---
    !--- see comments for Faxa_swnet
    if (associated(Faxa_swvdr)) Faxa_swvdr(:) = Faxa_swdn(:)*Faxa_swvdr(:)
    if (associated(Faxa_swndr)) Faxa_swndr(:) = Faxa_swdn(:)*Faxa_swndr(:)
    if (associated(Faxa_swvdf)) Faxa_swvdf(:) = Faxa_swdn(:)*Faxa_swvdf(:)
    if (associated(Faxa_swndf)) Faxa_swndf(:) = Faxa_swdn(:)*Faxa_swndf(:)

    !--- TODO: need to understand relationship between shortwave bands and 
    !--- net shortwave rad. currently it is provided directly from ERA5
    !--- and the total of the bands are not consistent with the swnet
    !--- swnet: a diagnostic quantity ---
    !if (associated(Faxa_swnet)) then
    !  if (associated(Faxa_swndr) .and. associated(Faxa_swvdr) .and. &
    !      associated(Faxa_swndf) .and. associated(Faxa_swvdf)) then
    !    Faxa_swnet(:) = Faxa_swndr(:) + Faxa_swvdr(:) + Faxa_swndf(:) + Faxa_swvdf(:)
    !  end if
    !end if

    !----------------------------------------------------------
    ! unit conversions (temporal resolution is hourly)
    !----------------------------------------------------------

    ! convert J/m^2 to W/m^2
    if (associated(Faxa_lwdn))  Faxa_lwdn(:)  = Faxa_lwdn(:)/3600.0_r8
    if (associated(Faxa_lwnet)) Faxa_lwnet(:) = Faxa_lwnet(:)/3600.0_r8
    if (associated(Faxa_swvdr)) Faxa_swvdr(:) = Faxa_swvdr(:)/3600.0_r8
    if (associated(Faxa_swndr)) Faxa_swndr(:) = Faxa_swndr(:)/3600.0_r8
    if (associated(Faxa_swvdf)) Faxa_swvdf(:) = Faxa_swvdf(:)/3600.0_r8
    if (associated(Faxa_swndf)) Faxa_swndf(:) = Faxa_swndf(:)/3600.0_r8
    if (associated(Faxa_swdn))  Faxa_swdn(:)  = Faxa_swdn(:)/3600.0_r8
    if (associated(Faxa_swnet)) Faxa_swnet(:) = Faxa_swnet(:)/3600.0_r8
    if (associated(Faxa_sen))   Faxa_sen(:)   = Faxa_sen(:)/3600.0_r8
    if (associated(Faxa_lat))   Faxa_lat(:)   = Faxa_lat(:)/3600.0_r8

    ! convert m to kg/m^2/s
    if (associated(Faxa_rain))  Faxa_rain(:)  = Faxa_rain(:)/3600.0_r8*rhofw
    if (associated(Faxa_rainc)) Faxa_rainc(:) = Faxa_rainc(:)/3600.0_r8*rhofw
    if (associated(Faxa_rainl)) Faxa_rainl(:) = Faxa_rainl(:)/3600.0_r8*rhofw
    if (associated(Faxa_snowc)) Faxa_snowc(:) = Faxa_snowc(:)/3600.0_r8*rhofw
    if (associated(Faxa_snowl)) Faxa_snowl(:) = Faxa_snowl(:)/3600.0_r8*rhofw

    ! convert N/m^2 s to N/m^2
    if (associated(Faxa_taux))  Faxa_taux(:)  = Faxa_taux(:)/3600.0_r8
    if (associated(Faxa_tauy))  Faxa_tauy(:)  = Faxa_tauy(:)/3600.0_r8

  end subroutine datm_datamode_era5_advance

  !===============================================================================
  subroutine datm_datamode_era5_restart_write(case_name, inst_suffix, ymd, tod, &
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

  end subroutine datm_datamode_era5_restart_write

  !===============================================================================
  subroutine datm_datamode_era5_restart_read(rest_filem, inst_suffix, logunit, my_task, mpicom, sdat)

    ! input/output arguments
    character(len=*)            , intent(inout) :: rest_filem
    character(len=*)            , intent(in)    :: inst_suffix
    integer                     , intent(in)    :: logunit
    integer                     , intent(in)    :: my_task
    integer                     , intent(in)    :: mpicom
    type(shr_strdata_type)      , intent(inout) :: sdat
    !-------------------------------------------------------------------------------

    call dshr_restart_read(rest_filem, rpfile, inst_suffix, nullstr, logunit, my_task, mpicom, sdat)

  end subroutine datm_datamode_era5_restart_read

  real(r8) function datm_eSat(tK,tKbot)

    !----------------------------------------------------------------------------
    ! use polynomials to calculate saturation vapor pressure and derivative with
    ! respect to temperature: over water when t > 0 c and over ice when t <= 0 c
    ! required to convert relative humidity to specific humidity
    !----------------------------------------------------------------------------

    ! input/output variables
    real(r8),intent(in) :: tK    ! temp used in polynomial calculation
    real(r8),intent(in) :: tKbot ! bottom atm temp

    ! local variables
    real(r8)           :: t     ! tK converted to Celcius
    real(r8),parameter :: tkFrz = shr_const_tkfrz  ! freezing T of fresh water ~ K

    !--- coefficients for esat over water ---
    real(r8),parameter :: a0=6.107799961_r8
    real(r8),parameter :: a1=4.436518521e-01_r8
    real(r8),parameter :: a2=1.428945805e-02_r8
    real(r8),parameter :: a3=2.650648471e-04_r8
    real(r8),parameter :: a4=3.031240396e-06_r8
    real(r8),parameter :: a5=2.034080948e-08_r8
    real(r8),parameter :: a6=6.136820929e-11_r8

    !--- coefficients for esat over ice ---
    real(r8),parameter :: b0=6.109177956_r8
    real(r8),parameter :: b1=5.034698970e-01_r8
    real(r8),parameter :: b2=1.886013408e-02_r8
    real(r8),parameter :: b3=4.176223716e-04_r8
    real(r8),parameter :: b4=5.824720280e-06_r8
    real(r8),parameter :: b5=4.838803174e-08_r8
    real(r8),parameter :: b6=1.838826904e-10_r8

    t = min( 50.0_r8, max(-50.0_r8,(tK-tKfrz)) )
    if ( tKbot < tKfrz) then
       datm_eSat = 100.0_r8*(b0+t*(b1+t*(b2+t*(b3+t*(b4+t*(b5+t*b6))))))
    else
       datm_eSat = 100.0_r8*(a0+t*(a1+t*(a2+t*(a3+t*(a4+t*(a5+t*a6))))))
    end if

  end function datm_eSat

end module datm_datamode_era5_mod
