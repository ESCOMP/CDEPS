module datm_datamode_clmncep_mod

  use ESMF             , only : ESMF_SUCCESS, ESMF_LogWrite, ESMF_State, ESMF_StateItem_Flag
  use ESMF             , only : ESMF_STATEITEM_NOTFOUND, ESMF_LOGMSG_INFO, ESMF_StateGet, operator(/=)
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_log_mod      , only : shr_log_error
  use shr_precip_mod   , only : shr_precip_partition_rain_snow_ramp
  use shr_const_mod    , only : shr_const_spval, shr_const_tkfrz, shr_const_pi
  use shr_const_mod    , only : shr_const_pstd, shr_const_stebol, shr_const_rdair
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_strdata_mod , only : shr_strdata_type
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add

  implicit none
  private ! except

  public  :: datm_datamode_clmncep_advertise
  public  :: datm_datamode_clmncep_init_pointers
  public  :: datm_datamode_clmncep_advance
  private :: datm_esat  ! determine saturation vapor pressure

  ! export state data
  real(r8), pointer :: Sa_z(:)              => null()
  real(r8), pointer :: Sa_u(:)              => null()
  real(r8), pointer :: Sa_v(:)              => null()
  real(r8), pointer :: Sa_tbot(:)           => null()
  real(r8), pointer :: Sa_ptem(:)           => null()
  real(r8), pointer :: Sa_shum(:)           => null()
! TODO: water isotope support
!  real(r8), pointer :: Sa_shum_wiso(:,:)    => null() ! water isotopes
  real(r8), pointer :: Sa_dens(:)           => null()
  real(r8), pointer :: Sa_pbot(:)           => null()
  real(r8), pointer :: Sa_pslv(:)           => null()
  real(r8), pointer :: Sa_o3(:)             => null()
  real(r8), pointer :: Faxa_lwdn(:)         => null()
  real(r8), pointer :: Faxa_rainc(:)        => null()
  real(r8), pointer :: Faxa_rainl(:)        => null()
  real(r8), pointer :: Faxa_snowc(:)        => null()
  real(r8), pointer :: Faxa_snowl(:)        => null()
  real(r8), pointer :: Faxa_swndr(:)        => null()
  real(r8), pointer :: Faxa_swndf(:)        => null()
  real(r8), pointer :: Faxa_swvdr(:)        => null()
  real(r8), pointer :: Faxa_swvdf(:)        => null()
  real(r8), pointer :: Faxa_swnet(:)        => null()
  real(r8), pointer :: Faxa_ndep(:,:)       => null()

  ! stream data
  real(r8), pointer :: strm_z(:)         => null()
  real(r8), pointer :: strm_wind(:)      => null()
  real(r8), pointer :: strm_tdew(:)      => null()
  real(r8), pointer :: strm_tbot(:)      => null()
  real(r8), pointer :: strm_pbot(:)      => null()
  real(r8), pointer :: strm_shum(:)      => null()
  real(r8), pointer :: strm_lwdn(:)      => null()
  real(r8), pointer :: strm_rh(:)        => null()
  real(r8), pointer :: strm_swdn(:)      => null()
  real(r8), pointer :: strm_swdndf(:)    => null()
  real(r8), pointer :: strm_swdndr(:)    => null()
  real(r8), pointer :: strm_precc(:)     => null()
  real(r8), pointer :: strm_precl(:)     => null()
  real(r8), pointer :: strm_precn(:)     => null()

  ! stream data - water isotopes
  real(r8), pointer :: strm_rh_16O(:)    => null() ! water isoptopes
  real(r8), pointer :: strm_rh_18O(:)    => null() ! water isoptopes
  real(r8), pointer :: strm_rh_HDO(:)    => null() ! water isoptopes
  real(r8), pointer :: strm_precn_16O(:) => null() ! water isoptopes
  real(r8), pointer :: strm_precn_18O(:) => null() ! water isoptopes
  real(r8), pointer :: strm_precn_HDO(:) => null() ! water isoptopes

  ! stream data bias correction
  real(r8), pointer :: strm_precsf(:)    => null()

  ! stream data anomaly forcing
  real(r8), pointer :: strm_u_af(:)      => null() ! anomaly forcing
  real(r8), pointer :: strm_v_af(:)      => null() ! anomaly forcing
  real(r8), pointer :: strm_prec_af(:)   => null() ! anomaly forcing
  real(r8), pointer :: strm_tbot_af(:)   => null() ! anomaly forcing
  real(r8), pointer :: strm_pbot_af(:)   => null() ! anomaly forcing
  real(r8), pointer :: strm_shum_af(:)   => null() ! anomaly forcing
  real(r8), pointer :: strm_swdn_af(:)   => null() ! anomaly forcing
  real(r8), pointer :: strm_lwdn_af(:)   => null() ! anomaly forcing

  ! import state data
  real(r8), pointer :: Sx_avsdr(:)        => null()
  real(r8), pointer :: Sx_anidr(:)        => null()
  real(r8), pointer :: Sx_avsdf(:)        => null()
  real(r8), pointer :: Sx_anidf(:)        => null()

  logical  :: atm_prognostic = .false.
  real(r8) :: tbotmax               ! units detector
  real(r8) :: tdewmax               ! units detector
  real(r8) :: anidrmax              ! existance detector

  real(r8) , parameter :: tKFrz    = SHR_CONST_TKFRZ
  real(r8) , parameter :: degtorad = SHR_CONST_PI/180.0_r8
  real(r8) , parameter :: pstd     = SHR_CONST_PSTD     ! standard pressure ~ Pa
  real(r8) , parameter :: stebol   = SHR_CONST_STEBOL   ! Stefan-Boltzmann constant ~ W/m^2/K^4
  real(r8) , parameter :: rdair    = SHR_CONST_RDAIR    ! dry air gas constant   ~ J/K/kg


  character(*), parameter :: nullstr = 'null'
  character(*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine datm_datamode_clmncep_advertise(exportState, fldsexport, flds_scalar_name, &
       flds_co2, flds_wiso, flds_presaero, flds_presndep, flds_preso3, rc)

    ! input/output variables
    type(esmf_State)   , intent(inout) :: exportState
    type(fldlist_type) , pointer       :: fldsexport
    logical            , intent(in)    :: flds_co2
    logical            , intent(in)    :: flds_wiso
    logical            , intent(in)    :: flds_presaero
    logical            , intent(in)    :: flds_presndep
    logical            , intent(in)    :: flds_preso3
    character(len=*)   , intent(in)    :: flds_scalar_name
    integer            , intent(out)   :: rc

    ! local variables
    type(fldlist_type), pointer :: fldList
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
    call dshr_fldList_add(fldsExport, 'Sa_topo'    )
    call dshr_fldList_add(fldsExport, 'Sa_z'       )
    call dshr_fldList_add(fldsExport, 'Sa_u'       )
    call dshr_fldList_add(fldsExport, 'Sa_v'       )
    call dshr_fldList_add(fldsExport, 'Sa_ptem'    )
    call dshr_fldList_add(fldsExport, 'Sa_dens'    )
    call dshr_fldList_add(fldsExport, 'Sa_pslv'    )
    call dshr_fldList_add(fldsExport, 'Sa_tbot'    )
    call dshr_fldList_add(fldsExport, 'Sa_pbot'    )
    call dshr_fldList_add(fldsExport, 'Sa_shum'    )
    call dshr_fldList_add(fldsExport, 'Faxa_rainc' )
    call dshr_fldList_add(fldsExport, 'Faxa_rainl' )
    call dshr_fldList_add(fldsExport, 'Faxa_snowc' )
    call dshr_fldList_add(fldsExport, 'Faxa_snowl' )
    call dshr_fldList_add(fldsExport, 'Faxa_swndr' )
    call dshr_fldList_add(fldsExport, 'Faxa_swvdr' )
    call dshr_fldList_add(fldsExport, 'Faxa_swndf' )
    call dshr_fldList_add(fldsExport, 'Faxa_swvdf' )
    call dshr_fldList_add(fldsExport, 'Faxa_swnet' )
    call dshr_fldList_add(fldsExport, 'Faxa_lwdn'  )
    call dshr_fldList_add(fldsExport, 'Faxa_swdn'  )
    if (flds_co2) then
       call dshr_fldList_add(fldsExport, 'Sa_co2prog')
       call dshr_fldList_add(fldsExport, 'Sa_co2diag')
    end if
    if (flds_preso3) then
       call dshr_fldList_add(fldsExport, 'Sa_o3')
    end if
    if (flds_presaero) then
       call dshr_fldList_add(fldsExport, 'Faxa_bcph'   , ungridded_lbound=1, ungridded_ubound=3)
       call dshr_fldList_add(fldsExport, 'Faxa_ocph'   , ungridded_lbound=1, ungridded_ubound=3)
       call dshr_fldList_add(fldsExport, 'Faxa_dstwet' , ungridded_lbound=1, ungridded_ubound=4)
       call dshr_fldList_add(fldsExport, 'Faxa_dstdry' , ungridded_lbound=1, ungridded_ubound=4)
    end if
    if (flds_presndep) then
       call dshr_fldList_add(fldsExport, 'Faxa_ndep', ungridded_lbound=1, ungridded_ubound=2)
    end if
    if (flds_wiso) then
       call dshr_fldList_add(fldsExport, 'Faxa_rainc_wiso', ungridded_lbound=1, ungridded_ubound=3)
       call dshr_fldList_add(fldsExport, 'Faxa_rainl_wiso', ungridded_lbound=1, ungridded_ubound=3)
       call dshr_fldList_add(fldsExport, 'Faxa_snowc_wiso', ungridded_lbound=1, ungridded_ubound=3)
       call dshr_fldList_add(fldsExport, 'Faxa_snowl_wiso', ungridded_lbound=1, ungridded_ubound=3)
       call dshr_fldList_add(fldsExport, 'Faxa_shum_wiso' , ungridded_lbound=1, ungridded_ubound=3)
    end if

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(datm_comp_advertise): Fr_atm'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine datm_datamode_clmncep_advertise

  !===============================================================================
  subroutine datm_datamode_clmncep_init_pointers(importState, exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: importState
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    type(ESMF_StateItem_Flag) :: itemFlag
    character(len=*), parameter :: subname='(datm_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! initialize pointers for module level stream arrays
    call shr_strdata_get_stream_pointer( sdat, 'Sa_pbot'        , strm_pbot  , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Sa_tbot'        , strm_tbot  , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Sa_shum'        , strm_shum  , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Sa_wind'        , strm_wind  , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Sa_tdew'        , strm_tdew  , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Sa_rh'          , strm_rh    , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Sa_z'           , strm_z  , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_swdndf'    , strm_swdndf, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_swdndr'    , strm_swdndr, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_lwdn'      , strm_lwdn  , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_swdn'      , strm_swdn  , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_precn'     , strm_precn , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_rh_16O'    , strm_rh_16O, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_rh_18O'    , strm_rh_18O   , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_rh_HDO'    , strm_rh_HDO   , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_precn_16O' , strm_precn_16O, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_precn_18O' , strm_precn_18O, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_precn_HDO' , strm_precn_HDO, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_precn_HDO' , strm_precn_HDO, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! initialize pointers for module level stream arrays for bias correction
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_precsf'   , strm_precsf   , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! initialize pointers for module level stream arrays for anomaly forcing
    call shr_strdata_get_stream_pointer( sdat, 'Sa_u_af'      , strm_u_af   , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Sa_v_af'      , strm_v_af   , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Sa_shum_af'   , strm_shum_af, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Sa_tbot_af'   , strm_tbot_af, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Sa_pbot_af'   , strm_pbot_af, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_prec_af' , strm_prec_af, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_swdn_af' , strm_swdn_af, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_lwdn_af' , strm_lwdn_af, rc)
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
    call dshr_state_getfldptr(exportState, 'Sa_pslv'    , fldptr1=Sa_pslv    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_ptem'    , fldptr1=Sa_ptem    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_shum'    , fldptr1=Sa_shum    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_dens'    , fldptr1=Sa_dens    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_rainc' , fldptr1=Faxa_rainc , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_rainl' , fldptr1=Faxa_rainl , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_snowc' , fldptr1=Faxa_snowc , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_snowl' , fldptr1=Faxa_snowl , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swvdr' , fldptr1=Faxa_swvdr , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swvdf' , fldptr1=Faxa_swvdf , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swndr' , fldptr1=Faxa_swndr , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swndf' , fldptr1=Faxa_swndf , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swnet' , fldptr1=Faxa_swnet , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_lwdn'  , fldptr1=Faxa_lwdn  , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_StateGet(exportstate, 'Faxa_ndep', itemFlag, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (itemflag /= ESMF_STATEITEM_NOTFOUND) then
       call dshr_state_getfldptr(exportState, 'Faxa_ndep', fldptr2=Faxa_ndep, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    call ESMF_StateGet(exportstate, 'Sa_o3', itemFlag, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (itemflag /= ESMF_STATEITEM_NOTFOUND) then
       call dshr_state_getfldptr(exportState, 'Sa_o3', fldptr1=Sa_o3, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    ! error check
    if (.not. associated(strm_wind) .or. .not. associated(strm_tbot)) then
       call shr_log_error(trim(subname)//' ERROR: wind and tbot must be in streams for CLMNCEP', rc=rc)
       return
    endif

    ! determine anidrmax (see below for use)
    call ESMF_StateGet(importstate, 'Sx_anidr', itemFlag, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (itemflag /= ESMF_STATEITEM_NOTFOUND) then
       atm_prognostic = .true.
       call dshr_state_getfldptr(importState, 'Sx_anidr', fldptr1=Sx_anidr, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call dshr_state_getfldptr(importState, 'Sx_anidf', fldptr1=Sx_anidf, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call dshr_state_getfldptr(importState, 'Sx_avsdr', fldptr1=Sx_avsdr, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call dshr_state_getfldptr(importState, 'Sx_avsdf', fldptr1=Sx_avsdf, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

  end subroutine datm_datamode_clmncep_init_pointers

  !===============================================================================
  subroutine datm_datamode_clmncep_advance(mainproc, logunit, mpicom, rc)
    use ESMF, only: ESMF_VMGetCurrent, ESMF_VMAllReduce, ESMF_REDUCE_MAX, ESMF_VM

    ! input/output variables
    logical                , intent(in)    :: mainproc
    integer                , intent(in)    :: logunit
    integer                , intent(in)    :: mpicom
    integer                , intent(out)   :: rc

    ! local variables
    logical  :: first_time = .true.
    integer  :: n                   ! indices
    integer  :: lsize               ! size of attr vect
    real(r8) :: rtmp(2)
    real(r8) :: swndr
    real(r8) :: swndf
    real(r8) :: swvdr
    real(r8) :: swvdf
    real(r8) :: ratio_rvrf
    real(r8) :: tbot, pbot
    real(r8) :: vp
    real(r8) :: ea, e, qsat, frac
    type(ESMF_VM) :: vm
    character(len=*), parameter :: subname='(datm_datamode_clmncep_advance): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    lsize = size(Sa_u)

    if (first_time) then
       call ESMF_VMGetCurrent(vm, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       ! determine tbotmax (see below for use)
       rtmp(1) = maxval(Sa_tbot(:))
       call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       tbotmax = rtmp(2)
       if (mainproc) write(logunit,*) trim(subname),' tbotmax = ',tbotmax
       if(tbotmax <= 0) then
          call shr_log_error(subname//'ERROR: bad value in tbotmax', rc=rc)
          return
       endif

       ! determine anidrmax (see below for use)
       if (atm_prognostic) then
          rtmp(1) = maxval(Sx_anidr(:))
          call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          anidrmax = rtmp(2)
       else
          anidrmax = SHR_CONST_SPVAL
       end if
       if (mainproc) write(logunit,*) trim(subname),' anidrmax = ',anidrmax

       ! determine tdewmax (see below for use)
       if (associated(strm_tdew)) then
          rtmp(1) = maxval(strm_tdew(:))
          call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          tdewmax = rtmp(2)
          if (mainproc) write(logunit,*) trim(subname),' tdewmax = ',tdewmax
       endif

       ! reset first_time
       first_time = .false.
    end if

    do n = 1,lsize
       !--- bottom layer height ---
       if (.not. associated(strm_z)) Sa_z(n) = 30.0_r8

       !--- temperature ---
       if (tbotmax < 50.0_r8) Sa_tbot(n) = Sa_tbot(n) + tkFrz
       ! Limit very cold forcing to 180K
       Sa_tbot(n) = max(180._r8, Sa_tbot(n))
       Sa_ptem(n) = Sa_tbot(n)

       !--- pressure ---
       if (.not. associated(strm_pbot)) then
          Sa_pbot(n) = pstd
       else if (Sa_pbot(n) == 0._r8) then
          ! This happens if you are using points over ocean where the mask is 0
          Sa_pbot(n) = pstd
       end if

       Sa_pslv(n) = Sa_pbot(n)

       !--- u, v wind velocity ---
       Sa_u(n) = strm_wind(n)/sqrt(2.0_r8)
       Sa_v(n) = Sa_u(n)

       !--- specific humidity ---
       tbot = Sa_tbot(n)
       pbot = Sa_pbot(n)
       if (associated(strm_shum)) then
          e = datm_esat(tbot,tbot)
          qsat = (0.622_r8 * e)/(pbot - 0.378_r8 * e)
          if (qsat < Sa_shum(n)) then
             Sa_shum(n) = qsat
          endif
       else if (associated(strm_rh)) then
          e = strm_rh(n) * 0.01_r8 * datm_esat(tbot,tbot)
          qsat = (0.622_r8 * e)/(pbot - 0.378_r8 * e)
          Sa_shum(n) = qsat
          ! for isotopic tracer specific humidity, expect a delta, just keep the delta from the input file
          ! if (associated(strm_rh_16O) .and. associated(strm_rh_18O) .and. associated(strm_rh_HDO)) then
          !   Sa_shum_wiso(1,n) = strm_rh_16O(n)
          !   Sa_shum_wiso(2,n) = strm_rh_18O(n)
          !   Sa_shum_wiso(3,n) = strm_rh_HDO(n)
          ! end if
       else if (associated(strm_tdew)) then
          if (tdewmax < 50.0_r8) strm_tdew(n) = strm_tdew(n) + tkFrz
          e = datm_esat(strm_tdew(n),tbot)
          qsat = (0.622_r8 * e)/(pbot - 0.378_r8 * e)
          Sa_shum(n) = qsat
       else
          call shr_log_error(subname//'ERROR: cannot compute shum', rc=rc)
          return
       endif
       !--- density ---
       vp = (Sa_shum(n)*pbot) / (0.622_r8 + 0.378_r8 * Sa_shum(n))
       Sa_dens(n) = (pbot - 0.378_r8 * vp) / (tbot*rdair)

       !--- downward longwave ---
       if (.not. associated(strm_lwdn)) then
          e  = Sa_pslv(n) * Sa_shum(n) / (0.622_r8 + 0.378_r8 * Sa_shum(n))
          ea = 0.70_r8 + 5.95e-05_r8 * 0.01_r8 * e * exp(1500.0_r8/tbot)
          Faxa_lwdn(n) = ea * stebol * tbot**4
       endif

       !--- shortwave radiation ---
       if (associated(strm_swdndf) .and. associated(strm_swdndr)) then
          Faxa_swndr(n) = strm_swdndr(n) * 0.50_r8
          Faxa_swvdr(n) = strm_swdndr(n) * 0.50_r8
          Faxa_swndf(n) = strm_swdndf(n) * 0.50_r8
          Faxa_swvdf(n) = strm_swdndf(n) * 0.50_r8
       elseif (associated(strm_swdn)) then
          ! relationship between incoming NIR or VIS radiation and ratio of
          ! direct to diffuse radiation calculated based on one year's worth of
          ! hourly CAM output from CAM version cam3_5_55
          swndr = strm_swdn(n) * 0.50_r8
          ratio_rvrf =  min(0.99_r8,max(0.29548_r8 + 0.00504_r8*swndr  &
               -1.4957e-05_r8*swndr**2 + 1.4881e-08_r8*swndr**3,0.01_r8))
          Faxa_swndr(n) = ratio_rvrf*swndr
          swndf = strm_swdn(n) * 0.50_r8
          Faxa_swndf(n) = (1._r8 - ratio_rvrf)*swndf

          swvdr = strm_swdn(n) * 0.50_r8
          ratio_rvrf =   min(0.99_r8,max(0.17639_r8 + 0.00380_r8*swvdr  &
               -9.0039e-06_r8*swvdr**2 + 8.1351e-09_r8*swvdr**3,0.01_r8))
          Faxa_swvdr(n) = ratio_rvrf*swvdr
          swvdf = strm_swdn(n) * 0.50_r8
          Faxa_swvdf(n) = (1._r8 - ratio_rvrf)*swvdf
       else
          call shr_log_error(subName//'ERROR: cannot compute short-wave down', rc=rc)
          return
       endif

       !--- swnet: a diagnostic quantity ---
       if (anidrmax < 1.0e-8 .or. anidrmax > SHR_CONST_SPVAL * 0.9_r8) then
          Faxa_swnet(n) = 0.0_r8
       else if ( associated(Sx_anidr) .and. associated(Sx_anidf) .and. &
                 associated(Sx_avsdr) .and. associated(Sx_avsdf)) then
          Faxa_swnet(n) = (1.0_r8-Sx_anidr(n))*Faxa_swndr(n) + &
                          (1.0_r8-Sx_avsdr(n))*Faxa_swvdr(n) + &
                          (1.0_r8-Sx_anidf(n))*Faxa_swndf(n) + &
                          (1.0_r8-Sx_avsdf(n))*Faxa_swvdf(n)
       else
          Faxa_swnet(n) = Faxa_swndr(n) + Faxa_swvdr(n) + Faxa_swndf(n) + Faxa_swvdf(n)
       endif

       !--- rain and snow ---
       if (associated(strm_precc) .and. associated(strm_precl)) then
          Faxa_rainc(n) = strm_precc(n)
          Faxa_rainl(n) = strm_precl(n)
       else if (associated(strm_precn)) then
          Faxa_rainc(n) = strm_precn(n)*0.1_r8
          Faxa_rainl(n) = strm_precn(n)*0.9_r8
       else
          call shr_log_error(subName//'ERROR: cannot compute rain and snow', rc=rc)
          return
       endif

       !--- split precip between rain & snow ---
       call shr_precip_partition_rain_snow_ramp(tbot, frac)
       Faxa_snowc(n) = max(0.0_r8, Faxa_rainc(n)*(1.0_r8 - frac))
       Faxa_snowl(n) = max(0.0_r8, Faxa_rainl(n)*(1.0_r8 - frac))
       Faxa_rainc(n) = max(0.0_r8, Faxa_rainc(n)*(         frac))
       Faxa_rainl(n) = max(0.0_r8, Faxa_rainl(n)*(         frac))

    end do

    !----------------------------------------------------------
    ! bias correction / anomaly forcing ( start block )
    ! modify atmospheric input fields if streams exist
    !----------------------------------------------------------

    ! bias correct precipitation relative to observed
    ! (via bias_correct nameslist option)
    if (associated(strm_precsf)) then
       Faxa_snowc(:) = Faxa_snowc(:) * min(1.e2_r8,strm_precsf(:))
       Faxa_snowl(:) = Faxa_snowl(:) * min(1.e2_r8,strm_precsf(:))
       Faxa_rainc(:) = Faxa_rainc(:) * min(1.e2_r8,strm_precsf(:))
       Faxa_rainl(:) = Faxa_rainl(:) * min(1.e2_r8,strm_precsf(:))
    endif

    ! adjust atmospheric input fields if anomaly forcing streams exist
    ! (via anomaly_forcing namelist option)
    if (associated(strm_u_af) .and. associated(strm_v_af)) then ! wind
       Sa_u(:) = Sa_u(:) + strm_u_af(:)
       Sa_v(:) = Sa_v(:) + strm_v_af(:)
    endif
    if (associated(strm_shum_af)) then  ! specific humidity
       Sa_shum(:) = Sa_shum(:) + strm_shum_af(:)
       ! avoid possible negative q values
       where (Sa_shum < 0._r8)
          Sa_shum = 1.e-6_r8
       end where
    endif
    if (associated(strm_pbot_af)) then ! pressure
       Sa_pbot(:) = Sa_pbot(:) + strm_pbot_af(:)
    endif
    if (associated(strm_tbot_af)) then ! temperature
       Sa_tbot(:) = Sa_tbot(:) + strm_tbot_af(:)
    endif
    if (associated(strm_lwdn_af)) then ! longwave
       Faxa_lwdn(:) = Faxa_lwdn(:) * strm_lwdn_af(:)
    endif
    if (associated(strm_prec_af)) then ! precipitation
       Faxa_snowc(:) = Faxa_snowc(:) * strm_prec_af(:)
       Faxa_snowl(:) = Faxa_snowl(:) * strm_prec_af(:)
       Faxa_rainc(:) = Faxa_rainc(:) * strm_prec_af(:)
       Faxa_rainl(:) = Faxa_rainl(:) * strm_prec_af(:)
    end if
    if (associated(strm_swdn_af)) then ! shortwave
       Faxa_swndr(:) = Faxa_swndr(:) * strm_swdn_af(:)
       Faxa_swvdr(:) = Faxa_swvdr(:) * strm_swdn_af(:)
       Faxa_swndf(:) = Faxa_swndf(:) * strm_swdn_af(:)
       Faxa_swvdf(:) = Faxa_swvdf(:) * strm_swdn_af(:)
    endif
    ! bias correction / anomaly forcing ( end block )

    if (associated(Faxa_ndep)) then
       ! convert ndep flux to units of kgN/m2/s (input is in gN/m2/s)
       Faxa_ndep(:,:) = Faxa_ndep(:,:) / 1000._r8
    end if

  end subroutine datm_datamode_clmncep_advance

  !===============================================================================

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

end module datm_datamode_clmncep_mod
