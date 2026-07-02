module datm_datamode_era5_mod

  use ESMF             , only : ESMF_State, ESMF_MeshGet, ESMF_SUCCESS, ESMF_LogWrite, ESMF_LOGMSG_INFO
  use NUOPC            , only : NUOPC_Advertise
  use shr_cal_mod      , only : shr_cal_date2julian
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_const_mod    , only : shr_const_tkfrz, shr_const_rhofw, shr_const_rdair, shr_const_pi
  use shr_log_mod      , only : shr_log_error
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add

  implicit none
  private

  public  :: datm_datamode_era5_advertise
  public  :: datm_datamode_era5_init_pointers
  public  :: datm_datamode_era5_advance

  private :: datm_eSat  ! determine saturation vapor pressure

  ! export state data
  real(r8), pointer :: Sa_z(:)              => null()
  real(r8), pointer :: Sa_u10m(:)           => null()
  real(r8), pointer :: Sa_v10m(:)           => null()
  real(r8), pointer :: Sa_wspd10m(:)        => null()
  real(r8), pointer :: Sa_u(:)              => null()
  real(r8), pointer :: Sa_v(:)              => null()
  real(r8), pointer :: Sa_wspd(:)           => null()
  real(r8), pointer :: Sa_t2m(:)            => null()
  real(r8), pointer :: Sa_tskn(:)           => null()
  real(r8), pointer :: Sa_tbot(:)           => null()
  real(r8), pointer :: Sa_ptem(:)           => null()
  real(r8), pointer :: Sa_q2m(:)            => null()
  real(r8), pointer :: Sa_shum(:)           => null()
  real(r8), pointer :: Sa_pslv(:)           => null()
  real(r8), pointer :: Sa_pbot(:)           => null()
  real(r8), pointer :: Sa_dens(:)           => null()
  real(r8), pointer :: Faxa_rain(:)         => null()
  real(r8), pointer :: Faxa_rainc(:)        => null()
  real(r8), pointer :: Faxa_rainl(:)        => null()
  real(r8), pointer :: Faxa_snow(:)         => null()
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
  real(r8), pointer :: strm_Sa_tdew(:)    => null()
  real(r8), pointer :: strm_Sa_t2m(:)     => null()
  real(r8), pointer :: strm_Sa_tbot(:)    => null()
  real(r8), pointer :: strm_Sa_u10m(:)    => null()
  real(r8), pointer :: strm_Sa_v10m(:)    => null()
  real(r8), pointer :: strm_Sa_wspd10m(:) => null()
  real(r8), pointer :: strm_Sa_u(:)       => null()
  real(r8), pointer :: strm_Sa_v(:)       => null()
  real(r8), pointer :: strm_Sa_wspd(:)    => null()
  real(r8), pointer :: strm_Sa_pslv(:)    => null()
  real(r8), pointer :: strm_Sa_pbot(:)    => null()
  real(r8), pointer :: strm_Sa_q2m(:)     => null()
  real(r8), pointer :: strm_Sa_shum(:)    => null()
  real(r8), pointer :: strm_Faxa_swdn(:)  => null()
  real(r8), pointer :: strm_Faxa_swnet(:) => null()
  real(r8), pointer :: strm_Faxa_lwdn(:)  => null()
  real(r8), pointer :: strm_Faxa_lwnet(:) => null()
  real(r8), pointer :: strm_Faxa_rain(:)  => null()
  real(r8), pointer :: strm_Faxa_rainc(:) => null()
  real(r8), pointer :: strm_Faxa_rainl(:) => null()
  real(r8), pointer :: strm_Faxa_snow(:)  => null()
  real(r8), pointer :: strm_Faxa_snowc(:) => null()
  real(r8), pointer :: strm_Faxa_snowl(:) => null()
  real(r8), pointer :: strm_Faxa_sen(:)   => null()
  real(r8), pointer :: strm_Faxa_lat(:)   => null()
  real(r8), pointer :: strm_Faxa_taux(:)  => null()
  real(r8), pointer :: strm_Faxa_tauy(:)  => null()

  ! other module arrays
  real(r8), pointer :: yc(:) ! array of model latitudes

  real(r8) :: t2max   ! units detector
  real(r8) :: td2max  ! units detector
  real(r8) :: lwmax   ! units detector
  real(r8) :: precmax ! units detector

  real(r8) , parameter :: tKFrz    = SHR_CONST_TKFRZ
  real(r8) , parameter :: rdair    = SHR_CONST_RDAIR ! dry air gas constant ~ J/K/kg
  real(r8) , parameter :: rhofw    = SHR_CONST_RHOFW ! density of fresh water ~ kg/m^3
  real(R8) , parameter :: degtorad = SHR_CONST_PI/180.0_R8
  real(R8) , parameter :: phs_c0   =   0.298_R8
  real(R8) , parameter :: dLWarc   =  -5.000_R8

  character(len=*), parameter :: nullstr = 'undefined'
  character(len=*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine datm_datamode_era5_advertise(exportState, fldsexport, flds_scalar_name, rc)

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
    call dshr_fldList_add(fldsExport, 'Sa_u'       )
    call dshr_fldList_add(fldsExport, 'Sa_v'       )
    call dshr_fldList_add(fldsExport, 'Sa_wspd'    )
    call dshr_fldList_add(fldsExport, 'Sa_t2m'     )
    call dshr_fldList_add(fldsExport, 'Sa_tskn'    )
    call dshr_fldList_add(fldsExport, 'Sa_tbot'    )
    call dshr_fldList_add(fldsExport, 'Sa_ptem'    )
    call dshr_fldList_add(fldsExport, 'Sa_q2m'     )
    call dshr_fldList_add(fldsExport, 'Sa_shum'    )
    call dshr_fldList_add(fldsExport, 'Sa_pslv'    )
    call dshr_fldList_add(fldsExport, 'Sa_pbot'    )
    call dshr_fldList_add(fldsExport, 'Sa_dens'    )
    call dshr_fldList_add(fldsExport, 'Faxa_rain'  )
    call dshr_fldList_add(fldsExport, 'Faxa_rainc' )
    call dshr_fldList_add(fldsExport, 'Faxa_rainl' )
    call dshr_fldList_add(fldsExport, 'Faxa_snow'  )
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
  subroutine datm_datamode_era5_init_pointers(exportState, sdat, skip_field_check, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    logical                , intent(in)    :: skip_field_check
    integer                , intent(out)   :: rc

    ! local variables
    integer           :: n
    integer           :: spatialDim         ! number of dimension in mesh
    integer           :: numOwnedElements   ! size of mesh
    real(r8), pointer :: ownedElemCoords(:) ! mesh lat and lons
    character(len=*), parameter :: subname='(datm_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! determine yc
    call ESMF_MeshGet(sdat%model_mesh, spatialDim=spatialDim, numOwnedElements=numOwnedElements, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    allocate(ownedElemCoords(spatialDim*numOwnedElements))
    allocate(yc(numOwnedElements))
    call ESMF_MeshGet(sdat%model_mesh, ownedElemCoords=ownedElemCoords)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    do n = 1,numOwnedElements
       yc(n) = ownedElemCoords(2*n)
    end do

    ! initialize pointers for module level stream arrays
    call shr_strdata_get_stream_pointer( sdat,'Sa_tdew', strm_Sa_tdew , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_t2m' , strm_Sa_t2m , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_tbot' , strm_Sa_tbot , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_u10m', strm_Sa_u10m, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_v10m', strm_Sa_v10m, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_wspd10m', strm_Sa_wspd10m, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_u', strm_Sa_u, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_v', strm_Sa_v, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_wspd', strm_Sa_wspd, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_pslv', strm_Sa_pslv, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_pbot', strm_Sa_pbot, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_shum', strm_Sa_shum, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_q2m', strm_Sa_q2m, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_rain', strm_Faxa_rain, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_rainc', strm_Faxa_rainc, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_rainl', strm_Faxa_rainl, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_snow', strm_Faxa_snow, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_snowc', strm_Faxa_snowc, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_snowl', strm_Faxa_snowl, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_swdn', strm_Faxa_swdn, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_swnet', strm_Faxa_swnet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_lwdn', strm_Faxa_lwdn, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_lwnet', strm_Faxa_lwnet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_sen', strm_Faxa_sen, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_lat', strm_Faxa_lat, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_taux', strm_Faxa_taux, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_tauy', strm_Faxa_tauy, rc=rc)
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
    call dshr_state_getfldptr(exportState, 'Sa_u'       , fldptr1=Sa_u       , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_v'       , fldptr1=Sa_v       , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_wspd'    , fldptr1=Sa_wspd    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_t2m'     , fldptr1=Sa_t2m     , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_tskn'    , fldptr1=Sa_tskn    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_tbot'    , fldptr1=Sa_tbot    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_ptem'    , fldptr1=Sa_ptem    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_q2m'     , fldptr1=Sa_q2m     , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_shum'    , fldptr1=Sa_shum    , allowNullReturn=.true., rc=rc) 
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_pslv'    , fldptr1=Sa_pslv    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_pbot'    , fldptr1=Sa_pbot    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_dens'    , fldptr1=Sa_dens    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_rain'  , fldptr1=Faxa_rain  , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_rainc' , fldptr1=Faxa_rainc , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_rainl' , fldptr1=Faxa_rainl , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_snow'  , fldptr1=Faxa_snow  , allowNullReturn=.true., rc=rc)
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

    ! Error checks
    if (.not. skip_field_check) then
       if (.not. associated(strm_Sa_tdew)) then
          call shr_log_error(subname//'ERROR: strm_Sa_tdew must be associated for era5 datamode')
          return
       end if
       if (associated(Sa_wspd10m) .and. .not. associated(strm_Sa_u10m)) then
          call shr_log_error(subname//'ERROR: strm_Sa_u10m must be associated for era5 datamode', rc=rc)
          return
       end if
       if (associated(Sa_wspd10m) .and. .not. associated(strm_Sa_v10m)) then
          call shr_log_error(subname//'ERROR: strm_Sa_v10m must be associated for era5 datamode', rc=rc)
          return
       end if
       if (associated(Sa_wspd) .and. .not. associated(strm_Sa_u)) then
          call shr_log_error(subname//'ERROR: strm_Sa_u must be associated for era5 datamode', rc=rc)
          return
       end if
       if (associated(Sa_wspd) .and. .not. associated(strm_Sa_v)) then
          call shr_log_error(subname//'ERROR: strm_Sa_v must be associated for era5 datamode', rc=rc)
          return
       end if
       if (associated(Sa_t2m) .and. .not. associated(strm_Sa_t2m)) then
          call shr_log_error(subname//'ERROR: strm_Sa_t2m must be associated for era5 datamode', rc=rc)
          return
       end if
       if (associated(Sa_t2m) .and. associated(Sa_pslv) .and. associated(Sa_q2m) .and. .not. associated(strm_Sa_pslv)) then
          call shr_log_error(subname//'ERROR: strm_Sa_pslv must be associated for era5 datamode', rc=rc)
          return
       end if
       if (associated(Faxa_swdn)) then
          if (.not. associated(strm_Faxa_swdn)) then
             call shr_log_error(subname//'ERROR: strm_Faxa_swdn must be associated for era5 datamode', rc=rc)
             return
          end if
       end if
       if ( associated(Faxa_swvdr) .or. associated(Faxa_swndr) .or. associated(Faxa_swvdf) .or. associated(Faxa_swndf)) then
          if (.not. associated(strm_Faxa_swdn)) then
             call shr_log_error(subname//'ERROR: strm_Faxa_swdn must be associated for era5 datamode', rc=rc)
             return
          end if
       end if
       if (associated(Faxa_lwdn) .and. .not. associated(strm_Faxa_lwdn)) then
          call shr_log_error(subname//'ERROR: strm_Faxa_lwdn must be associated for era5 datamode', rc=rc)
          return
       end if
       if (associated(Faxa_lwnet) .and. .not. associated(strm_Faxa_lwnet)) then
          call shr_log_error(subname//'ERROR: strm_Faxa_lwnet must be associated for era5 datamode', rc=rc)
          return
       end if
       if (associated(Faxa_swnet) .and. .not. associated(strm_Faxa_swnet)) then
          call shr_log_error(subname//'ERROR: strm_Faxa_swnet must be associated for era5 datamode', rc=rc)
          return
       end if
       if (associated(Faxa_sen) .and. .not. associated(strm_Faxa_sen)) then
          call shr_log_error(subname//'ERROR: strm_Faxa_sen must be associated for era5 datamode', rc=rc)
          return
       end if
       if (associated(Faxa_lat) .and. .not. associated(strm_Faxa_lat)) then
          call shr_log_error(subname//'ERROR: strm_Faxa_lat must be associated for era5 datamode', rc=rc)
          return
       end if
       if (associated(Faxa_rain) .and. .not. associated(strm_Faxa_rain)) then
          call shr_log_error(subname//'ERROR: strm_Faxa_rain must be associated for era5 datamode', rc=rc)
          return
       end if
       if (associated(Faxa_rainc) .and. .not. associated(strm_Faxa_rainc)) then
          call shr_log_error(subname//'ERROR: strm_Faxa_rainc must be associated for era5 datamode', rc=rc)
          return
       end if
       if (associated(Faxa_rainl) .and. .not. associated(strm_Faxa_rainl)) then
          call shr_log_error(subname//'ERROR: strm_Faxa_rainl must be associated for era5 datamode', rc=rc)
          return
       end if
       if (associated(Faxa_snowc) .and. .not. associated(strm_Faxa_snowc)) then
          call shr_log_error(subname//'ERROR: strm_Faxa_snowc must be associated for era5 datamode', rc=rc)
          return
       end if
       if (associated(Faxa_snowl) .and. .not. associated(strm_Faxa_snowl)) then
          call shr_log_error(subname//'ERROR: strm_Faxa_snowl must be associated for era5 datamode', rc=rc)
          return
       end if
       if (associated(Faxa_taux) .and. .not. associated(strm_Faxa_taux)) then
          call shr_log_error(subname//'ERROR: strm_Faxa_taux must be associated for era5 datamode', rc=rc)
          return
       end if
       if (associated(Faxa_tauy) .and. .not. associated(strm_Faxa_tauy)) then
          call shr_log_error(subname//'ERROR: strm_Faxa_tauy must be associated for era5 datamode', rc=rc)
          return
       end if
    end if

    ! Initialize value of export state
    if (associated(Faxa_rain)) Faxa_rain(:) = 0.0_r8
    if (associated(Faxa_rainc)) Faxa_rainc(:) = 0.0_r8
    if (associated(Faxa_rainl)) Faxa_rainl(:) = 0.0_r8
    if (associated(Faxa_snow)) Faxa_snow(:) = 0.0_r8
    if (associated(Faxa_snowc)) Faxa_snowc(:) = 0.0_r8
    if (associated(Faxa_snowl)) Faxa_snowl(:) = 0.0_r8

  end subroutine datm_datamode_era5_init_pointers

  !===============================================================================
  subroutine datm_datamode_era5_advance(exportstate, mainproc, logunit, target_ymd, target_tod, model_calendar, rc)

    use ESMF, only: ESMF_VMGetCurrent, ESMF_VMAllReduce, ESMF_REDUCE_MAX, ESMF_VM

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    logical                , intent(in)    :: mainproc
    integer                , intent(in)    :: logunit
    integer                , intent(in)    :: target_ymd
    integer                , intent(in)    :: target_tod
    character(len=*)       , intent(in)    :: model_calendar
    integer                , intent(out)   :: rc

    ! local variables
    logical  :: first_time = .true.
    integer  :: n                   ! indices
    integer  :: lsize               ! size of attr vect
    real(r8) :: avg_alb             ! average albedo
    real(r8) :: rday                ! elapsed day
    real(r8) :: cosFactor           ! cosine factor
    real(r8) :: rtmp(2)
    real(r8) :: t2, pslv
    real(r8) :: e, qsat
    real(r8) :: tbot, pbot
    type(ESMF_VM) :: vm
    character(len=*), parameter :: subname='(datm_datamode_era5_advance): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    if (associated(Sa_z)) then
       lsize = size(Sa_z)
    else
       if (mainproc) write(logunit,*) subname,' Sa_z is not given. Try with Sa_pslv to get stream size'
    end if
    if (associated(Sa_pslv)) then
       lsize = size(Sa_pslv)
    else
       if (mainproc) write(logunit,*) subname,' Sa_pslv is also not given. Exiting!'
       call shr_log_error(subname//'ERROR: Sa_z and/or Sa_pslv must be associated for era5 datamode', rc=rc)
       return
    end if

    call shr_cal_date2julian(target_ymd, target_tod, rday, model_calendar)
    rday = mod((rday - 1.0_R8),365.0_R8)
    cosfactor = cos((2.0_R8*SHR_CONST_PI*rday)/365 - phs_c0)

    if (first_time) then
       call ESMF_VMGetCurrent(vm, rc=rc)
       ! determine t2max (see below for use)
       if (associated(strm_Sa_t2m)) then
          rtmp(1) = maxval(strm_Sa_t2m(:))

          call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
          t2max = rtmp(2)
          if (mainproc) write(logunit,*) subname,' t2max = ',t2max
       end if

       ! determine tdewmax (see below for use)
       if (associated(strm_Sa_tdew)) then
          rtmp(1) = maxval(strm_Sa_tdew(:))

          call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
          td2max = rtmp(2)
          if (mainproc) write(logunit,*) subname,' td2max = ',td2max
       end if

       ! determine lwmax
       if (associated(strm_Faxa_lwdn)) then
          rtmp(1) = maxval(strm_Faxa_lwdn(:))

          call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
          lwmax = rtmp(2)
          if (mainproc) write(logunit,*) trim(subname),' lwmax = ',lwmax
       else
          ! try with other variable since Faxa_lwdn is not available
          if (associated(strm_Faxa_lwnet)) then
             rtmp(1) = maxval(strm_Faxa_lwnet(:))

             call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
             lwmax = rtmp(2)
             if (mainproc) write(logunit,*) trim(subname),' lwmax = ',lwmax
          else
             lwmax = 0.0_r8
          end if
       end if

       ! determine precmax
       if (associated(strm_Faxa_rain)) then
          rtmp(1) = maxval(strm_Faxa_rain(:))

          call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
          precmax = rtmp(2)
          if (mainproc) write(logunit,*) trim(subname),' precmax = ', precmax
       else
          ! try with other variable since Faxa_rain is not available
          if (associated(strm_Faxa_rainl)) then
            rtmp(1) = maxval(strm_Faxa_rainl(:))

            call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
            precmax = rtmp(2)
            if (mainproc) write(logunit,*) trim(subname),' precmax = ', precmax
          else
            precmax = 0.0_r8
          end if
       end if

       ! reset first_time
       first_time = .false.
    end if

    ! direct copy from stream
    if (associated(strm_Sa_tbot)) then
       if (associated(Sa_tbot)) Sa_tbot(:) = strm_Sa_tbot(:)
       if (associated(Sa_ptem)) Sa_ptem(:) = strm_Sa_tbot(:)
    end if
    if (associated(strm_Sa_t2m) .and. associated(Sa_t2m)) Sa_t2m(:) = strm_Sa_t2m(:)
    if (associated(strm_Sa_q2m) .and. associated(Sa_q2m)) Sa_q2m(:) = strm_Sa_q2m(:)
    if (associated(strm_Sa_shum) .and. associated(Sa_shum)) Sa_shum(:) = strm_Sa_shum(:)
    if (associated(strm_Sa_pbot) .and. associated(Sa_pbot)) Sa_pbot(:) = strm_Sa_pbot(:)
    if (associated(strm_Sa_pslv) .and. associated(Sa_pslv)) Sa_pslv(:) = strm_Sa_pslv(:)
    if (associated(strm_Sa_u) .and. associated(Sa_u)) Sa_u(:) = strm_Sa_u(:)
    if (associated(strm_Sa_v) .and. associated(Sa_v)) Sa_v(:) = strm_Sa_v(:)
    if (associated(strm_Sa_u10m) .and. associated(Sa_u10m)) Sa_u10m(:) = strm_Sa_u10m(:)
    if (associated(strm_Sa_v10m) .and. associated(Sa_v10m)) Sa_v10m(:) = strm_Sa_v10m(:)
    if (associated(strm_Faxa_rain) .and. associated(Faxa_rain)) Faxa_rain(:) = strm_Faxa_rain(:)
    if (associated(strm_Faxa_rainc) .and. associated(Faxa_rainc)) Faxa_rainc(:) = strm_Faxa_rainc(:)
    if (associated(strm_Faxa_rainl) .and. associated(Faxa_rainl)) Faxa_rainl(:) = strm_Faxa_rainl(:)
    if (associated(strm_Faxa_snow) .and. associated(Faxa_snow)) Faxa_snow(:) = strm_Faxa_snow(:)
    if (associated(strm_Faxa_snowc) .and. associated(Faxa_snowc)) Faxa_snowc(:) = strm_Faxa_snowc(:)
    if (associated(strm_Faxa_snowl) .and. associated(Faxa_snowl)) Faxa_snowl(:) = strm_Faxa_snowl(:)
    if (associated(strm_Faxa_swdn) .and. associated(Faxa_swdn)) Faxa_swdn(:) = strm_Faxa_swdn(:)
    if (associated(strm_Faxa_lwdn) .and. associated(Faxa_lwdn)) Faxa_lwdn(:) = strm_Faxa_lwdn(:)
    if (associated(strm_Faxa_swnet) .and. associated(Faxa_swnet)) Faxa_swnet(:) = strm_Faxa_swnet(:)
    if (associated(strm_Faxa_lwnet) .and. associated(Faxa_lwnet)) Faxa_lwnet(:) = strm_Faxa_lwnet(:)
    if (associated(strm_Faxa_sen) .and. associated(Faxa_sen)) Faxa_sen(:) = strm_Faxa_sen(:)
    if (associated(strm_Faxa_lat) .and. associated(Faxa_lat)) Faxa_lat(:) = strm_Faxa_lat(:)
    if (associated(strm_Faxa_taux) .and. associated(Faxa_taux)) Faxa_taux(:) = strm_Faxa_taux(:)
    if (associated(strm_Faxa_tauy) .and. associated(Faxa_tauy)) Faxa_tauy(:) = strm_Faxa_tauy(:)

    do n = 1, lsize
       !--- bottom layer height ---
       if (associated(Sa_z)) then
          Sa_z(n) = 10.0_r8
       end if

       !--- calculate wind components if wind speed is provided ---
       if (associated(strm_Sa_wspd)) then
          Sa_u(n) = strm_Sa_wspd(n)/sqrt(2.0_r8)
          Sa_v(n) = Sa_u(n)
       end if
       if (associated(strm_Sa_wspd10m)) then
          Sa_u10m(n) = strm_Sa_wspd10m(n)/sqrt(2.0_r8)
          Sa_v10m(n) = Sa_u10m(n)
       end if

       !--- calculate wind speed if wind components are provided ---
       if (associated(Sa_wspd10m) .and. associated(strm_Sa_u10m) .and. associated(strm_Sa_v10m)) then
          Sa_wspd10m(n) = sqrt(strm_Sa_u10m(n)*strm_Sa_u10m(n) + strm_Sa_v10m(n)*strm_Sa_v10m(n))
       end if
       if (associated(Sa_wspd) .and. associated(strm_Sa_u) .and. associated(strm_Sa_v)) then
          Sa_wspd(n) = sqrt(strm_Sa_u(n)*strm_Sa_u(n) + strm_Sa_v(n)*strm_Sa_v(n))
       end if

       !--- calculate specific humidity from dew point temperature ---
       if (associated(strm_Sa_tdew)) then
          if (associated(strm_Sa_t2m)) then
             tbot = strm_Sa_t2m(n)
          else if (associated(strm_Sa_tbot)) then
             tbot = strm_Sa_tbot(n)
          end if

          if (associated(strm_Sa_pslv)) then
             pbot = strm_Sa_pslv(n)
          else if (associated(strm_Sa_pbot)) then
             pbot = strm_Sa_pbot(n)
          end if

          if (td2max < 50.0_r8) strm_Sa_tdew(n) = strm_Sa_tdew(n) + tkFrz
          e = datm_eSat(strm_Sa_tdew(n), tbot)
          qsat = (0.622_r8 * e)/(pbot - 0.378_r8 * e)
          if (associated(Sa_q2m)) Sa_q2m(n) = qsat
          if (associated(Sa_shum)) Sa_shum(n) = qsat
       end if

       !--- calculate density --
       if (associated(strm_Sa_pbot) .and. associated(strm_Sa_tbot) .and. associated(strm_Sa_shum)) then
          if (associated(Sa_dens)) then
             Sa_dens(n) = strm_Sa_pbot(n)/(rdair*strm_Sa_tbot(n)*(1 + 0.608*strm_Sa_shum(n)))
          end if
       end if

       !--- radiation data ---
       ! fabricate required swdn components from net swdn
       if (associated(strm_Faxa_swdn)) then
          if (associated(Faxa_swvdr)) Faxa_swvdr(n) = strm_Faxa_swdn(n)*(0.28_R8)
          if (associated(Faxa_swndr)) Faxa_swndr(n) = strm_Faxa_swdn(n)*(0.31_R8)
          if (associated(Faxa_swvdf)) Faxa_swvdf(n) = strm_Faxa_swdn(n)*(0.24_R8)
          if (associated(Faxa_swndf)) Faxa_swndf(n) = strm_Faxa_swdn(n)*(0.17_R8)
       end if

       ! compute net short-wave based on LY08 latitudinally-varying albedo
       if (associated(strm_Faxa_swdn)) then
          if (.not. associated(strm_Faxa_swnet) .and. associated(Faxa_swnet)) then
             avg_alb = ( 0.069 - 0.011*cos(2.0_R8*yc(n)*degtorad ) )
             Faxa_swnet(n) = strm_Faxa_swdn(n)*(1.0_R8 - avg_alb)
             Faxa_swnet(n) = Faxa_swnet(n)*3600.0_r8 ! to J/m^2
          end if
       end if
    end do

    !----------------------------------------------------------
    ! unit conversions (temporal resolution is hourly)
    !----------------------------------------------------------

    ! convert J/m^2 to W/m^2
    if (lwmax < 1.0e4_r8) then
       if (mainproc) write(logunit,*) trim(subname),' flux related variables are already in W/m^2 unit!'
    else
       if (associated(Faxa_lwdn))  Faxa_lwdn(:)  = Faxa_lwdn(:) /3600.0_r8
       if (associated(Faxa_lwnet)) Faxa_lwnet(:) = Faxa_lwnet(:)/3600.0_r8
       if (associated(Faxa_swvdr)) Faxa_swvdr(:) = Faxa_swvdr(:)/3600.0_r8
       if (associated(Faxa_swndr)) Faxa_swndr(:) = Faxa_swndr(:)/3600.0_r8
       if (associated(Faxa_swvdf)) Faxa_swvdf(:) = Faxa_swvdf(:)/3600.0_r8
       if (associated(Faxa_swndf)) Faxa_swndf(:) = Faxa_swndf(:)/3600.0_r8
       if (associated(Faxa_swdn))  Faxa_swdn(:)  = Faxa_swdn(:) /3600.0_r8
       if (associated(Faxa_swnet)) Faxa_swnet(:) = Faxa_swnet(:)/3600.0_r8
       if (associated(Faxa_sen))   Faxa_sen(:)   = Faxa_sen(:)  /3600.0_r8
       if (associated(Faxa_lat))   Faxa_lat(:)   = Faxa_lat(:)  /3600.0_r8
    end if

    ! convert m to kg/m^2/s
    if (precmax < 0.01_r8) then
       if (mainproc) write(logunit,*) trim(subname),' precipitation related variables are already in kg/m^2/s unit!'
    else
       if (associated(Faxa_rain))  Faxa_rain(:)  = Faxa_rain(:)/3600.0_r8*rhofw
       if (associated(Faxa_rainc)) Faxa_rainc(:) = Faxa_rainc(:)/3600.0_r8*rhofw
       if (associated(Faxa_rainl)) Faxa_rainl(:) = Faxa_rainl(:)/3600.0_r8*rhofw
       if (associated(Faxa_snowc)) Faxa_snowc(:) = Faxa_snowc(:)/3600.0_r8*rhofw
       if (associated(Faxa_snowl)) Faxa_snowl(:) = Faxa_snowl(:)/3600.0_r8*rhofw
    end if

    ! convert N/m^2 s to N/m^2
    if (lwmax < 1.0e4_r8) then
       if (mainproc) write(logunit,*) trim(subname),' momentum flux related variables are already in N/m^2 unit!'
    else
       if (associated(Faxa_taux)) Faxa_taux(:) = Faxa_taux(:)/3600.0_r8
       if (associated(Faxa_tauy)) Faxa_tauy(:) = Faxa_tauy(:)/3600.0_r8
    end if

  end subroutine datm_datamode_era5_advance

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

end module datm_datamode_era5_mod
