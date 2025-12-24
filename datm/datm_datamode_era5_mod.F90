module datm_datamode_era5_mod

  use ESMF             , only : ESMF_State, ESMF_SUCCESS, ESMF_LogWrite, ESMF_LOGMSG_INFO
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_const_mod    , only : shr_const_tkfrz, shr_const_rhofw, shr_const_rdair
  use shr_log_mod      , only : shr_log_error
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add

  implicit none
  private ! except

  public  :: datm_datamode_era5_advertise
  public  :: datm_datamode_era5_init_pointers
  public  :: datm_datamode_era5_advance
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
  real(r8), pointer :: strm_Sa_tdew(:)    => null()
  real(r8), pointer :: strm_Sa_t2m(:)     => null()
  real(r8), pointer :: strm_Sa_u10m(:)    => null()
  real(r8), pointer :: strm_Sa_v10m(:)    => null()
  real(r8), pointer :: strm_Sa_pslv(:)    => null()
  real(r8), pointer :: strm_Faxa_swdn(:)  => null()
  real(r8), pointer :: strm_Faxa_swvdr(:) => null()
  real(r8), pointer :: strm_Faxa_swndr(:) => null()
  real(r8), pointer :: strm_Faxa_swvdf(:) => null()
  real(r8), pointer :: strm_Faxa_swndf(:) => null()
  real(r8), pointer :: strm_Faxa_swnet(:) => null()
  real(r8), pointer :: strm_Faxa_lwdn(:)  => null()
  real(r8), pointer :: strm_Faxa_lwnet(:) => null()
  real(r8), pointer :: strm_Faxa_rain(:)  => null()
  real(r8), pointer :: strm_Faxa_rainc(:) => null()
  real(r8), pointer :: strm_Faxa_rainl(:) => null()
  real(r8), pointer :: strm_Faxa_snowc(:) => null()
  real(r8), pointer :: strm_Faxa_snowl(:) => null()
  real(r8), pointer :: strm_Faxa_sen(:)   => null()
  real(r8), pointer :: strm_Faxa_lat(:)   => null()
  real(r8), pointer :: strm_Faxa_taux(:)  => null()
  real(r8), pointer :: strm_Faxa_tauy(:)  => null()

  real(r8) :: t2max  ! units detector
  real(r8) :: td2max ! units detector

  real(r8) , parameter :: tKFrz    = SHR_CONST_TKFRZ
  real(r8) , parameter :: rdair    = SHR_CONST_RDAIR ! dry air gas constant ~ J/K/kg
  real(r8) , parameter :: rhofw    = SHR_CONST_RHOFW ! density of fresh water ~ kg/m^3

  character(*), parameter :: nullstr = 'undefined'
  character(*), parameter :: u_FILE_u = &
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
    call shr_strdata_get_stream_pointer( sdat,'Sa_tdew', strm_Sa_tdew , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_t2m' , strm_Sa_t2m , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_u10m', strm_Sa_u10m, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_v10m', strm_Sa_v10m, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_pslv', strm_Sa_pslv, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_swdn', strm_Faxa_swdn, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_swvdr', strm_Faxa_swvdr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_swndr', strm_Faxa_swndr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_swvdf', strm_Faxa_swvdf, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_swndf', strm_Faxa_swndf, rc=rc)
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

    ! Error checks
    if (.not. associated(strm_Sa_tdew)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Sa_pslv must be associated for era5 datamode')
       return
    end if

    if (associated(Sa_wspd10m) .and. .not. associated(strm_Sa_u10m)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Sa_u10m must be associated for era5 datamode', rc=rc)
       return
    end if
    if (associated(Sa_wspd10m) .and. .not. associated(strm_Sa_v10m)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Sa_v10m must be associated for era5 datamode', rc=rc)
       return
    end if
    if (associated(Sa_t2m) .and. .not. associated(strm_Sa_t2m)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Sa_t2m must be associated for era5 datamode', rc=rc)
       return
    end if
    if (associated(Sa_t2m) .and. associated(Sa_pslv) .and. associated(Sa_q2m) .and. .not. associated(strm_Sa_pslv)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Sa_pslv must be associated for era5 datamode', rc=rc)
       return
    end if
    if (associated(Faxa_swdn)) then
       if (.not. associated(strm_Faxa_swdn)) then
          call shr_log_error(trim(subname)//'ERROR: strm_Faxa_swdn must be associated for era5 datamode', rc=rc)
          return
       end if
    end if
    if ( associated(Faxa_swvdr) .or. associated(Faxa_swndr) .or. associated(Faxa_swvdf) .or. associated(Faxa_swndf)) then
       if (.not. associated(strm_Faxa_swdn)) then
          call shr_log_error(trim(subname)//'ERROR: strm_Faxa_swdn must be associated for era5 datamode', rc=rc)
          return
       end if
    end if
    if (associated(Faxa_swvdr) .and. .not. associated(strm_Faxa_swvdr)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_swvdr must be associated for era5 datamode', rc=rc)
       return
       end if
    end if
    if (associated(Faxa_swndr) .and. .not. associated(strm_Faxa_swndr)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_swndr must be associated for era5 datamode', rc=rc)
       return
    end if
    if (associated(Faxa_swvdf) .and. .not. associated(strm_Faxa_swvdf)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_swvdf must be associated for era5 datamode', rc=rc)
       return
    end if
    if (associated(Faxa_swndf) .and. not. associated(strm_Faxa_swndf)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_swndf must be associated for era5 datamode', rc=rc)
       return
    end if
    if (associated(Faxa_lwdn) .and. .not. associated(strm_Faxa_lwdn)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_lwdn must be associated for era5 datamode', rc=rc)
       return
    end if
    if (associated(Faxa_lwnet) .and. .not. associated(strm_Faxa_lwnet)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_lwnet must be associated for era5 datamode', rc=rc)
       return
    end if
    if (associated(Faxa_swnet) .and. .not. associated(strm_Faxa_swnet)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_swnet must be associated for era5 datamode', rc=rc)
       return
    end if
    if (associated(Faxa_sen) .and. .not. associated(strm_Faxa_sen)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_sen must be associated for era5 datamode', rc=rc)
       return
    end if
    if (associated(Faxa_lat) .and. .not. associated(strm_Faxa_lat)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_lat must be associated for era5 datamode', rc=rc)
       return
    end if
    if (associated(Faxa_rain) .and. .not. associated(strm_Faxa_rain)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_rain must be associated for era5 datamode', rc=rc)
       return
    end if
    if (associated(Faxa_rainc) .and. .not. associated(strm_Faxa_rainc)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_rainc must be associated for era5 datamode', rc=rc)
       return
    end if
    if (associated(Faxa_rainl) .and. .not. associated(strm_Faxa_rainl)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_rainl must be associated for era5 datamode', rc=rc)
       return
    end if
    if (associated(Faxa_snowc) .and. .not. associated(strm_Faxa_snowc)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_snowc must be associated for era5 datamode', rc=rc)
       return
    end if
    if (associated(Faxa_snowl) .and. .not. associated(strm_Faxa_snowl)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_snowl must be associated for era5 datamode', rc=rc)
       return
    end if
    if (associated(Faxa_taux) .and. .not. associated(strm_Faxa_taux)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_taux must be associated for era5 datamode', rc=rc)
       return
    end if
    if (associated(Faxa_tauy) .and. .not. associated(strm_Faxa_tauy)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_tauy must be associated for era5 datamode', rc=rc)
       return
    end if

  end subroutine datm_datamode_era5_init_pointers

  !===============================================================================
  subroutine datm_datamode_era5_advance(exportstate, mainproc, logunit, rc)

    use ESMF, only: ESMF_VMGetCurrent, ESMF_VMAllReduce, ESMF_REDUCE_MAX, ESMF_VM

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    logical                , intent(in)    :: mainproc
    integer                , intent(in)    :: logunit
    integer                , intent(out)   :: rc

    ! local variables
    logical  :: first_time = .true.
    integer  :: n                   ! indices
    integer  :: lsize               ! size of attr vect
    real(r8) :: rtmp(2)
    real(r8) :: t2, pslv
    real(r8) :: e, qsat
    type(ESMF_VM) :: vm
    character(len=*), parameter :: subname='(datm_datamode_era5_advance): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    lsize = size(strm_Sa_tdew)
    if (first_time) then
       call ESMF_VMGetCurrent(vm, rc=rc)
       ! determine t2max (see below for use)
       if (associated(Sa_t2m)) then
         Sa_t2m(:) = strm_Sa_t2m(:)
         rtmp(1) = maxval(Sa_t2m(:))

         call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
         t2max = rtmp(2)
         if (mainproc) write(logunit,*) trim(subname),' t2max = ',t2max
       end if

       ! determine tdewmax (see below for use)
       rtmp(1) = maxval(strm_Sa_tdew(:))
       call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
       td2max = rtmp(2)

       if (mainproc) write(logunit,*) trim(subname),' td2max = ',td2max

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
         Sa_wspd10m(n) = sqrt(strm_Sa_u10m(n)*strm_Sa_u10m(n) + strm_Sa_v10m(n)*strm_Sa_v10m(n))
       end if

       !--- specific humidity at 2m ---
       if (associated(Sa_t2m) .and. associated(Sa_pslv) .and. associated(Sa_q2m)) then
         t2 = Sa_t2m(n)
         pslv = strm_Sa_pslv(n)
         if (td2max < 50.0_r8) strm_Sa_tdew(n) = strm_Sa_tdew(n) + tkFrz
         e = datm_eSat(strm_Sa_tdew(n), t2)
         qsat = (0.622_r8 * e)/(pslv - 0.378_r8 * e)
         Sa_q2m(n) = qsat
       end if
    end do

    !----------------------------------------------------------
    ! shortwave bands
    !----------------------------------------------------------

    !--- shortwave radiation (Faxa_* basically holds albedo) ---
    !--- see comments for Faxa_swnet
    if (associated(Faxa_swvdr)) Faxa_swvdr(:) = strm_Faxa_swdn(:)*strm_Faxa_swvdr(:)
    if (associated(Faxa_swndr)) Faxa_swndr(:) = strm_Faxa_swdn(:)*strm_Faxa_swndr(:)
    if (associated(Faxa_swvdf)) Faxa_swvdf(:) = strm_Faxa_swdn(:)*strm_Faxa_swvdf(:)
    if (associated(Faxa_swndf)) Faxa_swndf(:) = strm_Faxa_swdn(:)*strm_Faxa_swndf(:)

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
    if (associated(Faxa_lwdn))  Faxa_lwdn(:)  = strm_Faxa_lwdn(:)/3600.0_r8
    if (associated(Faxa_lwnet)) Faxa_lwnet(:) = strm_Faxa_lwnet(:)/3600.0_r8
    if (associated(Faxa_swvdr)) Faxa_swvdr(:) = strm_Faxa_swvdr(:)/3600.0_r8
    if (associated(Faxa_swndr)) Faxa_swndr(:) = strm_Faxa_swndr(:)/3600.0_r8
    if (associated(Faxa_swvdf)) Faxa_swvdf(:) = strm_Faxa_swvdf(:)/3600.0_r8
    if (associated(Faxa_swndf)) Faxa_swndf(:) = strm_Faxa_swndf(:)/3600.0_r8
    if (associated(Faxa_swdn))  Faxa_swdn(:)  = strm_Faxa_swdn(:)/3600.0_r8
    if (associated(Faxa_swnet)) Faxa_swnet(:) = strm_Faxa_swnet(:)/3600.0_r8
    if (associated(Faxa_sen))   Faxa_sen(:)   = strm_Faxa_sen(:)/3600.0_r8
    if (associated(Faxa_lat))   Faxa_lat(:)   = strm_Faxa_lat(:)/3600.0_r8

    ! convert m to kg/m^2/s
    if (associated(Faxa_rain))  Faxa_rain(:)  = strm_Faxa_rain(:)/3600.0_r8*rhofw
    if (associated(Faxa_rainc)) Faxa_rainc(:) = strm_Faxa_rainc(:)/3600.0_r8*rhofw
    if (associated(Faxa_rainl)) Faxa_rainl(:) = strm_Faxa_rainl(:)/3600.0_r8*rhofw
    if (associated(Faxa_snowc)) Faxa_snowc(:) = strm_Faxa_snowc(:)/3600.0_r8*rhofw
    if (associated(Faxa_snowl)) Faxa_snowl(:) = strm_Faxa_snowl(:)/3600.0_r8*rhofw

    ! convert N/m^2 s to N/m^2
    if (associated(Faxa_taux))  Faxa_taux(:)  = strm_Faxa_taux(:)/3600.0_r8
    if (associated(Faxa_tauy))  Faxa_tauy(:)  = strm_Faxa_tauy(:)/3600.0_r8

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
