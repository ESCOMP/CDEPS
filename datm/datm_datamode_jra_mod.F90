module datm_datamode_jra_mod

  use ESMF             , only : ESMF_State, ESMF_StateGet, ESMF_SUCCESS, ESMF_LogWrite, ESMF_LOGMSG_INFO
  use ESMF             , only : ESMF_MeshGet
  use ESMF             , only : ESMF_StateItem_Flag, ESMF_STATEITEM_NOTFOUND, operator(/=)
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_log_mod      , only : shr_log_error
  use shr_cal_mod      , only : shr_cal_date2julian
  use shr_const_mod    , only : shr_const_tkfrz, shr_const_pi, shr_const_rdair
  use dshr_strdata_mod , only : shr_strdata_get_stream_pointer, shr_strdata_type
  use dshr_methods_mod , only : dshr_state_getfldptr, dshr_fldbun_getfldptr, dshr_fldbun_regrid, chkerr
  use dshr_strdata_mod , only : shr_strdata_type
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add

  implicit none
  private ! except

  public  :: datm_datamode_jra_advertise
  public  :: datm_datamode_jra_init_pointers
  public  :: datm_datamode_jra_advance

  ! export state pointers
  real(r8), pointer :: Sa_z(:)       => null()
  real(r8), pointer :: Sa_u(:)       => null()
  real(r8), pointer :: Sa_v(:)       => null()
  real(r8), pointer :: Sa_u10m(:)    => null()
  real(r8), pointer :: Sa_v10m(:)    => null()
  real(r8), pointer :: Sa_tbot(:)    => null()
  real(r8), pointer :: Sa_ptem(:)    => null()
  real(r8), pointer :: Sa_shum(:)    => null()
  real(r8), pointer :: Sa_dens(:)    => null()
  real(r8), pointer :: Sa_pbot(:)    => null()
  real(r8), pointer :: Sa_pslv(:)    => null()
  real(r8), pointer :: Faxa_rainc(:) => null()
  real(r8), pointer :: Faxa_rainl(:) => null()
  real(r8), pointer :: Faxa_snowc(:) => null()
  real(r8), pointer :: Faxa_snowl(:) => null()
  real(r8), pointer :: Faxa_swndr(:) => null()
  real(r8), pointer :: Faxa_swndf(:) => null()
  real(r8), pointer :: Faxa_swvdr(:) => null()
  real(r8), pointer :: Faxa_swvdf(:) => null()
  real(r8), pointer :: Faxa_swnet(:) => null()

  ! stream data
  real(r8), pointer :: strm_Sa_tbot(:)   => null()
  real(r8), pointer :: strm_Sa_pslv(:)   => null()
  real(r8), pointer :: strm_Sa_u(:)      => null()
  real(r8), pointer :: strm_Sa_v(:)      => null()
  real(r8), pointer :: strm_Sa_shum(:)   => null()
  real(r8), pointer :: strm_Faxa_prec(:) => null()
  real(r8), pointer :: strm_Faxa_lwdn(:) => null()
  real(r8), pointer :: strm_Faxa_swdn(:) => null()

  ! othe module arrays
  real(R8), pointer :: yc(:) ! array of model latitudes

  ! constants
  real(R8) , parameter :: tKFrz    = SHR_CONST_TKFRZ
  real(R8) , parameter :: rdair    = SHR_CONST_RDAIR    ! dry air gas constant   ~ J/K/kg
  real(R8) , parameter :: degtorad = SHR_CONST_PI/180.0_R8
  real(R8) , parameter :: phs_c0   =   0.298_R8
  real(R8) , parameter :: dLWarc   =  -5.000_R8

  character(*), parameter :: nullstr = 'null'
  character(*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine datm_datamode_jra_advertise(exportState, fldsexport, flds_scalar_name, rc)

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
    call dshr_fldList_add(fldsExport, 'Sa_u10m'    )
    call dshr_fldList_add(fldsExport, 'Sa_v10m'    )
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

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(datm_comp_advertise): Fr_atm'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine datm_datamode_jra_advertise

  !===============================================================================
  subroutine datm_datamode_jra_init_pointers(exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    integer           :: n
    integer           :: lsize
    integer           :: spatialDim         ! number of dimension in mesh
    integer           :: numOwnedElements   ! size of mesh
    real(r8), pointer :: ownedElemCoords(:) ! mesh lat and lons
    character(len=*), parameter :: subname='(datm_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    lsize = sdat%model_lsize

    call ESMF_MeshGet(sdat%model_mesh, spatialDim=spatialDim, numOwnedElements=numOwnedElements, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    allocate(ownedElemCoords(spatialDim*numOwnedElements))
    allocate(yc(numOwnedElements))
    call ESMF_MeshGet(sdat%model_mesh, ownedElemCoords=ownedElemCoords)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    do n = 1,numOwnedElements
       yc(n) = ownedElemCoords(2*n)
    end do

    ! Stream pointers
    call shr_strdata_get_stream_pointer( sdat, 'Sa_tbot'   , strm_Sa_tbot   , rc) ! required
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Sa_pslv'   , strm_Sa_pslv   , rc) ! required
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Sa_u'      , strm_Sa_u      , rc) ! required
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Sa_v'      , strm_Sa_v      , rc) ! required
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Sa_shum'   , strm_Sa_shum   , rc) ! required
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_prec' , strm_Faxa_prec , rc) ! required
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_lwdn' , strm_Faxa_lwdn , rc) ! required
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_swdn' , strm_Faxa_swdn , rc) ! required
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Export state pointers
    call dshr_state_getfldptr(exportState, 'Sa_u'       , fldptr1=Sa_u       , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_v'       , fldptr1=Sa_v       , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_u10m'    , fldptr1=Sa_u10m    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_v10m'    , fldptr1=Sa_v10m    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_z'       , fldptr1=Sa_z       , rc=rc)
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

    ! error check
    if (.not. associated(strm_Sa_tbot)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Sa_tbot required for CORE_IAF_JRA', rc=rc)
       return
    endif
    if (.not. associated(strm_Sa_pslv)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Sa_pslv required for CORE_IAF_JRA', rc=rc)
       return
    endif
    if (.not. associated(strm_Sa_u)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Sa_u required for CORE_IAF_JRA', rc=rc)
       return
    endif
    if (.not. associated(strm_Sa_v)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Sa_v required for CORE_IAF_JRA', rc=rc)
       return
    endif
    if (.not. associated(strm_Sa_shum)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Sa_shum required for CORE_IAF_JRA', rc=rc)
       return
    endif
    if (.not. associated(strm_Faxa_prec)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_prec required for CORE_IAF_JRA', rc=rc)
       return
    endif
    if (.not. associated(strm_Faxa_lwdn)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_lwdn required for CORE_IAF_JRA', rc=rc)
       return
    endif
    if (.not. associated(strm_Faxa_swdn)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_swdn required for CORE_IAF_JRA', rc=rc)
       return
    endif

  end subroutine datm_datamode_jra_init_pointers

  !===============================================================================
  subroutine datm_datamode_jra_advance(exportstate, target_ymd, target_tod, model_calendar, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    integer                , intent(in)    :: target_ymd
    integer                , intent(in)    :: target_tod
    character(len=*)       , intent(in)    :: model_calendar
    integer                , intent(out)   :: rc

    ! local variables
    integer           :: n
    integer           :: lsize
    real(R8)          :: avg_alb            ! average albedo
    real(R8)          :: rday               ! elapsed day
    real(R8)          :: cosFactor          ! cosine factor
    character(len=*), parameter :: subname='(datm_datamode_jra): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    lsize = size(Sa_z)

    call shr_cal_date2julian(target_ymd, target_tod, rday, model_calendar)
    rday = mod((rday - 1.0_R8),365.0_R8)
    cosfactor = cos((2.0_R8*SHR_CONST_PI*rday)/365 - phs_c0)

    do n = 1,lsize
       Sa_z(n) = 10.0_R8

       ! Set export fields as copies directly from streams
       Sa_pslv(n) = strm_Sa_pslv(n)
       Sa_tbot(n) = strm_Sa_tbot(n)
       Sa_u(n)    = strm_Sa_u(n)
       Sa_v(n)    = strm_Sa_v(n)
       Sa_shum(n) = strm_Sa_shum(n)

       ! Set Sa_pbot from Sa_pslv
       Sa_pbot(n) = Sa_pslv(n)

       ! Set Sa_ptem from Sa_tbot
       Sa_ptem(n) = Sa_tbot(n)

       ! Set Sa_u10m and Sa_v10m to Sa_u and Sa_v
       Sa_u10m(n) = Sa_u(n)
       Sa_v10m(n) = Sa_v(n)

       ! density computation for JRA55 forcing
       Sa_dens(n) = Sa_pbot(n)/(rdair*Sa_tbot(n)*(1 + 0.608*Sa_shum(n)))

       ! precipitation data
       Faxa_rainc(n) = 0.0_R8               ! default zero
       Faxa_snowc(n) = 0.0_R8
       if (Sa_tbot(n) < tKFrz ) then        ! assign precip to rain/snow components
          Faxa_rainl(n) = 0.0_R8
          Faxa_snowl(n) = strm_Faxa_prec(n)
       else
          Faxa_rainl(n) = strm_Faxa_prec(n)
          Faxa_snowl(n) = 0.0_R8
       endif

       ! radiation data - fabricate required swdn components from net swdn
       Faxa_swvdr(n) = strm_Faxa_swdn(n)*(0.28_R8)
       Faxa_swndr(n) = strm_Faxa_swdn(n)*(0.31_R8)
       Faxa_swvdf(n) = strm_Faxa_swdn(n)*(0.24_R8)
       Faxa_swndf(n) = strm_Faxa_swdn(n)*(0.17_R8)

       ! radiation data - compute net short-wave based on LY08 latitudinally-varying albedo
       avg_alb = ( 0.069 - 0.011*cos(2.0_R8*yc(n)*degtorad ) )
       Faxa_swnet(n) = strm_Faxa_swdn(n)*(1.0_R8 - avg_alb)
    enddo   ! lsize

  end subroutine datm_datamode_jra_advance

end module datm_datamode_jra_mod
