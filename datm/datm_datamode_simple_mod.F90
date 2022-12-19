module datm_datamode_simple_mod

  use ESMF             , only : ESMF_State, ESMF_StateGet, ESMF_Field, ESMF_FieldBundle
  use ESMF             , only : ESMF_DistGrid, ESMF_RouteHandle, ESMF_MeshCreate
  use ESMF             , only : ESMF_Mesh, ESMF_MeshGet, ESMF_MeshCreate
  use ESMF             , only : ESMF_SUCCESS, ESMF_LogWrite, ESMF_FILEFORMAT_ESMFMESH
  use ESMF             , only : ESMF_StateItem_Flag, ESMF_STATEITEM_NOTFOUND, operator(/=)
  use ESMF             , only : ESMF_FieldBundleCreate, ESMF_FieldCreate, ESMF_MESHLOC_ELEMENT
  use ESMF             , only : ESMF_FieldBundleAdd, ESMF_LOGMSG_INFO, ESMF_TYPEKIND_R8
  use ESMF             , only : ESMF_RouteHandleDestroy, ESMF_EXTRAPMETHOD_NEAREST_STOD
  use ESMF             , only : ESMF_POLEMETHOD_ALLAVG, ESMF_REGRIDMETHOD_BILINEAR
  use ESMF             , only : ESMF_DistGridGet, ESMF_FieldRegridStore, ESMF_FieldRedistStore
  use pio              , only : Var_Desc_t, file_desc_t, io_desc_t, pio_read_darray, pio_freedecomp
  use pio              , only : pio_openfile, PIO_NOWRITE, pio_seterrorhandling, PIO_BCAST_ERROR
  use pio              , only : pio_initdecomp, pio_inq_dimlen, pio_inq_varid
  use pio              , only : pio_inq_varndims, pio_inq_vardimid, pio_double
  use pio              , only : pio_closefile
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_sys_mod      , only : shr_sys_abort
  use shr_cal_mod      , only : shr_cal_date2julian
  use shr_mpi_mod      , only : shr_mpi_bcast
  use shr_const_mod    , only : shr_const_tkfrz, shr_const_pi
  use dshr_strdata_mod , only : shr_strdata_get_stream_pointer, shr_strdata_type
  use dshr_methods_mod , only : dshr_state_getfldptr, dshr_fldbun_getfldptr, dshr_fldbun_regrid, chkerr
  use dshr_mod         , only : dshr_restart_read, dshr_restart_write
  use dshr_strdata_mod , only : shr_strdata_type
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add

  implicit none
  private ! except

  public  :: datm_datamode_simple_advertise
  public  :: datm_datamode_simple_init_pointers
  public  :: datm_datamode_simple_advance
  public  :: datm_datamode_simple_restart_write
  public  :: datm_datamode_simple_restart_read

  ! export state pointers
  real(r8), pointer :: Sa_u(:)       => null()
  real(r8), pointer :: Sa_v(:)       => null()
  real(r8), pointer :: Sa_z(:)       => null()
  real(r8), pointer :: Sa_tbot(:)    => null()
  real(r8), pointer :: Sa_ptem(:)    => null()
  real(r8), pointer :: Sa_shum(:)    => null()
  real(r8), pointer :: Sa_pbot(:)    => null()
  real(r8), pointer :: Sa_dens(:)    => null()
  real(r8), pointer :: Sa_pslv(:)    => null()
  real(r8), pointer :: Faxa_lwdn(:)  => null()
  real(r8), pointer :: Faxa_rainc(:) => null()
  real(r8), pointer :: Faxa_rainl(:) => null()
  real(r8), pointer :: Faxa_snowc(:) => null()
  real(r8), pointer :: Faxa_snowl(:) => null()
  real(r8), pointer :: Faxa_swndr(:) => null()
  real(r8), pointer :: Faxa_swndf(:) => null()
  real(r8), pointer :: Faxa_swvdr(:) => null()
  real(r8), pointer :: Faxa_swvdf(:) => null()
  real(r8), pointer :: Faxa_swnet(:) => null()
  real(r8), pointer :: Faxa_ndep(:,:) => null()

  ! stream data
  real(r8), pointer :: strm_swdn(:)      => null()

  ! othe module arrays
  real(R8), pointer :: yc(:)                 ! array of model latitudes

  ! constant forcing values
  real(R8) :: dn10 = 1.204_R8
  real(R8) :: slp = 101325.0_R8
  real(R8) :: q = 0.0_R8
  real(R8) :: t = 273.15_R8
  real(R8) :: u = 0.0_R8
  real(R8) :: v = 0.0_R8

  ! constants
  real(R8) , parameter :: tKFrz    = SHR_CONST_TKFRZ
  real(R8) , parameter :: degtorad = SHR_CONST_PI/180.0_R8
  real(R8) , parameter :: phs_c0   =   0.298_R8
  real(R8) , parameter :: dLWarc   =  -5.000_R8

  character(*), parameter :: nullstr = 'null'
  character(*), parameter :: rpfile  = 'rpointer.atm'
  character(*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine datm_datamode_simple_advertise(exportState, fldsexport, flds_scalar_name, &
    nlfilename, my_task, mpicom, rc)

    ! input/output variables
    type(esmf_State)   , intent(inout) :: exportState
    type(fldlist_type) , pointer       :: fldsexport
    character(len=*)   , intent(in)    :: flds_scalar_name
    character(len=*)   , intent(in)    :: nlfilename
    integer            , intent(in)    :: my_task
    integer            , intent(in)    :: mpicom
    integer            , intent(out)   :: rc

    ! local variables
    type(fldlist_type), pointer   :: fldList
    integer           , parameter :: main_task   = 0 ! task number of main task
    integer                       :: ierr       ! error code
    integer                       :: nu         ! unit number
    character(len=*)  , parameter :: subname='(datm_datamode_simple_advertise): '

    !-------------------------------------------------------------------------------

    namelist / const_forcing_nml / dn10, slp, q, t, u, v

    rc = ESMF_SUCCESS

    ! Read const_forcing_nml from nlfilename
    if (my_task == main_task) then
       open (newunit=nu,file=trim(nlfilename),status="old",action="read")
       read (nu,nml=const_forcing_nml,iostat=ierr)
       close(nu)
       if (ierr > 0) then
          call shr_sys_abort(subName//': namelist read error '//trim(nlfilename))
       end if
    end if

    call shr_mpi_bcast(dn10         , mpicom, 'dn10')
    call shr_mpi_bcast(slp          , mpicom, 'slp')
    call shr_mpi_bcast(q            , mpicom, 'q')
    call shr_mpi_bcast(t            , mpicom, 't')
    call shr_mpi_bcast(u            , mpicom, 'u')
    call shr_mpi_bcast(v            , mpicom, 'v')

    call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
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

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(datm_comp_advertise): Fr_atm'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine datm_datamode_simple_advertise

  !===============================================================================
  subroutine datm_datamode_simple_init_pointers(exportState, sdat, rc)

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
    type(ESMF_StateItem_Flag) :: itemFlag
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

    ! get stream pointers
    !todo call shr_strdata_get_stream_pointer( sdat, 'Faxa_prec'  , strm_prec  , rc)
    !if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_swdn'  , strm_swdn  , rc)
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
    call dshr_state_getfldptr(exportState, 'Sa_dens'    , fldptr1=Sa_dens    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_pslv'    , fldptr1=Sa_pslv    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_ptem'    , fldptr1=Sa_ptem    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_shum'    , fldptr1=Sa_shum    , rc=rc)
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

    if (.not. associated(strm_swdn)) then
       call shr_sys_abort(trim(subname)//'ERROR: swdn must be in streams for SIMPLE')
    endif

  end subroutine datm_datamode_simple_init_pointers

  !===============================================================================
  subroutine datm_datamode_simple_advance(target_ymd, target_tod, target_mon, &
       model_calendar, rc)

    ! input/output variables
    integer                , intent(in)    :: target_ymd
    integer                , intent(in)    :: target_tod
    integer                , intent(in)    :: target_mon
    character(len=*)       , intent(in)    :: model_calendar
    integer                , intent(out)   :: rc

    ! local variables
    integer  :: n
    integer  :: lsize
    real(R8) :: avg_alb            ! average albedo
    real(R8) :: rday               ! elapsed day
    real(R8) :: cosFactor          ! cosine factor
    real(R8) :: factor             ! generic/temporary correction factor
    real(R8) :: tMin               ! minimum temperature
    real(R8) :: uprime,vprime
    character(len=*), parameter :: subname='(datm_datamode_simple): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    lsize = size(Sa_z)

    call shr_cal_date2julian(target_ymd, target_tod, rday, model_calendar)
    rday = mod((rday - 1.0_R8),365.0_R8)
    cosfactor = cos((2.0_R8*SHR_CONST_PI*rday)/365 - phs_c0)

    do n = 1,lsize
      Sa_z(n) = 10.0_R8

      !--- (i) Set forcing fields to constant values read from the namelist file ---
      Sa_dens(n) = dn10
      Sa_pslv(n) = slp
      Sa_pbot(n) = Sa_pslv(n)
      Sa_shum(n) = q
      Sa_tbot(n) = t
      Sa_ptem(n) = Sa_tbot(n)
      Sa_u(n) = u
      Sa_v(n) = v

      !--- (ii) Set precipitation (currently all zeros) ---

      Faxa_rainc(n) = 0.0_R8               ! default zero
      Faxa_snowc(n) = 0.0_R8
      if (Sa_tbot(n) < tKFrz ) then        ! assign precip to rain/snow components
         Faxa_rainl(n) = 0.0_R8
         Faxa_snowl(n) = 0.0_R8 ! todo
      else
         Faxa_rainl(n) = 0.0_R8 ! todo
         Faxa_snowl(n) = 0.0_R8
      endif

      !--- (iii) RADIATION DATA ---

      !--- fabricate required swdn components from net swdn ---
      Faxa_swvdr(n) = strm_swdn(n)*(0.28_R8)
      Faxa_swndr(n) = strm_swdn(n)*(0.31_R8)
      Faxa_swvdf(n) = strm_swdn(n)*(0.24_R8)
      Faxa_swndf(n) = strm_swdn(n)*(0.17_R8)
      !--- compute net short-wave based on LY08 latitudinally-varying albedo ---
      avg_alb = ( 0.069 - 0.011*cos(2.0_R8*yc(n)*degtorad ) )
      Faxa_swnet(n) = strm_swdn(n)*(1.0_R8 - avg_alb)
      !--- corrections to GISS sswdn for heat budget balancing ---
      factor = 1.0_R8
      if      ( -60.0_R8 < yc(n) .and. yc(n) < -50.0_R8 ) then
         factor = 1.0_R8 - (yc(n) + 60.0_R8)*(0.05_R8/10.0_R8)
      else if ( -50.0_R8 < yc(n) .and. yc(n) <  30.0_R8 ) then
         factor = 0.95_R8
      else if (  30.0_R8 < yc(n) .and. yc(n) <  40._R8 ) then
         factor = 1.0_R8 - (40.0_R8 - yc(n))*(0.05_R8/10.0_R8)
      endif
      Faxa_swnet(n) = Faxa_swnet(n)*factor
      Faxa_swvdr(n) = Faxa_swvdr(n)*factor
      Faxa_swndr(n) = Faxa_swndr(n)*factor
      Faxa_swvdf(n) = Faxa_swvdf(n)*factor
      Faxa_swndf(n) = Faxa_swndf(n)*factor
      !--- correction to GISS lwdn in Arctic ---
      if ( yc(n) > 60._R8 ) then
         factor = MIN(1.0_R8, 0.1_R8*(yc(n)-60.0_R8) )
         Faxa_lwdn(n) = Faxa_lwdn(n) + factor * dLWarc
      endif

    enddo   ! lsize

    if (associated(Faxa_ndep)) then
       ! convert ndep flux to units of kgN/m2/s (input is in gN/m2/s)
       Faxa_ndep(:,:) = Faxa_ndep(:,:) / 1000._r8
    end if

  end subroutine datm_datamode_simple_advance

  !===============================================================================
  subroutine datm_datamode_simple_restart_write(case_name, inst_suffix, ymd, tod, &
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

  end subroutine datm_datamode_simple_restart_write

  !===============================================================================
  subroutine datm_datamode_simple_restart_read(rest_filem, inst_suffix, logunit, my_task, mpicom, sdat)

    ! input/output arguments
    character(len=*)            , intent(inout) :: rest_filem
    character(len=*)            , intent(in)    :: inst_suffix
    integer                     , intent(in)    :: logunit
    integer                     , intent(in)    :: my_task
    integer                     , intent(in)    :: mpicom
    type(shr_strdata_type)      , intent(inout) :: sdat
    !-------------------------------------------------------------------------------

    call dshr_restart_read(rest_filem, rpfile, inst_suffix, nullstr, logunit, my_task, mpicom, sdat)

  end subroutine datm_datamode_simple_restart_read

end module datm_datamode_simple_mod
