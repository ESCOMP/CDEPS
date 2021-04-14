module dshr_mod

  use NUOPC            , only : NUOPC_CompAttributeGet, NUOPC_CompFilterPhaseMap
  use NUOPC_Model      , only : NUOPC_ModelGet
  use ESMF             , only : operator(<), operator(/=), operator(+)
  use ESMF             , only : operator(-), operator(*) , operator(>=)
  use ESMF             , only : operator(<=), operator(>), operator(==)
  use ESMF             , only : ESMF_METHOD_INITIALIZE
  use ESMF             , only : ESMF_LOGERR_PASSTHRU, ESMF_LogFoundError, ESMF_LOGMSG_ERROR, ESMF_MAXSTR
  use ESMF             , only : ESMF_SUCCESS, ESMF_LogWrite, ESMF_LOGMSG_INFO, ESMF_FAILURE, ESMF_LogSetError
  use ESMF             , only : ESMF_State, ESMF_StateGet
  use ESMF             , only : ESMF_Field, ESMF_FieldGet
  use ESMF             , only : ESMF_FieldBundle, ESMF_FieldBundleGet, ESMF_FieldBundleIsCreated
  use ESMF             , only : ESMF_DistGrid, ESMF_DistGridGet, ESMF_Array, ESMF_ArrayCreate, ESMF_ArrayDestroy
  use ESMF             , only : ESMF_GridComp, ESMF_GridCompGet, ESMF_GridCompSet
  use ESMF             , only : ESMF_GeomType_Flag, ESMF_FieldStatus_Flag
  use ESMF             , only : ESMF_Mesh, ESMF_MeshGet, ESMF_MeshSet, ESMF_MeshCreate, ESMF_MeshDestroy
  use ESMF             , only : ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER, ESMF_GRIDCREATENOPERIDIMUFRM
  use ESMF             , only : ESMF_FILEFORMAT_ESMFMESH, ESMF_Grid
  use ESMF             , only : ESMF_GEOMTYPE_MESH, ESMF_GEOMTYPE_GRID, ESMF_FIELDSTATUS_COMPLETE
  use ESMF             , only : ESMF_Clock, ESMF_ClockCreate, ESMF_ClockGet, ESMF_ClockSet
  use ESMF             , only : ESMF_ClockPrint, ESMF_ClockAdvance, ESMF_ClockGetAlarmList
  use ESMF             , only : ESMF_Alarm, ESMF_AlarmCreate, ESMF_AlarmGet, ESMF_AlarmSet
  use ESMF             , only : ESMF_ALARMLIST_ALL
  use ESMF             , only : ESMF_Calendar
  use ESMF             , only : ESMF_CALKIND_NOLEAP, ESMF_CALKIND_GREGORIAN, ESMF_CALKIND_FLAG
  use ESMF             , only : ESMF_Time, ESMF_TimeGet, ESMF_TimeSet
  use ESMF             , only : ESMF_TimeInterval, ESMF_TimeIntervalSet, ESMF_TimeIntervalGet
  use ESMF             , only : ESMF_VM, ESMF_VMGet, ESMF_VMBroadcast, ESMF_VMGetCurrent
  use ESMF             , only : ESMF_RouteHandle, ESMF_FieldRegrid
  use ESMF             , only : ESMF_TERMORDER_SRCSEQ, ESMF_FieldRegridStore, ESMF_SparseMatrixWrite
  use ESMF             , only : ESMF_Region_Flag, ESMF_REGION_TOTAL, ESMF_MAXSTR, ESMF_RC_NOT_VALID
  use ESMF             , only : ESMF_UtilStringUpperCase
  use shr_kind_mod     , only : r8=>shr_kind_r8, cs=>shr_kind_cs, cl=>shr_kind_cl, cxx=>shr_kind_cxx, i8=>shr_kind_i8
  use shr_sys_mod      , only : shr_sys_abort
  use shr_mpi_mod      , only : shr_mpi_bcast
  use shr_cal_mod      , only : shr_cal_noleap, shr_cal_gregorian, shr_cal_calendarname
  use shr_cal_mod      , only : shr_cal_datetod2string, shr_cal_date2julian
  use shr_const_mod    , only : shr_const_spval, shr_const_cday
  use shr_orb_mod      , only : shr_orb_params, SHR_ORB_UNDEF_INT, SHR_ORB_UNDEF_REAL
#ifdef CESMCOUPLED
  use shr_pio_mod      , only : shr_pio_getiosys, shr_pio_getiotype, shr_pio_getioformat
#endif
  use dshr_strdata_mod , only : shr_strdata_type, SHR_STRDATA_GET_STREAM_COUNT
  use dshr_methods_mod , only : chkerr
  use pio

  implicit none
  public

  public  :: dshr_model_initphase
  public  :: dshr_init
  public  :: dshr_mesh_init
  public  :: dshr_set_runclock
  public  :: dshr_restart_read
  public  :: dshr_restart_write
  public  :: dshr_log_clock_advance
  public  :: dshr_state_getscalar
  public  :: dshr_state_setscalar
  public  :: dshr_orbital_update
  public  :: dshr_orbital_init

  private :: dshr_mesh_create
  private :: dshr_alarm_init
  private :: dshr_time_init

  ! Note that gridTofieldMap = 2, therefore the ungridded dimension is innermost

  ! orbital settings
  character(len=CL) :: orb_mode        ! attribute - orbital mode (nuopc attribute)
  integer           :: orb_iyear       ! attribute - orbital year (nuopc attribute)
  integer           :: orb_iyear_align ! attribute - associated with model year (nuopc attribute)
  real(R8)          :: orb_obliq       ! attribute - obliquity in degrees (nuopc attribute)
  real(R8)          :: orb_mvelp       ! attribute - moving vernal equinox longitude (nuopc attribute)
  real(R8)          :: orb_eccen       ! attribute and update-  orbital eccentricity (nuopc attribute)
  character(len=*) , parameter :: orb_fixed_year        = 'fixed_year'
  character(len=*) , parameter :: orb_variable_year     = 'variable_year'
  character(len=*) , parameter :: orb_fixed_parameters  = 'fixed_parameters'

  integer     , parameter :: master_task = 0
  character(*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine dshr_model_initphase(gcomp, importState, exportState, clock, rc)

    ! input/output variables
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Switch to IPDv01 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, acceptStringList=(/"IPDv01p"/), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine dshr_model_initphase

  !===============================================================================
  subroutine dshr_init(gcomp, compname, sdat, mpicom, my_task, inst_index, inst_suffix, &
       flds_scalar_name, flds_scalar_num, flds_scalar_index_nx, flds_scalar_index_ny, logunit, rc)

    ! input/output variables
    type(ESMF_GridComp)                   :: gcomp
    character(len=*)      , intent(in)    :: compname  !e.g. ATM, OCN, ...
    type(shr_strdata_type), intent(inout) :: sdat
    integer               , intent(inout) :: mpicom
    integer               , intent(out)   :: my_task
    integer               , intent(out)   :: inst_index
    character(len=*)      , intent(out)   :: inst_suffix
    character(len=*)      , intent(out)   :: flds_scalar_name
    integer               , intent(out)   :: flds_scalar_num
    integer               , intent(out)   :: flds_scalar_index_nx
    integer               , intent(out)   :: flds_scalar_index_ny
    integer               , intent(out)   :: logunit
    integer               , intent(out)   :: rc

    ! local variables
    type(ESMF_VM)     :: vm
    logical           :: isPresent, isSet
    character(len=CL) :: cvalue
    character(len=CL) :: logmsg
    character(len=CL) :: diro
    character(len=CL) :: logfile
    character(len=*),parameter  :: subname='(dshr_advertise)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    ! generate local mpi comm
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMGet(vm, mpiCommunicator=mpicom, localPet=my_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! get scalar attributes
    call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldName", value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       flds_scalar_name = trim(cvalue)
       call ESMF_LogWrite(trim(subname)//' flds_scalar_name = '//trim(flds_scalar_name), ESMF_LOGMSG_INFO)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldCount", value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       read(cvalue, *) flds_scalar_num
       write(logmsg,*) flds_scalar_num
       call ESMF_LogWrite(trim(subname)//' flds_scalar_num = '//trim(logmsg), ESMF_LOGMSG_INFO)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldIdxGridNX", value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       read(cvalue,*) flds_scalar_index_nx
       write(logmsg,*) flds_scalar_index_nx
       call ESMF_LogWrite(trim(subname)//' : flds_scalar_index_nx = '//trim(logmsg), ESMF_LOGMSG_INFO)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldIdxGridNY", value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       read(cvalue,*) flds_scalar_index_ny
       write(logmsg,*) flds_scalar_index_ny
       call ESMF_LogWrite(trim(subname)//' : flds_scalar_index_ny = '//trim(logmsg), ESMF_LOGMSG_INFO)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    ! set output logging
    if (my_task == master_task) then
       call NUOPC_CompAttributeGet(gcomp, name="diro", value=diro, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       call NUOPC_CompAttributeGet(gcomp, name="logfile", value=logfile, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       open(newunit=logunit,file=trim(diro)//"/"//trim(logfile))
    else
       logUnit = 6
    endif

    ! set component instance and suffix
    call NUOPC_CompAttributeGet(gcomp, name="inst_suffix", isPresent=isPresent, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    if (isPresent) then
       call NUOPC_CompAttributeGet(gcomp, name="inst_suffix", value=inst_suffix, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       cvalue = inst_suffix(2:)
       read(cvalue, *) inst_index
    else
       inst_suffix = ""
       inst_index=1
    endif

#ifdef CESMCOUPLED
    sdat%pio_subsystem => shr_pio_getiosys(trim(compname))
    sdat%io_type       =  shr_pio_getiotype(trim(compname))
    sdat%io_format     =  shr_pio_getioformat(trim(compname))
#else
    call dshr_pio_init(gcomp, sdat, logunit, rc)
#endif

  end subroutine dshr_init

  !===============================================================================
  subroutine dshr_mesh_init(gcomp, sdat, nullstr, logunit, compname, model_nxg, model_nyg, &
       model_meshfile, model_maskfile, model_createmesh_fromfile, model_mesh, &
       model_mask, model_frac, read_restart, rc)

    ! ----------------------------------------------
    ! Initialize model mesh
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_GridComp)        , intent(inout) :: gcomp
    type(shr_strdata_type)     , intent(in)    :: sdat
    integer                    , intent(in)    :: logunit
    character(len=*)           , intent(in)    :: compname  !e.g. ATM, OCN, ...
    character(len=*)           , intent(in)    :: nullstr
    integer                    , intent(in)    :: model_nxg
    integer                    , intent(in)    :: model_nyg
    character(len=*)           , intent(in)    :: model_meshfile
    character(len=*)           , intent(in)    :: model_maskfile
    character(len=*)           , intent(in)    :: model_createmesh_fromfile
    type(ESMF_Mesh)            , intent(out)   :: model_mesh
    integer , pointer          , intent(out)   :: model_mask(:)
    real(r8), pointer          , intent(out)   :: model_frac(:)
    logical                    , intent(out)   :: read_restart
    integer                    , intent(out)   :: rc

    ! local variables
    type(ESMF_VM)                  :: vm
    type(ESMF_Calendar)            :: esmf_calendar           ! esmf calendar
    type(ESMF_DistGrid)            :: distGrid
    integer, pointer               :: model_gindex(:)         ! model global index spzce
    integer                        :: mpicom
    integer                        :: my_task
    logical                        :: scol_mode
    real(r8)                       :: scol_lon
    real(r8)                       :: scol_lat
    character(CL)                  :: cvalue
    integer                        :: lsize                   ! local size of mesh
    integer                        :: petcount
    type(ESMF_Array)               :: elemMaskArray
    type(file_desc_t)              :: pioid
    type(var_desc_t)               :: varid
    integer                        :: rcode
    logical                        :: isPresent, isSet
    logical                        :: masterproc
    character(len=*), parameter    :: subname='(dshr_mod:dshr_mesh_init)'
    character(*)    , parameter    :: F00 ="('(dshr_mesh_init) ',a)"
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    ! generate local mpi comm
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMGet(vm, mpiCommunicator=mpicom, localPet=my_task, petcount=petcount, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Set restart flag
    ! TODO: should this be a variable from the driver?
    call NUOPC_CompAttributeGet(gcomp, name='read_restart', value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       read(cvalue,*) read_restart
    else
       call shr_sys_abort(subname//' ERROR: read restart flag must be present')
    end if

    masterproc = (my_task == master_task)

    ! Obtain the data model mesh
    ! (1) if asked to create the mesh
    !     - create mesh from input file given by model_createmesh_fromfile
    !     - if single column find the nearest neighbor in model_createmesh_fromfile
    ! (2) if not single column - obtain the mesh directly from the mesh input 
    !     - reset the model mesh if the model maskfile is not equal to the model mesh file 

    if (trim(model_meshfile) == nullstr) then

       ! Get single column values
       call NUOPC_CompAttributeGet(gcomp, name='single_column', value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       if (isPresent .and. isSet) then
          read(cvalue,*) scol_mode
          if (scol_mode) then
             ! verify that ROF, WAV and LND are not trying to use single column mode
             if (trim(compname) == 'ROF' .or. trim(compname) == 'LND' .or. trim(compname) == 'WAV') then
                if (masterproc) then
                   write(logunit,*) subname,' ERROR: '//trim(compname)//' does not support single column mode '
                end if
                call shr_sys_abort(subname//' ERROR: '//trim(compname)//' does not support single column mode ')
             end if

             ! verify that are only using 1 pe
             if (petcount > 1) then
                if (masterproc) then
                   write(logunit,*) subname,' ERROR: single column mode must be run on one pe, petcount= ',petcount
                end if
                call shr_sys_abort(subname//' ERROR: single column mode must be run on 1 pe')
             endif

             ! obtain the single column lon and lat
             call NUOPC_CompAttributeGet(gcomp, name='scmlon', value=cvalue, rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
             read(cvalue,*) scol_lon
             call NUOPC_CompAttributeGet(gcomp, name='scmlat', value=cvalue, rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
             read(cvalue,*) scol_lat
             if (my_task == master_task) then
                write(logunit,*) ' single column mode, lon lat = ',scol_mode, scol_lon, scol_lat
             end if
          else
             scol_lon  = shr_const_spval
             scol_lat  = shr_const_spval
          end if
       else
          scol_mode = .false.
          scol_lon  = shr_const_spval
          scol_lat  = shr_const_spval
       end if

       ! Now create the model meshfile
       call dshr_mesh_create(trim(model_createmesh_fromfile), scol_mode, scol_lon, scol_lat, &
            trim(compname), sdat, my_task, logunit, model_mesh, model_mask, model_frac, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

    else

       ! Read in the input model mesh
       model_mesh = ESMF_MeshCreate(trim(model_meshfile), fileformat=ESMF_FILEFORMAT_ESMFMESH, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Reset the model mesh mask if the mask file is different from the mesh file
       if (trim(model_meshfile) /= trim(model_maskfile)) then

          ! obtain the model mask by mapping the mesh created by reading in the model_maskfile to the
          ! model mesh and then reset the model mesh mask
          call dshr_set_modelmask(model_mesh, model_maskfile, compname, model_mask, model_frac, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return

          if (masterproc) then
             write(logunit,F00) trim(subname)// " obtained "//trim(compname)//" mesh from "// &
                  trim(model_meshfile)
             write(logunit,F00) trim(subname)// " obtained "//trim(compname)//" mask from "// &
                  trim(model_maskfile)
          end if

       else ! model_meshfile and model_maskfile are the same

          ! obtain the mask and fraction from the mesh file
          call ESMF_MeshGet(model_mesh, numOwnedElements=lsize, elementdistGrid=distGrid, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return

          ! allocate memory for model_mask and model frac
          allocate(model_mask(lsize))
          allocate(model_frac(lsize))

          ! Get the values of model_mask
          elemMaskArray = ESMF_ArrayCreate(distGrid, model_mask, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          call ESMF_MeshGet(model_mesh, elemMaskArray=elemMaskArray, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return

          ! Now set the fraction as just the real mask
          model_frac(:) = real(model_mask(:), kind=r8)
          call ESMF_ArrayDestroy(elemMaskArray, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return

          if (masterproc) then
             write(logunit,F00) trim(subname)// " obtained "//trim(compname)//" mesh and mask from "// &
                  trim(model_meshfile)
          end if
       end if
    end if


  end subroutine dshr_mesh_init

  !===============================================================================
  subroutine dshr_mesh_create(filename, scol_mode, scol_lon, scol_lat, &
       compname, sdat, my_task, logunit, model_mesh, model_mask, model_frac, rc)

    ! Create the model mesh from the domain file - for either single column mode
    ! or for a regional grid

    ! input/output variables
    character(len=*)      , intent(in)    :: filename
    logical               , intent(in)    :: scol_mode
    real(r8)              , intent(inout) :: scol_lon
    real(r8)              , intent(inout) :: scol_lat
    character(len=*)      , intent(in)    :: compname
    type(shr_strdata_type), intent(in)    :: sdat
    integer               , intent(in)    :: my_task
    integer               , intent(in)    :: logunit
    type(ESMF_Mesh)       , intent(out)   :: model_mesh
    integer , pointer     , intent(out)   :: model_mask(:)
    real(r8), pointer     , intent(out)   :: model_frac(:)
    integer               , intent(out)   :: rc

    ! local variables
    type(file_desc_t)              :: pioid
    integer                        :: rcode                   ! error code
    type(var_desc_t)               :: varid                   ! variable id
    integer                        :: dimid                   ! dimension id
    integer                        :: ni, nj, nv              ! dimension sizes
    integer                        :: i,j
    integer                        :: maxIndex(2)
    real(r8)                       :: mincornerCoord(2)
    real(r8)                       :: maxcornerCoord(2)
    type(ESMF_Grid)                :: lgrid
    integer                        :: spatialDim              ! number of dimension in mesh
    integer                        :: numOwnedElements        ! number of elements owned by this PET
    real(r8), pointer              :: ownedElemCoords(:)      ! mesh element coordinates owned by this PET
    integer                        :: n                       ! index
    integer                        :: start(2)                ! start index to read in for single column mode
    integer                        :: count(2)                ! number of points to read in
    real(r8), allocatable          :: xv(:,:,:), yv(:,:,:)    ! coordinates of vertices
    real(r8), allocatable          :: xc(:,:), yc(:,:)        ! coordinates of centers
    real(r8)                       :: scol_data(1)            ! temporary
    integer , allocatable          :: mask(:)                 ! temporary
    real(r8), allocatable          :: lats(:)                 ! temporary
    real(r8), allocatable          :: lons(:)                 ! temporary
    real(r8), allocatable          :: pos_lons(:)             ! temporary
    real(r8)                       :: pos_scol_lon            ! temporary
    real(r8)                       :: scol_area               ! temporary
    character(len=*), parameter    :: subname='(dshr_mesh_create)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    !-------------------------------------
    ! open domain file and get dimensions
    !-------------------------------------

    rcode = pio_openfile(sdat%pio_subsystem, pioid, sdat%io_type, trim(filename), pio_nowrite)
    call pio_check_err(rcode, 'error opening file '//trim(filename))
    call PIO_seterrorhandling(pioid, PIO_BCAST_ERROR)

    rcode = pio_inq_dimid(pioid, 'ni', dimid)
    call pio_check_err(rcode, 'pio_inq_dimid for ni in file '//trim(filename))
    rcode = pio_inquire_dimension(pioid, dimid, len=ni)
    call pio_check_err(rcode, 'pio_inq_dimension for ni in file '//trim(filename))

    rcode = pio_inq_dimid(pioid, 'nj', dimid)
    call pio_check_err(rcode, 'pio_inq_dimid for nj in file '//trim(filename))
    rcode = pio_inquire_dimension(pioid, dimid, len=nj)
    call pio_check_err(rcode, 'pio_inq_dimension for nj in file '//trim(filename))

    rcode = pio_inq_dimid(pioid, 'nv', dimid)
    call pio_check_err(rcode, 'pio_inq_dimid for nv in file '//trim(filename))
    rcode = pio_inquire_dimension(pioid, dimid, len=nv)
    call pio_check_err(rcode, 'pio_inq_dimension for nv in file '//trim(filename))

    !-------------------------------------
    ! Single column model (size 1)
    !-------------------------------------

    if (scol_mode) then

       ! get lons from domain file
       rcode = pio_inq_varid(pioid, 'xc', varid)
       call pio_check_err(rcode, 'pio_inq_varid for xc in file '//trim(filename))
       allocate(xc(ni,nj))
       rcode = pio_get_var(pioid, varid, xc)
       call pio_check_err(rcode, 'pio_get_var for xc in file '//trim(filename))

       ! get lats from domain file
       rcode = pio_inq_varid(pioid, 'yc', varid)
       call pio_check_err(rcode, 'pio_inq_varid for yc in file '//trim(filename))
       allocate(yc(ni,nj))
       rcode = pio_get_var(pioid, varid, yc)
       call pio_check_err(rcode, 'pio_get_var for yc in file '//trim(filename))

       ! find nearest neighbor indices of scol_lon and scol_lat in domain file
       allocate(lats(nj))
       allocate(lons(ni))
       allocate(pos_lons(ni))
       do i = 1,ni
          lons(i) = xc(i,1)
       end do
       do j = 1,nj
          lats(j) = yc(1,j)
       end do
       pos_lons(:)  = mod(lons(:)  + 360._r8, 360._r8)
       pos_scol_lon = mod(scol_lon + 360._r8, 360._r8)
       start(1) = (MINLOC(abs(pos_lons - pos_scol_lon), dim=1))
       start(2) = (MINLOC(abs(lats      -scol_lat    ), dim=1))
       count(:) = 1
       deallocate(lons)
       deallocate(lats)

       ! read in value of nearest neighbor lon and RESET scol_lat
       rcode = pio_inq_varid(pioid, 'xc' , varid)
       call pio_check_err(rcode, 'pio_inq_varid for xc in file '//trim(filename))
       rcode = pio_get_var(pioid, varid, start, count, scol_data)
       call pio_check_err(rcode, 'pio_get_var for xc in file '//trim(filename))
       scol_lon = scol_data(1)

       ! read in value of nearest neighbor lon and RESET scol_lon
       rcode = pio_inq_varid(pioid, 'yc' , varid)
       call pio_check_err(rcode, 'pio_inq_varid for yc in file '//trim(filename))
       rcode = pio_get_var(pioid, varid, start, count, scol_data)
       call pio_check_err(rcode, 'pio_get_var for yc in file '//trim(filename))
       scol_lat = scol_data(1)

       ! get area of gridcell
       rcode = pio_inq_varid(pioid, 'area', varid)
       call pio_check_err(rcode, 'pio_inq_varid for area in file '//trim(filename))
       rcode = pio_get_var(pioid, varid, start, count, scol_data)
       call pio_check_err(rcode, 'pio_get_var for area in file '//trim(filename))
       scol_area = scol_data(1)

       ! create the single column grid
       maxIndex(1)       = 1                     ! number of lons
       maxIndex(2)       = 1                     ! number of lats
       mincornerCoord(1) = scol_lon - scol_area/2._r8 ! min lon
       mincornerCoord(2) = scol_lat - scol_area/2._r8 ! min lat
       maxcornerCoord(1) = scol_lon + scol_area/2._r8 ! max lon
       maxcornerCoord(2) = scol_lat + scol_area/2._r8 ! max lat

       ! create the model grid
       lgrid = ESMF_GridCreateNoPeriDimUfrm (maxindex=maxindex, &
            mincornercoord=mincornercoord, maxcornercoord= maxcornercoord, &
            staggerloclist=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! create the mesh from the grid
       model_mesh = ESMF_MeshCreate(lgrid, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! For the ATM component - the fractions and masks are always 1 - don't read them
       ! in or reset the mesh

       if (compname /= 'ATM') then
          ! get model_mask
          allocate(model_mask(1))
          rcode = pio_inq_varid(pioid, 'mask', varid)
          call pio_check_err(rcode, 'pio_inq_varid for area in file '//trim(filename))
          rcode = pio_get_var(pioid, varid, start, count, model_mask)
          call pio_check_err(rcode, 'pio_get_var for area in file '//trim(filename))

          ! get model_frac
          allocate(model_frac(1))
          rcode = pio_inq_varid(pioid, 'frac', varid)
          call pio_check_err(rcode, 'pio_inq_varid for area in file '//trim(filename))
          rcode = pio_get_var(pioid, varid, start, count, model_frac)
          call pio_check_err(rcode, 'pio_get_var for area in file '//trim(filename))

          ! set the model mesh mask
          call ESMF_MeshSet(model_mesh, elementMask=model_mask, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       end if

    end if

    !-------------------------------------
    ! Non single-column mode
    !-------------------------------------

    if (.not. scol_mode) then

       ! allocate xv and read it in
       allocate(xv(nv,ni,nj))
       rcode = pio_inq_varid(pioid, 'xv', varid)
       call pio_check_err(rcode, 'pio_inq_varid for xv in file '//trim(filename))
       rcode = pio_get_var(pioid, varid, xv)
       call pio_check_err(rcode, 'pio_get_var for xv in file '//trim(filename))

       ! allocate yv and read it in
       allocate(yv(nv,ni,nj))
       rcode = pio_inq_varid(pioid, 'yv', varid)
       call pio_check_err(rcode, 'pio_inq_varid for yv in file '//trim(filename))
       rcode = pio_get_var(pioid, varid, yv)
       call pio_check_err(rcode, 'pio_get_var for yv in file '//trim(filename))

       ! create grid from corner vertices
       maxIndex(1)       = ni          ! number of lons
       maxIndex(2)       = nj          ! number of lats
       mincornerCoord(1) = xv(1,1,1)   ! min lon
       mincornerCoord(2) = yv(1,1,1)   ! min lat
       maxcornerCoord(1) = xv(3,ni,nj) ! max lon
       maxcornerCoord(2) = yv(3,ni,nj) ! max lat

       ! create the model grid
       lgrid = ESMF_GridCreateNoPeriDimUfrm (maxindex=maxindex, &
            mincornercoord=mincornercoord, maxcornercoord= maxcornercoord, &
            staggerloclist=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! create the mesh from the grid
       model_mesh = ESMF_MeshCreate(lgrid, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! For the ATM component - the fractions and masks are always 1 - don't read them
       ! in or reset the mesh
       if (compname /= 'ATM') then
          ! allocate model_mask and read it in
          allocate(model_mask(ni*nj))
          rcode = pio_inq_varid(pioid, 'mask', varid)
          call pio_check_err(rcode, 'pio_inq_varid for mask in file '//trim(filename))
          rcode = pio_get_var(pioid, varid, model_mask)
          call pio_check_err(rcode, 'pio_get_var for mask in file '//trim(filename))

          ! allocate model_frac and read it in
          allocate(model_frac(ni*nj))
          rcode = pio_inq_varid(pioid, 'frac', varid)
          call pio_check_err(rcode, 'pio_inq_varid for frac in file '//trim(filename))
          rcode = pio_get_var(pioid, varid, model_frac)
          call pio_check_err(rcode, 'pio_get_var for frac in file '//trim(filename))

          ! reset the model mesh mask if not an atmosphere component
          call ESMF_MeshSet(model_mesh, elementMask=mask, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       end if

    end if

    !-------------------------------------
    ! close domain file and write diagnostic output
    !-------------------------------------

    call pio_seterrorhandling(pioid, PIO_INTERNAL_ERROR)
    call pio_closefile(pioid)

    call ESMF_MeshGet(model_mesh, numOwnedElements=numOwnedElements, spatialDim=spatialDim, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    allocate(ownedElemCoords(spatialDim*numOwnedElements)) ! this is a pointer and must be deallocated
    call ESMF_MeshGet(model_mesh, ownedElemCoords=ownedElemCoords)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    allocate(lons(numOwnedElements))
    allocate(lats(numOwnedElements))
    do n = 1, numOwnedElements
       lons(n) = ownedElemCoords(2*n-1)
       lats(n) = ownedElemCoords(2*n)
    end do
    if (my_task == master_task) then
       write(logunit,*)' Mesh created from file ',trim(filename)
       write(logunit,*)' mesh element lons = ',lons(:)
       write(logunit,*)' mesh element lats = ',lats(:)
    end if
    deallocate(ownedElemCoords)

  contains

    subroutine pio_check_err(ierror, description)
      integer     , intent(in) :: ierror
      character(*), intent(in) :: description
      if (ierror /= PIO_NOERR) then
         write (*,'(6a)') 'ERROR ', trim(description)
         call shr_sys_abort()
      endif
    end subroutine pio_check_err

  end subroutine dshr_mesh_create

  !===============================================================================
  subroutine dshr_set_runclock(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)         :: mclock, dclock
    type(ESMF_Time)          :: mcurrtime, dcurrtime
    type(ESMF_Time)          :: mstoptime
    type(ESMF_TimeInterval)  :: mtimestep, dtimestep
    character(len=256)       :: cvalue
    character(len=256)       :: restart_option       ! Restart option units
    integer                  :: restart_n            ! Number until restart interval
    integer                  :: restart_ymd          ! Restart date (YYYYMMDD)
    type(ESMF_ALARM)         :: restart_alarm
    character(len=128)       :: name
    integer                  :: alarmcount
    character(len=*),parameter :: subname='dshr_mod:(ModelSetRunClock) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! query the Component for its clocks
    call NUOPC_ModelGet(gcomp, driverClock=dclock, modelClock=mclock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(dclock, currTime=dcurrtime, timeStep=dtimestep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(mclock, currTime=mcurrtime, timeStep=mtimestep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! force model clock currtime and timestep to match driver and set stoptime
    mstoptime = mcurrtime + dtimestep
    call ESMF_ClockSet(mclock, currTime=dcurrtime, timeStep=dtimestep, stopTime=mstoptime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! set restart alarm
    call ESMF_ClockGetAlarmList(mclock, alarmlistflag=ESMF_ALARMLIST_ALL, alarmCount=alarmCount, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (alarmCount == 0) then

       call ESMF_GridCompGet(gcomp, name=name, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite(subname//'setting alarms for' // trim(name), ESMF_LOGMSG_INFO)

       call NUOPC_CompAttributeGet(gcomp, name="restart_option", value=restart_option, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       call NUOPC_CompAttributeGet(gcomp, name="restart_n", value=cvalue, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       read(cvalue,*) restart_n

       call NUOPC_CompAttributeGet(gcomp, name="restart_ymd", value=cvalue, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       read(cvalue,*) restart_ymd

       call dshr_alarm_init(mclock, restart_alarm, restart_option, &
            opt_n   = restart_n,           &
            opt_ymd = restart_ymd,         &
            RefTime = mcurrTime,           &
            alarmname = 'alarm_restart', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       call ESMF_AlarmSet(restart_alarm, clock=mclock, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

    end if

    ! Advance model clock to trigger alarms then reset model clock back to currtime
    call ESMF_ClockAdvance(mclock,rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockSet(mclock, currTime=dcurrtime, timeStep=dtimestep, stopTime=mstoptime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine dshr_set_runclock

  !===============================================================================
  subroutine dshr_alarm_init( clock, alarm, option, &
       opt_n, opt_ymd, opt_tod, RefTime, alarmname, rc)

    ! Setup an alarm in a clock
    ! Notes: The ringtime sent to AlarmCreate MUST be the next alarm
    ! time.  If you send an arbitrary but proper ringtime from the
    ! past and the ring interval, the alarm will always go off on the
    ! next clock advance and this will cause serious problems.  Even
    ! if it makes sense to initialize an alarm with some reference
    ! time and the alarm interval, that reference time has to be
    ! advance forward to be >= the current time.  In the logic below
    ! we set an appropriate "NextAlarm" and then we make sure to
    ! advance it properly based on the ring interval.

    ! input/output variables
    type(ESMF_Clock)            , intent(inout) :: clock     ! clock
    type(ESMF_Alarm)            , intent(inout) :: alarm     ! alarm
    character(len=*)            , intent(in)    :: option    ! alarm option
    integer          , optional , intent(in)    :: opt_n     ! alarm freq
    integer          , optional , intent(in)    :: opt_ymd   ! alarm ymd
    integer          , optional , intent(in)    :: opt_tod   ! alarm tod (sec)
    type(ESMF_Time)  , optional , intent(in)    :: RefTime   ! ref time
    character(len=*) , optional , intent(in)    :: alarmname ! alarm name
    integer                     , intent(inout) :: rc        ! Return code

    ! local variables
    type(ESMF_Calendar)     :: cal                ! calendar
    integer                 :: lymd             ! local ymd
    integer                 :: ltod             ! local tod
    integer                 :: cyy,cmm,cdd,csec ! time info
    character(len=64)       :: lalarmname       ! local alarm name
    logical                 :: update_nextalarm ! update next alarm
    type(ESMF_Time)         :: CurrTime         ! Current Time
    type(ESMF_Time)         :: NextAlarm        ! Next restart alarm time
    type(ESMF_TimeInterval) :: AlarmInterval    ! Alarm interval
    character(len=*), parameter :: &   ! Clock and alarm options
         optNONE           = "none"      , &
         optNever          = "never"     , &
         optNSteps         = "nsteps"    , &
         optNStep          = "nstep"     , &
         optNSeconds       = "nseconds"  , &
         optNSecond        = "nsecond"   , &
         optNMinutes       = "nminutes"  , &
         optNMinute        = "nminute"   , &
         optNHours         = "nhours"    , &
         optNHour          = "nhour"     , &
         optNDays          = "ndays"     , &
         optNDay           = "nday"      , &
         optNMonths        = "nmonths"   , &
         optNMonth         = "nmonth"    , &
         optNYears         = "nyears"    , &
         optNYear          = "nyear"     , &
         optMonthly        = "monthly"   , &
         optYearly         = "yearly"    , &
         optDate           = "date"      , &
         optIfdays0        = "ifdays0"
    character(len=*), parameter :: subname = '(dshr_alarm_init): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    lalarmname = 'alarm_unknown'
    if (present(alarmname)) lalarmname = trim(alarmname)
    ltod = 0
    if (present(opt_tod)) ltod = opt_tod
    lymd = -1
    if (present(opt_ymd)) lymd = opt_ymd

    call ESMF_ClockGet(clock, CurrTime=CurrTime, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeGet(CurrTime, yy=cyy, mm=cmm, dd=cdd, s=csec, rc=rc )
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! initial guess of next alarm, this will be updated below
    if (present(RefTime)) then
       NextAlarm = RefTime
    else
       NextAlarm = CurrTime
    endif

    ! Determine calendar
    call ESMF_ClockGet(clock, calendar=cal)

    ! Determine inputs for call to create alarm
    selectcase (trim(option))

    case (optNONE)
       call ESMF_TimeIntervalSet(AlarmInterval, yy=9999, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       call ESMF_TimeSet( NextAlarm, yy=9999, mm=12, dd=1, s=0, calendar=cal, rc=rc )
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       update_nextalarm  = .false.

    case (optNever)
       call ESMF_TimeIntervalSet(AlarmInterval, yy=9999, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       call ESMF_TimeSet( NextAlarm, yy=9999, mm=12, dd=1, s=0, calendar=cal, rc=rc )
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       update_nextalarm  = .false.

    case (optDate)
       if (.not. present(opt_ymd)) then
          call shr_sys_abort(subname//trim(option)//' requires opt_ymd')
       end if
       if (lymd < 0 .or. ltod < 0) then
          call shr_sys_abort(subname//trim(option)//'opt_ymd, opt_tod invalid')
       end if
       call ESMF_TimeIntervalSet(AlarmInterval, yy=9999, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       call dshr_time_init(NextAlarm, lymd, cal, ltod, rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       update_nextalarm  = .false.

    case (optIfdays0)
       if (.not. present(opt_ymd)) then
          call shr_sys_abort(subname//trim(option)//' requires opt_ymd')
       end if
       if (.not.present(opt_n)) then
          call shr_sys_abort(subname//trim(option)//' requires opt_n')
       end if
       if (opt_n <= 0)  then
          call shr_sys_abort(subname//trim(option)//' invalid opt_n')
       end if
       call ESMF_TimeIntervalSet(AlarmInterval, mm=1, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       call ESMF_TimeSet( NextAlarm, yy=cyy, mm=cmm, dd=opt_n, s=0, calendar=cal, rc=rc )
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       update_nextalarm  = .true.

   case (optNSteps)
       if (.not.present(opt_n)) then
          call shr_sys_abort(subname//trim(option)//' requires opt_n')
       end if
       if (opt_n <= 0) then
          call shr_sys_abort(subname//trim(option)//' invalid opt_n')
       end if
       call ESMF_ClockGet(clock, TimeStep=AlarmInterval, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       AlarmInterval = AlarmInterval * opt_n
       update_nextalarm  = .true.

    case (optNStep)
       if (.not.present(opt_n)) call shr_sys_abort(subname//trim(option)//' requires opt_n')
       if (opt_n <= 0)  call shr_sys_abort(subname//trim(option)//' invalid opt_n')
       call ESMF_ClockGet(clock, TimeStep=AlarmInterval, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       AlarmInterval = AlarmInterval * opt_n
       update_nextalarm  = .true.

    case (optNSeconds)
       if (.not.present(opt_n)) then
          call shr_sys_abort(subname//trim(option)//' requires opt_n')
       end if
       if (opt_n <= 0) then
          call shr_sys_abort(subname//trim(option)//' invalid opt_n')
       end if
       call ESMF_TimeIntervalSet(AlarmInterval, s=1, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       AlarmInterval = AlarmInterval * opt_n
       update_nextalarm  = .true.

    case (optNSecond)
       if (.not.present(opt_n)) then
          call shr_sys_abort(subname//trim(option)//' requires opt_n')
       end if
       if (opt_n <= 0) then
          call shr_sys_abort(subname//trim(option)//' invalid opt_n')
       end if
       call ESMF_TimeIntervalSet(AlarmInterval, s=1, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       AlarmInterval = AlarmInterval * opt_n
       update_nextalarm  = .true.

    case (optNMinutes)
       call ESMF_TimeIntervalSet(AlarmInterval, s=60, rc=rc)
       if (.not.present(opt_n)) then
          call shr_sys_abort(subname//trim(option)//' requires opt_n')
       end if
       if (opt_n <= 0) then
          call shr_sys_abort(subname//trim(option)//' invalid opt_n')
       end if
       AlarmInterval = AlarmInterval * opt_n
       update_nextalarm  = .true.

    case (optNMinute)
       if (.not.present(opt_n)) then
          call shr_sys_abort(subname//trim(option)//' requires opt_n')
       end if
       if (opt_n <= 0) then
          call shr_sys_abort(subname//trim(option)//' invalid opt_n')
       end if
       call ESMF_TimeIntervalSet(AlarmInterval, s=60, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       AlarmInterval = AlarmInterval * opt_n
       update_nextalarm  = .true.

    case (optNHours)
       if (.not.present(opt_n)) then
          call shr_sys_abort(subname//trim(option)//' requires opt_n')
       end if
       if (opt_n <= 0) then
          call shr_sys_abort(subname//trim(option)//' invalid opt_n')
       end if
       call ESMF_TimeIntervalSet(AlarmInterval, s=3600, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       AlarmInterval = AlarmInterval * opt_n
       update_nextalarm  = .true.

    case (optNHour)
       if (.not.present(opt_n)) then
          call shr_sys_abort(subname//trim(option)//' requires opt_n')
       end if
       if (opt_n <= 0) then
          call shr_sys_abort(subname//trim(option)//' invalid opt_n')
       end if
       call ESMF_TimeIntervalSet(AlarmInterval, s=3600, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       AlarmInterval = AlarmInterval * opt_n
       update_nextalarm  = .true.

    case (optNDays)
       if (.not.present(opt_n)) then
          call shr_sys_abort(subname//trim(option)//' requires opt_n')
       end if
       if (opt_n <= 0) then
          call shr_sys_abort(subname//trim(option)//' invalid opt_n')
       end if
       call ESMF_TimeIntervalSet(AlarmInterval, d=1, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       AlarmInterval = AlarmInterval * opt_n
       update_nextalarm  = .true.

    case (optNDay)
       if (.not.present(opt_n)) then
          call shr_sys_abort(subname//trim(option)//' requires opt_n')
       end if
       if (opt_n <= 0) then
          call shr_sys_abort(subname//trim(option)//' invalid opt_n')
       end if
       call ESMF_TimeIntervalSet(AlarmInterval, d=1, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       AlarmInterval = AlarmInterval * opt_n
       update_nextalarm  = .true.

    case (optNMonths)
       if (.not.present(opt_n)) then
          call shr_sys_abort(subname//trim(option)//' requires opt_n')
       end if
       if (opt_n <= 0) then
          call shr_sys_abort(subname//trim(option)//' invalid opt_n')
       end if
       call ESMF_TimeIntervalSet(AlarmInterval, mm=1, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       AlarmInterval = AlarmInterval * opt_n
       update_nextalarm  = .true.

    case (optNMonth)
       if (.not.present(opt_n)) then
          call shr_sys_abort(subname//trim(option)//' requires opt_n')
       end if
       if (opt_n <= 0) then
          call shr_sys_abort(subname//trim(option)//' invalid opt_n')
       end if
       call ESMF_TimeIntervalSet(AlarmInterval, mm=1, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       AlarmInterval = AlarmInterval * opt_n
       update_nextalarm  = .true.

    case (optMonthly)
       call ESMF_TimeIntervalSet(AlarmInterval, mm=1, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       call ESMF_TimeSet( NextAlarm, yy=cyy, mm=cmm, dd=1, s=0, calendar=cal, rc=rc )
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       update_nextalarm  = .true.

    case (optNYears)
       if (.not.present(opt_n)) then
          call shr_sys_abort(subname//trim(option)//' requires opt_n')
       end if
       if (opt_n <= 0) then
          call shr_sys_abort(subname//trim(option)//' invalid opt_n')
       end if
       call ESMF_TimeIntervalSet(AlarmInterval, yy=1, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       AlarmInterval = AlarmInterval * opt_n
       update_nextalarm  = .true.

    case (optNYear)
       if (.not.present(opt_n)) then
          call shr_sys_abort(subname//trim(option)//' requires opt_n')
       end if
       if (opt_n <= 0) then
          call shr_sys_abort(subname//trim(option)//' invalid opt_n')
       end if
       call ESMF_TimeIntervalSet(AlarmInterval, yy=1, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       AlarmInterval = AlarmInterval * opt_n
       update_nextalarm  = .true.

    case (optYearly)
       call ESMF_TimeIntervalSet(AlarmInterval, yy=1, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       call ESMF_TimeSet( NextAlarm, yy=cyy, mm=1, dd=1, s=0, calendar=cal, rc=rc )
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       update_nextalarm  = .true.

    case default
       call shr_sys_abort(subname//'unknown option '//trim(option))

    end select

    ! --------------------------------------------------------------------------------
    ! --- AlarmInterval and NextAlarm should be set ---
    ! --------------------------------------------------------------------------------

    ! --- advance Next Alarm so it won't ring on first timestep for
    ! --- most options above. go back one alarminterval just to be careful

    if (update_nextalarm) then
       NextAlarm = NextAlarm - AlarmInterval
       do while (NextAlarm <= CurrTime)
          NextAlarm = NextAlarm + AlarmInterval
       enddo
    endif

    alarm = ESMF_AlarmCreate( name=lalarmname, clock=clock, ringTime=NextAlarm, &
         ringInterval=AlarmInterval, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

  end subroutine dshr_alarm_init

  !===============================================================================
  subroutine dshr_time_init( Time, ymd, cal, tod, rc)

    ! Create the ESMF_Time object corresponding to the given input time,
    ! given in YMD (Year Month Day) and TOD (Time-of-day) format.
    ! Set the time by an integer as YYYYMMDD and integer seconds in the day

    ! input/output parameters:
    type(ESMF_Time)     , intent(inout) :: Time ! ESMF time
    integer             , intent(in)    :: ymd  ! year, month, day YYYYMMDD
    type(ESMF_Calendar) , intent(in)    :: cal  ! ESMF calendar
    integer             , intent(in)    :: tod  ! time of day in seconds
    integer             , intent(out)   :: rc

    ! local variables
    integer :: year, mon, day ! year, month, day as integers
    integer :: tdate
    integer         , parameter :: SecPerDay = 86400 ! Seconds per day
    character(len=*), parameter :: subname='(dshr_time_init)'
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    if ( (ymd < 0) .or. (tod < 0) .or. (tod > SecPerDay) )then
       call shr_sys_abort( subname//'ERROR yymmdd is a negative number or time-of-day out of bounds' )
    end if

    tdate = abs(ymd)
    year = int(tdate/10000)
    if (ymd < 0) year = -year
    mon = int( mod(tdate,10000)/  100)
    day = mod(tdate,  100)

    call ESMF_TimeSet( Time, yy=year, mm=mon, dd=day, s=tod, calendar=cal, rc=rc )
    if (chkerr(rc,__LINE__,u_FILE_u)) return

  end subroutine dshr_time_init

  !===============================================================================
  subroutine dshr_restart_read(rest_filem, rpfile, inst_suffix, nullstr, &
       logunit, my_task, mpicom, sdat, fld, fldname)

    ! Read restart file

    use dshr_stream_mod, only : shr_stream_restIO

    ! input/output arguments
    character(len=*)            , intent(inout) :: rest_filem
    character(len=*)            , intent(in)    :: rpfile
    character(len=*)            , intent(in)    :: inst_suffix
    character(len=*)            , intent(in)    :: nullstr
    integer                     , intent(in)    :: logunit
    integer                     , intent(in)    :: my_task
    integer                     , intent(in)    :: mpicom
    type(shr_strdata_type)      , intent(inout) :: sdat
    real(r8)         , optional , pointer       :: fld(:)
    character(len=*) , optional , intent(in)    :: fldname

    ! local variables
    integer           :: nu
    logical           :: exists  ! file existance
    type(file_desc_t) :: pioid
    type(var_desc_t)  :: varid
    type(io_desc_t)   :: pio_iodesc
    integer           :: rcode
    character(*), parameter :: F00   = "('(dshr_restart_read) ',8a)"
    character(*), parameter :: subName = "(dshr_restart_read) "
    !-------------------------------------------------------------------------------

    ! no streams means no restart file is read.
    if(shr_strdata_get_stream_count(sdat) <= 0) return

    if (trim(rest_filem) == trim(nullstr)) then
       if (my_task == master_task) then
          write(logunit,F00) ' restart filename from rpointer'
          inquire(file=trim(rpfile)//trim(inst_suffix), exist=exists)
          if (.not.exists) then
             write(logunit, F00) ' ERROR: rpointer file does not exist'
             call shr_sys_abort(trim(subname)//' ERROR: rpointer file missing')
          endif
          open(newunit=nu, file=trim(rpfile)//trim(inst_suffix), form='formatted')
          read(nu, '(a)') rest_filem
          close(nu)
          inquire(file=trim(rest_filem), exist=exists)
       endif
       call shr_mpi_bcast(rest_filem, mpicom, 'rest_filem')
    else
       ! use namelist already read
       if (my_task == master_task) then
          write(logunit, F00) ' restart filenames from namelist '
          inquire(file=trim(rest_filem), exist=exists)
       endif
    endif
    call shr_mpi_bcast(exists, mpicom, 'exists')
    if (exists) then
       if (my_task == master_task) write(logunit, F00) ' reading data model restart ', trim(rest_filem)
       rcode = pio_openfile(sdat%pio_subsystem, pioid, sdat%io_type, trim(rest_filem), pio_nowrite)
       call shr_stream_restIO(pioid, sdat%stream, 'read')
       if (present(fld) .and. present(fldname)) then
          call pio_initdecomp(sdat%pio_subsystem, pio_double, (/sdat%model_gsize/), sdat%model_gindex, pio_iodesc)
          rcode = pio_inq_varid(pioid, trim(fldname), varid)
          call pio_read_darray(pioid, varid, pio_iodesc, fld, rcode)
       end if
       call pio_closefile(pioid)
       if (present(fld) .and. present(fldname)) then
          call pio_freedecomp(sdat%pio_subsystem, pio_iodesc)
       endif
    else
       if (my_task == master_task) write(logunit, F00) ' file not found, skipping ',trim(rest_filem)
    endif
  end subroutine dshr_restart_read

  !===============================================================================
  subroutine dshr_restart_write(rpfile, case_name, model_name, inst_suffix, ymd, tod, &
       logunit, my_task, sdat, fld, fldname)

    ! Write restart file

    use dshr_stream_mod, only : shr_stream_restIO

    ! input/output variables
    character(len=*)            , intent(in)    :: rpfile
    character(len=*)            , intent(in)    :: case_name
    character(len=*)            , intent(in)    :: model_name
    character(len=*)            , intent(in)    :: inst_suffix
    integer                     , intent(in)    :: ymd       ! model date
    integer                     , intent(in)    :: tod       ! model sec into model date
    integer                     , intent(in)    :: logunit
    integer                     , intent(in)    :: my_task
    type(shr_strdata_type)      , intent(inout) :: sdat
    real(r8)         , optional , pointer       :: fld(:)
    character(len=*) , optional , intent(in)    :: fldname

    ! local variables
    integer           :: nu
    character(len=CL) :: rest_file_model
    character(len=CS) :: date_str
    type(file_desc_t) :: pioid
    integer           :: dimid(1)
    type(var_desc_t)  :: varid
    type(io_desc_t)   :: pio_iodesc
    integer           :: rcode
    character(*), parameter :: F00   = "('(dshr_restart_write) ',2a,2(i0,2x))"
    !-------------------------------------------------------------------------------

    ! no streams means no restart file is written.
    if (shr_strdata_get_stream_count(sdat) <= 0) return

    call shr_cal_datetod2string(date_str, ymd, tod)
    write(rest_file_model ,"(7a)") trim(case_name),'.', trim(model_name),trim(inst_suffix),'.r.', trim(date_str),'.nc'

    ! write restart info to rpointer file
    if (my_task == master_task) then
       open(newunit=nu, file=trim(rpfile)//trim(inst_suffix), form='formatted')
       write(nu,'(a)') rest_file_model
       close(nu)
       write(logunit,F00)' writing ',trim(rest_file_model), ymd, tod
    endif

    ! write data model restart data
    rcode = pio_createfile(sdat%pio_subsystem, pioid, sdat%io_type, trim(rest_file_model), pio_clobber)
    rcode = pio_put_att(pioid, pio_global, "version", "nuopc_data_models_v0")
    if (present(fld) .and. present(fldname)) then
       rcode = pio_def_dim(pioid, 'gsize', sdat%model_gsize, dimid(1))
       rcode = pio_def_var(pioid, trim(fldname), PIO_DOUBLE, dimid, varid)
    endif
    call shr_stream_restIO(pioid, sdat%stream, 'define')
    rcode = pio_enddef(pioid)
    call shr_stream_restIO(pioid, sdat%stream, 'write')
    if (present(fld) .and. present(fldname)) then
       call pio_initdecomp(sdat%pio_subsystem, pio_double, (/sdat%model_gsize/), sdat%model_gindex, pio_iodesc)
       call pio_write_darray(pioid, varid, pio_iodesc, fld, rcode, fillval=shr_const_spval)
    endif
    call pio_closefile(pioid)
    if (present(fld) .and. present(fldname)) then
       call pio_freedecomp(sdat%pio_subsystem, pio_iodesc)
    endif

  end subroutine dshr_restart_write

  !===============================================================================
  subroutine dshr_log_clock_advance(clock, component, logunit, rc)

    ! input/output variables
    type(ESMF_Clock)               :: clock
    character(len=*) , intent(in)  :: component
    integer          , intent(in)  :: logunit
    integer          , intent(out) :: rc

    ! local variables
    character(len=CL) :: cvalue, prestring
    !-----------------------------------------------------------------------

    rc = ESMF_SUCCESS

    write(prestring, *) "------>Advancing ",trim(component)," from: "
    call ESMF_ClockPrint(clock, options="currTime", unit=cvalue, preString=trim(prestring), rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    write(logunit, *) trim(cvalue)

    call ESMF_ClockPrint(clock, options="stopTime", unit=cvalue, &
         preString="--------------------------------> to: ", rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    write(logunit, *) trim(cvalue)

  end subroutine dshr_log_clock_advance

  !===============================================================================
  subroutine dshr_state_getscalar(state, scalar_id, scalar_value, flds_scalar_name, flds_scalar_num, rc)

    ! ----------------------------------------------
    ! Get scalar data from State for a particular name and broadcast it to all other pets
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_State), intent(in)     :: state
    integer,          intent(in)     :: scalar_id
    real(r8),         intent(out)    :: scalar_value
    character(len=*), intent(in)     :: flds_scalar_name
    integer,          intent(in)     :: flds_scalar_num
    integer,          intent(inout)  :: rc

    ! local variables
    integer           :: mytask
    type(ESMF_VM)     :: vm
    type(ESMF_Field)  :: field
    real(r8), pointer :: farrayptr(:,:)
    real(r8)          :: tmp(1)
    character(len=*), parameter :: subname='(state_getscalar)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMGet(vm, localPet=mytask, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call ESMF_StateGet(State, itemName=trim(flds_scalar_name), field=field, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    if (mytask == master_task) then
      call ESMF_FieldGet(field, farrayPtr = farrayptr, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      if (scalar_id < 0 .or. scalar_id > flds_scalar_num) then
        call ESMF_LogWrite(trim(subname)//": ERROR in scalar_id", ESMF_LOGMSG_INFO, line=__LINE__, file=u_FILE_u)
        rc = ESMF_FAILURE
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
      endif
      tmp(:) = farrayptr(scalar_id,:)
    endif
    call ESMF_VMBroadCast(vm, tmp, 1, 0, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    scalar_value = tmp(1)

  end subroutine dshr_state_getscalar

  !================================================================================
  subroutine dshr_state_setscalar(scalar_value, scalar_id, State, flds_scalar_name, flds_scalar_num,  rc)

    ! ----------------------------------------------
    ! Set scalar data from State for a particular name
    ! ----------------------------------------------

    ! input/output arguments
    real(r8),         intent(in)     :: scalar_value
    integer,          intent(in)     :: scalar_id
    type(ESMF_State), intent(inout)  :: State
    character(len=*), intent(in)     :: flds_scalar_name
    integer,          intent(in)     :: flds_scalar_num
    integer,          intent(inout)  :: rc

    ! local variables
    integer           :: mytask
    type(ESMF_Field)  :: lfield
    type(ESMF_VM)     :: vm
    real(r8), pointer :: farrayptr(:,:)
    character(len=*), parameter :: subname='(state_setscalar)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMGet(vm, localPet=mytask, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call ESMF_StateGet(State, itemName=trim(flds_scalar_name), field=lfield, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    if (mytask == master_task) then
       call ESMF_FieldGet(lfield, farrayPtr = farrayptr, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       if (scalar_id < 0 .or. scalar_id > flds_scalar_num) then
          call ESMF_LogWrite(trim(subname)//": ERROR in scalar_id", ESMF_LOGMSG_INFO)
          rc = ESMF_FAILURE
          return
       endif
       farrayptr(scalar_id,1) = scalar_value
    endif

  end subroutine dshr_state_setscalar

  !===============================================================================
  subroutine dshr_orbital_init(gcomp, logunit, mastertask, rc)

    !----------------------------------------------------------
    ! Initialize orbital related values
    !----------------------------------------------------------

    ! input/output variables
    type(ESMF_GridComp)                 :: gcomp
    integer             , intent(in)    :: logunit
    logical             , intent(in)    :: mastertask
    integer             , intent(out)   :: rc              ! output error

    ! local variables
    character(len=CL) :: msgstr          ! temporary
    character(len=CL) :: cvalue          ! temporary
    character(len=*) , parameter :: subname = "(dshr_orbital_init)"
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Determine orbital attributes from input
    call NUOPC_CompAttributeGet(gcomp, name="orb_mode", value=cvalue, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) orb_mode
    call NUOPC_CompAttributeGet(gcomp, name="orb_iyear", value=cvalue, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) orb_iyear
    call NUOPC_CompAttributeGet(gcomp, name="orb_iyear_align", value=cvalue, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) orb_iyear_align
    call NUOPC_CompAttributeGet(gcomp, name="orb_obliq", value=cvalue, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) orb_obliq
    call NUOPC_CompAttributeGet(gcomp, name="orb_eccen", value=cvalue, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) orb_eccen
    call NUOPC_CompAttributeGet(gcomp, name="orb_mvelp", value=cvalue, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) orb_mvelp

    ! Error checks
    if (trim(orb_mode) == trim(orb_fixed_year)) then
       if (orb_iyear == SHR_ORB_UNDEF_INT) then
          if (mastertask) then
             write(logunit,*) trim(subname),' ERROR: invalid settings orb_mode =',trim(orb_mode)
             write(logunit,*) trim(subname),' ERROR: fixed_year settings = ',orb_iyear
             write (msgstr, *) ' ERROR: invalid settings for orb_mode '//trim(orb_mode)
          end if
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msgstr, line=__LINE__, file=__FILE__, rcToReturn=rc)
          return  ! bail out
       else
          orb_obliq = SHR_ORB_UNDEF_REAL
          orb_eccen = SHR_ORB_UNDEF_REAL
          orb_mvelp = SHR_ORB_UNDEF_REAL
       endif
    elseif (trim(orb_mode) == trim(orb_variable_year)) then
       if (orb_iyear == SHR_ORB_UNDEF_INT .or. orb_iyear_align == SHR_ORB_UNDEF_INT) then
          if (mastertask) then
             write(logunit,*) trim(subname),' ERROR: invalid settings orb_mode =',trim(orb_mode)
             write(logunit,*) trim(subname),' ERROR: variable_year settings = ',orb_iyear, orb_iyear_align
             write (msgstr, *) subname//' ERROR: invalid settings for orb_mode '//trim(orb_mode)
          end if
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msgstr, line=__LINE__, file=__FILE__, rcToReturn=rc)
          return  ! bail out
       else
          orb_obliq = SHR_ORB_UNDEF_REAL
          orb_eccen = SHR_ORB_UNDEF_REAL
          orb_mvelp = SHR_ORB_UNDEF_REAL
       endif
    elseif (trim(orb_mode) == trim(orb_fixed_parameters)) then
       !-- force orb_iyear to undef to make sure shr_orb_params works properly
       if (orb_eccen == SHR_ORB_UNDEF_REAL .or. orb_obliq == SHR_ORB_UNDEF_REAL .or. orb_mvelp == SHR_ORB_UNDEF_REAL) then
          if (mastertask) then
             write(logunit,*) trim(subname),' ERROR: invalid settings orb_mode =',trim(orb_mode)
             write(logunit,*) trim(subname),' ERROR: orb_eccen = ',orb_eccen
             write(logunit,*) trim(subname),' ERROR: orb_obliq = ',orb_obliq
             write(logunit,*) trim(subname),' ERROR: orb_mvelp = ',orb_mvelp
             write (msgstr, *) subname//' ERROR: invalid settings for orb_mode '//trim(orb_mode)
          end if
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msgstr, line=__LINE__, file=__FILE__, rcToReturn=rc)
          return  ! bail out
       else
          orb_iyear       = SHR_ORB_UNDEF_INT
          orb_iyear_align = SHR_ORB_UNDEF_INT
       endif
    else
       write (msgstr, *) subname//' ERROR: invalid orb_mode '//trim(orb_mode)
       call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msgstr, line=__LINE__, file=__FILE__, rcToReturn=rc)
       rc = ESMF_FAILURE
       return  ! bail out
    endif

  end subroutine dshr_orbital_init

  !===============================================================================
  subroutine dshr_orbital_update(clock, logunit,  mastertask, eccen, obliqr, lambm0, mvelpp, rc)

    !----------------------------------------------------------
    ! Update orbital settings
    !----------------------------------------------------------

    ! input/output variables
    type(ESMF_Clock) , intent(in)    :: clock
    integer          , intent(in)    :: logunit
    logical          , intent(in)    :: mastertask
    real(R8)         , intent(inout) :: eccen  ! orbital eccentricity
    real(R8)         , intent(inout) :: obliqr ! Earths obliquity in rad
    real(R8)         , intent(inout) :: lambm0 ! Mean long of perihelion at vernal equinox (radians)
    real(R8)         , intent(inout) :: mvelpp ! moving vernal equinox longitude of perihelion plus pi (radians)
    integer          , intent(out)   :: rc     ! output error

    ! local variables
    type(ESMF_Time)   :: CurrTime ! current time
    integer           :: year     ! model year at current time
    integer           :: orb_year ! orbital year for current orbital computation
    character(len=CL) :: msgstr   ! temporary
    logical           :: lprint
    logical           :: first_time = .true.
    character(len=*) , parameter :: subname = "(dshr_orbital_update)"
    !-------------------------------------------

    if (trim(orb_mode) == trim(orb_variable_year)) then
       call ESMF_ClockGet(clock, CurrTime=CurrTime, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       call ESMF_TimeGet(CurrTime, yy=year, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       orb_year = orb_iyear + (year - orb_iyear_align)
       lprint = mastertask
    else
       orb_year = orb_iyear
       if (first_time) then
          lprint = mastertask
          first_time = .false.
       else
          lprint = .false.
       end if
    end if

    eccen = orb_eccen
    call shr_orb_params(orb_year, eccen, orb_obliq, orb_mvelp, obliqr, lambm0, mvelpp, lprint)

    if ( eccen  == SHR_ORB_UNDEF_REAL .or. obliqr == SHR_ORB_UNDEF_REAL .or. &
         mvelpp == SHR_ORB_UNDEF_REAL .or. lambm0 == SHR_ORB_UNDEF_REAL) then
       write (msgstr, *) subname//' ERROR: orb params incorrect'
       write (logunit, *) msgstr
       call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msgstr, line=__LINE__, file=__FILE__, rcToReturn=rc)
       return  ! bail out
    endif

  end subroutine dshr_orbital_update

  !===============================================================================
  real(R8) function getNextRadCDay_i8( ymd, tod, stepno, dtime, iradsw, calendar )

    !  Return the calendar day of the next radiation time-step.
    !  General Usage: nextswday = getNextRadCDay(curr_date)

    ! input/output variables
    integer    , intent(in)    :: ymd
    integer    , intent(in)    :: tod
    integer(I8), intent(in)    :: stepno
    integer    , intent(in)    :: dtime
    integer    , intent(in)    :: iradsw
    character(*),intent(in)    :: calendar

    ! local variables
    real(R8) :: nextsw_cday
    real(R8) :: julday
    integer  :: liradsw
    character(*),parameter :: subName =  '(getNextRadCDay) '
    !-------------------------------------------------------------------------------

    liradsw = iradsw
    if (liradsw < 0) liradsw  = nint((-liradsw *3600._r8)/dtime)
    call shr_cal_date2julian(ymd,tod,julday,calendar)
    if (liradsw > 1) then
       if (mod(stepno+1,liradsw) == 0 .and. stepno > 0) then
          nextsw_cday = julday + 2*dtime/shr_const_cday
       else
          nextsw_cday = -1._r8
       end if
    else
       nextsw_cday = julday + dtime/shr_const_cday
    end if
    getNextRadCDay_i8 = nextsw_cday

  end function getNextRadCDay_i8

  !===============================================================================
  subroutine dshr_set_modelmask(mesh_dst, meshfile_mask, compname, mask_dst, frac_dst, rc)

    use ESMF, only : ESMF_FieldRegridStore, ESMF_FieldRegrid, ESMF_FIELDCREATE
    use ESMF, only : ESMF_REGRIDMETHOD_CONSERVE, ESMF_NORMTYPE_DSTAREA, ESMF_UNMAPPEDACTION_IGNORE
    use ESMF, only : ESMF_TYPEKIND_R8, ESMF_MESHLOC_ELEMENT
    use ESMF, only : ESMF_RouteHandleDestroy, ESMF_FieldDestroy

    ! input/out variables
    type(ESMF_Mesh)     , intent(in)  :: mesh_dst
    character(len=*)    , intent(in)  :: meshfile_mask
    character(len=*)    , intent(in)  :: compname
    integer , pointer   , intent(out) :: mask_dst(:)
    real(r8), pointer   , intent(out) :: frac_dst(:)
    integer             , intent(out) :: rc

    ! local variables:
    type(ESMF_Mesh)        :: mesh_mask
    type(ESMF_Field)       :: field_mask
    type(ESMF_Field)       :: field_dst
    type(ESMF_RouteHandle) :: rhandle
    integer                :: srcMaskValue = 0
    integer                :: dstMaskValue = -987987 ! spval for RH mask values
    integer                :: srcTermProcessing_Value = 0
    logical                :: checkflag = .false.
    real(r8) , pointer     :: mask_src(:) ! on mesh created from meshfile_mask
    real(r8) , pointer     :: dataptr1d(:)
    type(ESMF_DistGrid)    :: distgrid_mask
    type(ESMF_Array)       :: elemMaskArray
    integer                :: lsize_mask, lsize_dst
    integer                :: n, spatialDim
    real(r8)               :: fminval = 0.001_r8
    real(r8)               :: fmaxval = 1._r8
    real(r8)               :: lfrac,ofrac
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    mesh_mask = ESMF_MeshCreate(trim(meshfile_mask), fileformat=ESMF_FILEFORMAT_ESMFMESH, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_MeshGet(mesh_dst, spatialDim=spatialDim, numOwnedElements=lsize_dst, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    allocate(mask_dst(lsize_dst))
    allocate(frac_dst(lsize_dst))

    ! create fields on source and destination meshes
    field_mask = ESMF_FieldCreate(mesh_mask, ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    field_dst = ESMF_FieldCreate(mesh_dst, ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! create route handle to map source mask (assume ocean) to destination mesh (assume atm/lnd)
    call ESMF_FieldRegridStore(field_mask, field_dst, routehandle=rhandle, &
         srcMaskValues=(/srcMaskValue/), dstMaskValues=(/dstMaskValue/), &
         regridmethod=ESMF_REGRIDMETHOD_CONSERVE, normType=ESMF_NORMTYPE_DSTAREA, &
         srcTermProcessing=srcTermProcessing_Value, &
         ignoreDegenerate=.true., unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! fill in values for field_mask with mask on source mesh
    call ESMF_MeshGet(mesh_mask, elementdistGrid=distgrid_mask, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_DistGridGet(distgrid_mask, localDe=0, elementCount=lsize_mask, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    allocate(mask_src(lsize_mask))
    elemMaskArray = ESMF_ArrayCreate(distgrid_mask, mask_src, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    ! The following call fills in the values of mask_src
    call ESMF_MeshGet(mesh_mask, elemMaskArray=elemMaskArray, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    ! The following call fills in the values of field_mask
    call ESMF_FieldGet(field_mask, farrayptr=dataptr1d, rc=rc)
    dataptr1d(:) = mask_src(:)

    ! map source mask to destination mesh - to obtain destination mask and frac
    call ESMF_FieldRegrid(field_mask, field_dst, routehandle=rhandle, &
         termorderflag=ESMF_TERMORDER_SRCSEQ, checkflag=checkflag, zeroregion=ESMF_REGION_TOTAL, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! now determine mask_dst and frac_dst 
    call ESMF_MeshGet(mesh_dst, spatialDim=spatialDim, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldGet(field_dst, farrayptr=dataptr1d, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    do n = 1,lsize_dst
       lfrac = 1._r8 - dataptr1d(n)
       if (lfrac > fmaxval) lfrac = 1._r8
       if (lfrac < fminval) lfrac = 0._r8
       ofrac = 1._r8 - lfrac
       if (compname == 'LND') then
          frac_dst(n) = lfrac
          if (lfrac /= 0._r8) then
             mask_dst(n) = 1
          else
             mask_dst(n) = 0
          end if
       else if (compname == 'OCN' .or. compname == 'ICE') then
          frac_dst(n) = ofrac
          if (ofrac == 0._r8) then
             mask_dst(n) = 0
          else
             mask_dst(n) = 1
          end if
       end if
    enddo

    ! reset the model mesh mask
    call ESMF_MeshSet(mesh_dst, elementMask=mask_dst, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! deallocate memory
    call ESMF_RouteHandleDestroy(rhandle, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldDestroy(field_mask, rc=rc) 
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldDestroy(field_dst, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    deallocate(mask_src)

  end subroutine dshr_set_modelmask

  subroutine dshr_pio_init(gcomp, sdat, logunit, rc)

    ! ----------------------------------------------
    ! Initialize PIO
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_GridComp)   , intent(in)    :: gcomp
    type(shr_strdata_type), intent(inout) :: sdat
    integer               , intent(in)    :: logunit
    integer               , intent(out)   :: rc

    ! local variables:
    type(ESMF_VM)     :: vm
    integer           :: mpicom, my_task, petCount
    integer           :: pio_numiotasks
    integer           :: pio_stride
    integer           :: pio_rearranger
    integer           :: pio_root
    integer           :: pio_debug_level
    integer           :: pio_rearr_comm_type
    integer           :: pio_rearr_comm_fcd
    logical           :: pio_rearr_comm_enable_hs_comp2io
    logical           :: pio_rearr_comm_enable_isend_comp2io
    integer           :: pio_rearr_comm_max_pend_req_comp2io
    logical           :: pio_rearr_comm_enable_hs_io2comp
    logical           :: pio_rearr_comm_enable_isend_io2comp
    integer           :: pio_rearr_comm_max_pend_req_io2comp
    logical           :: isPresent, isSet, ret
    character(len=CL) :: cvalue
    character(len=CS) :: cname
    character(len=CL) :: logmsg
    character(len=CL) :: diro
    character(len=CL) :: logfile
    character(len=*),parameter  :: subname='(dshr_pio_init)'
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! generate local mpi comm
    call ESMF_GridCompGet(gcomp, vm=vm, name=cname, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMGet(vm, mpiCommunicator=mpicom, localPet=my_task, &
         petCount=petCount, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! query component specific PIO attributes 
    ! pio_netcdf_format
    call NUOPC_CompAttributeGet(gcomp, name='pio_netcdf_format', value=cvalue, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (isPresent .and. isSet) then
       cvalue = ESMF_UtilStringUpperCase(cvalue)
       if (trim(cvalue) .eq. 'CLASSIC') then
          sdat%io_format = 0
       else if (trim(cvalue) .eq. '64BIT_OFFSET') then
          sdat%io_format = PIO_64BIT_OFFSET
       else if (trim(cvalue) .eq. '64BIT_DATA') then
          sdat%io_format = PIO_64BIT_DATA
       else
         call ESMF_LogWrite(trim(subname)//'-'//trim(cname)// &
              ' : need to provide valid option for pio_ioformat &
              (CLASSIC|64BIT_OFFSET|64BIT_DATA)', ESMF_LOGMSG_INFO)
         rc = ESMF_FAILURE
         return
       end if
    else
       cvalue = '64BIT_OFFSET'
       sdat%io_format = PIO_64BIT_OFFSET
    end if
    if (my_task == master_task) write(logunit,*) trim(subname)//' : pio_netcdf_format = ', &
       trim(cvalue), sdat%io_format

    ! pio_typename
    call NUOPC_CompAttributeGet(gcomp, name='pio_typename', value=cvalue, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (isPresent .and. isSet) then
       cvalue = ESMF_UtilStringUpperCase(cvalue)
       if (trim(cvalue) .eq. 'NETCDF') then
          sdat%io_type = PIO_IOTYPE_NETCDF 
       else if (trim(cvalue) .eq. 'PNETCDF') then
          sdat%io_type = PIO_IOTYPE_PNETCDF 
       else if (trim(cvalue) .eq. 'NETCDF4C') then
          sdat%io_type = PIO_IOTYPE_NETCDF4C
       else if (trim(cvalue) .eq. 'NETCDF4P') then
          sdat%io_type = PIO_IOTYPE_NETCDF4P
       else
         call ESMF_LogWrite(trim(subname)//'-'//trim(cname)// &
              ' : need to provide valid option for pio_typename &
              (NETCDF|PNETCDF|NETCDF4C|NETCDF4P)', ESMF_LOGMSG_INFO)
         rc = ESMF_FAILURE
         return
       end if
    else
       cvalue = 'NETCDF'
       sdat%io_type = PIO_IOTYPE_NETCDF
    end if
    if (my_task == master_task) write(logunit,*) trim(subname)//' : pio_typename = ', &
       trim(cvalue), sdat%io_type

    ! pio_root
    call NUOPC_CompAttributeGet(gcomp, name='pio_root', value=cvalue, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (isPresent .and. isSet) then
       read(cvalue,*) pio_root
       if (pio_root < 0) then
          pio_root = 1
       endif
       pio_root = min(pio_root, petCount-1)
    else
       pio_root = 1 
    end if
    if (my_task == master_task) write(logunit,*) trim(subname)//' : pio_root = ', &
       pio_root

    ! pio_stride
    call NUOPC_CompAttributeGet(gcomp, name='pio_stride', value=cvalue, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (isPresent .and. isSet) then
       read(cvalue,*) pio_stride
    else
       pio_stride = -99
    end if
    if (my_task == master_task) write(logunit,*) trim(subname)//' : pio_stride = ', &
       pio_stride

    ! pio_numiotasks
    call NUOPC_CompAttributeGet(gcomp, name='pio_numiotasks', value=cvalue, &
        isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (isPresent .and. isSet) then
       read(cvalue,*) pio_numiotasks
    else
       pio_numiotasks = -99
    end if
    if (my_task == master_task) write(logunit,*) trim(subname)//' : pio_numiotasks = ', &
       pio_numiotasks

    ! check for parallel IO, it requires at least two io pes
    if (petCount > 1 .and. pio_numiotasks == 1 .and. &
       (sdat%io_type .eq. PIO_IOTYPE_PNETCDF .or. &
        sdat%io_type .eq. PIO_IOTYPE_NETCDF4P)) then
       pio_numiotasks = 2
       pio_stride = min(pio_stride, petCount/2)
       if (my_task == master_task) then
          write(logunit,*) ' parallel io requires at least two io pes - following parameters are updated:'
          write(logunit,*) trim(subname)//' : pio_stride = ', pio_stride
          write(logunit,*) trim(subname)//' : pio_numiotasks = ', pio_numiotasks
       end if
    endif

    ! check/set/correct io pio parameters
    if (pio_stride > 0 .and. pio_numiotasks < 0) then
       pio_numiotasks = max(1, petCount/pio_stride)
       if (my_task == master_task) write(logunit,*) trim(subname)//' : update pio_numiotasks = ', pio_numiotasks
    else if(pio_numiotasks > 0 .and. pio_stride < 0) then
       pio_stride = max(1, petCount/pio_numiotasks)
       if (my_task == master_task) write(logunit,*) trim(subname)//' : update pio_stride = ', pio_stride
    else if(pio_numiotasks < 0 .and. pio_stride < 0) then
       pio_stride = max(1,petCount/4)
       pio_numiotasks = max(1,petCount/pio_stride)
       if (my_task == master_task) write(logunit,*) trim(subname)//' : update pio_numiotasks = ', pio_numiotasks
       if (my_task == master_task) write(logunit,*) trim(subname)//' : update pio_stride = ', pio_stride
    end if
    if (pio_stride == 1) then
       pio_root = 0
    endif

    if (pio_root + (pio_stride)*(pio_numiotasks-1) >= petCount .or. &
       pio_stride <= 0 .or. pio_numiotasks <= 0 .or. &
       pio_root < 0 .or. pio_root > petCount-1) then
       if (petCount < 100) then
          pio_stride = max(1, petCount/4)
       else if(petCount < 1000) then
          pio_stride = max(1, petCount/8)
       else
          pio_stride = max(1, petCount/16)
       end if
       if(pio_stride > 1) then
          pio_numiotasks = petCount/pio_stride
          pio_root = min(1, petCount-1)
       else
          pio_numiotasks = petCount
          pio_root = 0
       end if
       if (my_task == master_task) then
          write(logunit,*) 'pio_stride, iotasks or root out of bounds - resetting to defaults:'
          write(logunit,*) trim(subname)//' : pio_root = ', pio_root
          write(logunit,*) trim(subname)//' : pio_stride = ', pio_stride
          write(logunit,*) trim(subname)//' : pio_numiotasks = ', pio_numiotasks
       end if
    end if

    ! init PIO
    allocate(sdat%pio_subsystem)
    if (my_task == master_task) write(logunit,*) trim(subname)//' : calling pio init'
    call pio_init(my_task, mpicom, pio_numiotasks, 0, pio_stride, &
                  pio_rearranger, sdat%pio_subsystem, base=pio_root)

    ! PIO debug related options
    ! pio_debug_level
    call NUOPC_CompAttributeGet(gcomp, name='pio_debug_level', value=cvalue, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (isPresent .and. isSet) then
       read(cvalue,*) pio_debug_level
       if (pio_debug_level < 0 .or. pio_debug_level > 6) then
         call ESMF_LogWrite(trim(subname)//': need to provide valid option for &
              pio_debug_level (0-6)', ESMF_LOGMSG_INFO)
         rc = ESMF_FAILURE
         return
       end if
    else
       pio_debug_level = 0
    end if
    if (my_task == master_task) write(logunit,*) trim(subname), &
       ' : pio_debug_level = ',pio_debug_level

    ! set PIO debug level
    call pio_setdebuglevel(pio_debug_level)

    ! pio_rearranger
    call NUOPC_CompAttributeGet(gcomp, name='pio_rearranger', value=cvalue, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       cvalue = ESMF_UtilStringUpperCase(cvalue)
       if (trim(cvalue) .eq. 'BOX') then
         pio_rearranger = PIO_REARR_BOX
       else if (trim(cvalue) .eq. 'SUBSET') then
         pio_rearranger = PIO_REARR_SUBSET
       else
         call ESMF_LogWrite(trim(subname)//'-'//trim(cname)// &
              ' : need to provide valid option for pio_rearranger &
              (BOX|SUBSET)', ESMF_LOGMSG_INFO)
         rc = ESMF_FAILURE
         return
       end if
    else
       cvalue = 'BOX'
       pio_rearranger = PIO_REARR_BOX
    end if
    if (my_task == master_task) write(logunit,*) trim(subname)//' : pio_rearranger = ', &
       trim(cvalue), pio_rearranger

    ! init PIO
    allocate(sdat%pio_subsystem)
    if (my_task == master_task) write(logunit,*) trim(subname)//' : calling pio init'
    call pio_init(my_task, mpicom, pio_numiotasks, 0, pio_stride, &
                  pio_rearranger, sdat%pio_subsystem, base=pio_root)

    ! query shared PIO rearranger attributes 
    ! pio_rearr_comm_type 
    call NUOPC_CompAttributeGet(gcomp, name='pio_rearr_comm_type', value=cvalue, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (isPresent .and. isSet) then
       cvalue = ESMF_UtilStringUpperCase(cvalue)
       if (trim(cvalue) .eq. 'P2P') then
          pio_rearr_comm_type = PIO_REARR_COMM_P2P
       else if (trim(cvalue) .eq. 'COLL') then
          pio_rearr_comm_type = PIO_REARR_COMM_COLL
       else
         call ESMF_LogWrite(trim(subname)//' : need to provide valid option for &
              pio_rearr_comm_type (P2P|COLL)', ESMF_LOGMSG_INFO)
         rc = ESMF_FAILURE
         return
       end if
    else
       cvalue = 'P2P'
       pio_rearr_comm_type = PIO_REARR_COMM_P2P
    end if
    if (my_task == master_task) write(logunit,*) trim(subname)// &
       ' : pio_rearr_comm_type = ', trim(cvalue), pio_rearr_comm_type 

    ! pio_rearr_comm_fcd
    call NUOPC_CompAttributeGet(gcomp, name='pio_rearr_comm_fcd', value=cvalue, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (isPresent .and. isSet) then
       cvalue = ESMF_UtilStringUpperCase(cvalue)
       if (trim(cvalue) .eq. '2DENABLE') then
          pio_rearr_comm_fcd = PIO_REARR_COMM_FC_2D_ENABLE
       else if (trim(cvalue) .eq. 'IO2COMP') then
          pio_rearr_comm_fcd = PIO_REARR_COMM_FC_1D_IO2COMP
       else if (trim(cvalue) .eq. 'COMP2IO') then
          pio_rearr_comm_fcd = PIO_REARR_COMM_FC_1D_COMP2IO
       else if (trim(cvalue) .eq. '2DDISABLE') then
          pio_rearr_comm_fcd = PIO_REARR_COMM_FC_2D_DISABLE
       else
         call ESMF_LogWrite(trim(subname)//' : need to provide valid option for &
              pio_rearr_comm_fcd (2DENABLE|IO2COMP|COMP2IO|2DDISABLE)', ESMF_LOGMSG_INFO)
         rc = ESMF_FAILURE
         return
       end if
    else
       cvalue = '2DENABLE'
       pio_rearr_comm_fcd = PIO_REARR_COMM_FC_2D_ENABLE
    end if
    if (my_task == master_task) write(logunit,*) trim(subname)// &
       ' : pio_rearr_comm_fcd = ', trim(cvalue), pio_rearr_comm_fcd

    ! pio_rearr_comm_enable_hs_comp2io
    call NUOPC_CompAttributeGet(gcomp, name='pio_rearr_comm_enable_hs_comp2io', &
         value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (isPresent .and. isSet) then
       read(cvalue,*) pio_rearr_comm_enable_hs_comp2io
    else
       pio_rearr_comm_enable_hs_comp2io = .true.
    end if

    ! pio_rearr_comm_enable_isend_comp2io
    call NUOPC_CompAttributeGet(gcomp, name='pio_rearr_comm_enable_isend_comp2io', &
         value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (isPresent .and. isSet) then
       read(cvalue,*) pio_rearr_comm_enable_isend_comp2io
    else
       pio_rearr_comm_enable_isend_comp2io = .false.
    end if
   
    ! pio_rearr_comm_max_pend_req_comp2io
    call NUOPC_CompAttributeGet(gcomp, name='pio_rearr_comm_max_pend_req_comp2io', &
         value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (isPresent .and. isSet) then
       read(cvalue,*) pio_rearr_comm_max_pend_req_comp2io
    else
       pio_rearr_comm_max_pend_req_comp2io = 0
    end if

    ! pio_rearr_comm_enable_hs_io2comp
    call NUOPC_CompAttributeGet(gcomp, name='pio_rearr_comm_enable_hs_io2comp', &
         value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (isPresent .and. isSet) then
       read(cvalue,*) pio_rearr_comm_enable_hs_io2comp
    else
       pio_rearr_comm_enable_hs_io2comp = .false.
    end if

    ! pio_rearr_comm_enable_isend_io2comp
    call NUOPC_CompAttributeGet(gcomp, name='pio_rearr_comm_enable_isend_io2comp', &
         value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (isPresent .and. isSet) then
       read(cvalue,*) pio_rearr_comm_enable_isend_io2comp
    else
       pio_rearr_comm_enable_isend_io2comp = .true.
    end if

    ! pio_rearr_comm_max_pend_req_io2comp
    call NUOPC_CompAttributeGet(gcomp, name='pio_rearr_comm_max_pend_req_io2comp', &
         value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (isPresent .and. isSet) then
       read(cvalue,*) pio_rearr_comm_max_pend_req_io2comp
    else
       pio_rearr_comm_max_pend_req_io2comp = 64
    end if

    ! print out PIO rearranger parameters
    if (my_task == master_task) then
       write(logunit,*) trim(subname)//' : pio_rearr_comm_enable_hs_comp2io = ', &
            pio_rearr_comm_enable_hs_comp2io
       write(logunit,*) trim(subname)//' : pio_rearr_comm_enable_isend_comp2io = ', &
            pio_rearr_comm_enable_isend_comp2io
       write(logunit,*) trim(subname)//' : pio_rearr_comm_max_pend_req_comp2io = ', &
            pio_rearr_comm_max_pend_req_comp2io
       write(logunit,*) trim(subname)//' : pio_rearr_comm_enable_hs_io2comp = ', &
            pio_rearr_comm_enable_hs_io2comp
       write(logunit,*) trim(subname)//' : pio_rearr_comm_enable_isend_io2comp = ', &
            pio_rearr_comm_enable_isend_io2comp
       write(logunit,*) trim(subname)//' : pio_rearr_comm_max_pend_req_io2comp = ', &
            pio_rearr_comm_max_pend_req_io2comp
    end if

    ! set PIO rearranger options
    if (my_task == master_task) write(logunit,*) trim(subname)// &
       ' calling pio_set_rearr_opts'
    ret = pio_set_rearr_opts(sdat%pio_subsystem, pio_rearr_comm_type, &
                             pio_rearr_comm_fcd, &
                             pio_rearr_comm_enable_hs_comp2io, &
                             pio_rearr_comm_enable_isend_comp2io, &
                             pio_rearr_comm_max_pend_req_comp2io, &
                             pio_rearr_comm_enable_hs_io2comp, &
                             pio_rearr_comm_enable_isend_io2comp, &
                             pio_rearr_comm_max_pend_req_io2comp)

  end subroutine dshr_pio_init

end module dshr_mod
