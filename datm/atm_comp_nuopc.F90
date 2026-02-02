#ifdef CESMCOUPLED
module atm_comp_nuopc
#else
module cdeps_datm_comp
#endif

  !----------------------------------------------------------------------------
  ! This is the NUOPC cap for DATM
  !----------------------------------------------------------------------------

  use ESMF             , only : ESMF_VM, ESMF_VMBroadcast
  use ESMF             , only : ESMF_Mesh, ESMF_GridComp, ESMF_SUCCESS, ESMF_LogWrite
  use ESMF             , only : ESMF_GridCompSetEntryPoint, ESMF_METHOD_INITIALIZE
  use ESMF             , only : ESMF_MethodRemove, ESMF_State, ESMF_Clock, ESMF_TimeInterval
  use ESMF             , only : ESMF_State, ESMF_Field, ESMF_LOGMSG_INFO, ESMF_ClockGet
  use ESMF             , only : ESMF_Time, ESMF_Alarm, ESMF_TimeGet, ESMF_TimeInterval
  use ESMF             , only : operator(+), ESMF_TimeIntervalGet, ESMF_ClockGetAlarm
  use ESMF             , only : ESMF_AlarmIsRinging, ESMF_AlarmRingerOff, ESMF_StateGet
  use ESMF             , only : ESMF_FieldGet, ESMF_MAXSTR, ESMF_VMBroadcast
  use ESMF             , only : ESMF_TraceRegionEnter, ESMF_TraceRegionExit, ESMF_GridCompGet
  use NUOPC            , only : NUOPC_CompDerive, NUOPC_CompSetEntryPoint, NUOPC_CompSpecialize
  use NUOPC            , only : NUOPC_CompAttributeGet, NUOPC_Advertise
  use NUOPC_Model      , only : model_routine_SS        => SetServices
  use NUOPC_Model      , only : model_label_Advance     => label_Advance
  use NUOPC_Model      , only : model_label_SetRunClock => label_SetRunClock
  use NUOPC_Model      , only : model_label_Finalize    => label_Finalize
  use NUOPC_Model      , only : NUOPC_ModelGet, setVM
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_kind_mod     , only : cx=>shr_kind_cx
  use shr_const_mod    , only : shr_const_cday
  use shr_cal_mod      , only : shr_cal_ymd2date
  use shr_log_mod      , only : shr_log_setLogUnit, shr_log_error
  use dshr_methods_mod , only : dshr_state_diagnose, chkerr, memcheck
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_init_from_config, shr_strdata_advance
  use dshr_strdata_mod , only : shr_strdata_get_stream_pointer, shr_strdata_setOrbs
  use dshr_mod         , only : dshr_model_initphase, dshr_init, dshr_restart_write
  use dshr_mod         , only : dshr_state_setscalar, dshr_set_runclock, dshr_log_clock_advance
  use dshr_mod         , only : dshr_mesh_init, dshr_check_restart_alarm, dshr_restart_read
  use dshr_mod         , only : dshr_orbital_init, dshr_orbital_update
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add, dshr_fldlist_realize

  use datm_datamode_core2_mod   , only : datm_datamode_core2_advertise
  use datm_datamode_core2_mod   , only : datm_datamode_core2_init_pointers
  use datm_datamode_core2_mod   , only : datm_datamode_core2_advance

  use datm_datamode_jra_mod     , only : datm_datamode_jra_advertise
  use datm_datamode_jra_mod     , only : datm_datamode_jra_init_pointers
  use datm_datamode_jra_mod     , only : datm_datamode_jra_advance

  use datm_datamode_clmncep_mod , only : datm_datamode_clmncep_advertise
  use datm_datamode_clmncep_mod , only : datm_datamode_clmncep_init_pointers
  use datm_datamode_clmncep_mod , only : datm_datamode_clmncep_advance

  use datm_datamode_cplhist_mod , only : datm_datamode_cplhist_advertise
  use datm_datamode_cplhist_mod , only : datm_datamode_cplhist_init_pointers
  use datm_datamode_cplhist_mod , only : datm_datamode_cplhist_advance

  use datm_datamode_era5_mod    , only : datm_datamode_era5_advertise
  use datm_datamode_era5_mod    , only : datm_datamode_era5_init_pointers
  use datm_datamode_era5_mod    , only : datm_datamode_era5_advance

  use datm_datamode_gefs_mod    , only : datm_datamode_gefs_advertise
  use datm_datamode_gefs_mod    , only : datm_datamode_gefs_init_pointers
  use datm_datamode_gefs_mod    , only : datm_datamode_gefs_advance

  use datm_datamode_simple_mod  , only : datm_datamode_simple_advertise
  use datm_datamode_simple_mod  , only : datm_datamode_simple_init_pointers
  use datm_datamode_simple_mod  , only : datm_datamode_simple_advance

  use datm_pres_ndep_mod        , only : datm_pres_ndep_advertise
  use datm_pres_ndep_mod        , only : datm_pres_ndep_init_pointers
  use datm_pres_ndep_mod        , only : datm_pres_ndep_advance

  use datm_pres_aero_mod        , only : datm_pres_aero_advertise
  use datm_pres_aero_mod        , only : datm_pres_aero_init_pointers
  use datm_pres_aero_mod        , only : datm_pres_aero_advance

  use datm_pres_o3_mod          , only : datm_pres_o3_advertise
  use datm_pres_o3_mod          , only : datm_pres_o3_init_pointers
  use datm_pres_o3_mod          , only : datm_pres_o3_advance

  use datm_pres_co2_mod         , only : datm_pres_co2_advertise
  use datm_pres_co2_mod         , only : datm_pres_co2_init_pointers
  use datm_pres_co2_mod         , only : datm_pres_co2_advance

  implicit none
  private

  public  :: SetServices
  public  :: SetVM

  private :: InitializeAdvertise
  private :: InitializeRealize
  private :: ModelAdvance
  private :: datm_comp_run
  private :: ModelFinalize

  !--------------------------------------------------------------------------
  ! Private module data
  !--------------------------------------------------------------------------

  type(shr_strdata_type)       :: sdat
  type(ESMF_Mesh)              :: model_mesh                ! model mesh
  character(len=128)           :: flds_scalar_name = ''
  integer                      :: flds_scalar_num = 0
  integer                      :: flds_scalar_index_nx = 0
  integer                      :: flds_scalar_index_ny = 0
  integer                      :: flds_scalar_index_nextsw_cday = 0
  integer                      :: mpicom                    ! mpi communicator
  integer                      :: my_task                   ! my task in mpi communicator mpicom
  logical                      :: mainproc                  ! true of my_task == main_task
  integer                      :: inst_index                ! number of current instance (ie. 1)
  character(len=16)            :: inst_suffix = ""          ! char string associated with instance (ie. "_0001" or "")
  integer                      :: logunit                   ! logging unit number
  logical                      :: restart_read              ! start from restart
  character(CL)                :: case_name                 ! case name
  character(len=*) , parameter :: nullstr = 'null'

  ! datm_in namelist input
  character(CX)                :: nlfilename = nullstr                ! filename to obtain namelist info from
  character(CX)                :: streamfilename = nullstr            ! filename to obtain stream info from
  character(CL)                :: dataMode = nullstr                  ! flags physics options wrt input data
  character(CX)                :: model_meshfile = nullstr            ! full pathname to model meshfile
  character(CX)                :: model_maskfile = nullstr            ! full pathname to obtain mask from
  integer                      :: iradsw = 0                          ! radiation interval (input namelist)
  logical                      :: nextsw_cday_calc_cam7               ! true => use logic appropriate to cam7 (and later) for calculating nextsw_cday
  character(CL)                :: factorFn_mesh = 'null'              ! file containing correction factors mesh
  character(CL)                :: factorFn_data = 'null'              ! file containing correction factors data

  logical                      :: flds_presaero = .false.             ! true => send valid prescribed aero fields to mediator
  logical                      :: flds_presndep = .false.             ! true => send valid prescribed ndep fields to mediator
  logical                      :: flds_preso3 = .false.               ! true => send valid prescribed ozone fields to mediator
  logical                      :: flds_co2 = .false.                  ! true => send prescribed co2 to mediator

  character(CL)                :: bias_correct = nullstr              ! send bias correction fields to coupler
  character(CL)                :: anomaly_forcing(8) = nullstr        ! send anomaly forcing fields to coupler

  character(CX)                :: restfilm = nullstr                  ! model restart file namelist
  integer                      :: nx_global                           ! global nx
  integer                      :: ny_global                           ! global ny
  logical                      :: skip_restart_read = .false.         ! true => skip restart read in continuation run
  logical                      :: export_all = .false.                ! true => export all fields, do not check connected or not
  logical                      :: first_call = .true.

  ! linked lists
  type(fldList_type) , pointer :: fldsImport => null()
  type(fldList_type) , pointer :: fldsExport => null()

  ! model mask and model fraction
  real(r8), pointer            :: model_frac(:) => null()
  integer , pointer            :: model_mask(:) => null()

  ! constants
  integer                      :: idt                               ! integer model timestep
  logical                      :: diagnose_data = .true.
  integer          , parameter :: main_task   = 0                   ! task number of main task
#ifdef CESMCOUPLED
  character(len=*) , parameter :: modName = "(atm_comp_nuopc)"
#else
  character(len=*) , parameter :: modName = "(cdeps_datm_comp)"
#endif

  character(len=*) , parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(len=*),parameter  :: subname = modName//':(SetServices) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    ! the NUOPC gcomp component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! switching to IPD versions
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         userRoutine=dshr_model_initphase, phase=0, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         phaseLabelList=(/"IPDv01p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         phaseLabelList=(/"IPDv01p3"/), userRoutine=InitializeRealize, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, specRoutine=ModelAdvance, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_MethodRemove(gcomp, label=model_label_SetRunClock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_SetRunClock, specRoutine=dshr_set_runclock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, specRoutine=ModelFinalize, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine SetServices

  !===============================================================================

  subroutine InitializeAdvertise(gcomp, importState, exportState, clock, rc)
    use shr_nl_mod, only:  shr_nl_find_group_name
    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer           :: nu         ! unit number
    integer           :: ierr       ! error code
    integer           :: bcasttmp(10)
    character(CL)     :: nextsw_cday_calc
    type(ESMF_VM)     :: vm
    character(len=*),parameter :: subname=trim(modName) // ':(InitializeAdvertise) '
    !-------------------------------------------------------------------------------

    namelist / datm_nml / &
         datamode, &
         model_meshfile, &
         model_maskfile, &
         nx_global, &
         ny_global, &
         restfilm, &
         iradsw, &
         nextsw_cday_calc, &
         factorFn_data, &
         factorFn_mesh, &
         flds_presaero, &
         flds_co2, &
         bias_correct, &
         anomaly_forcing, &
         skip_restart_read, &
         flds_presndep, &
         flds_preso3, &
         export_all

    rc = ESMF_SUCCESS

    ! Initialize locally-declared namelist items to default values
    nextsw_cday_calc = 'cam6'

    call NUOPC_CompAttributeGet(gcomp, name='case_name', value=case_name, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Obtain flds_scalar values, mpi values, multi-instance values and
    ! set logunit and set shr logging to my log file
    call dshr_init(gcomp, 'ATM', mpicom, my_task, inst_index, inst_suffix, &
         flds_scalar_name, flds_scalar_num, flds_scalar_index_nx, flds_scalar_index_ny, &
         logunit, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Determine logical mainproc
    mainproc = (my_task == main_task)

    ! Read atm_nml from nlfilename
    if (my_task == main_task) then
       nlfilename = "datm_in"//trim(inst_suffix)
       open (newunit=nu,file=trim(nlfilename),status="old",action="read")
       call shr_nl_find_group_name(nu, 'datm_nml', status=ierr)
       if (ierr > 0) then
          rc = ierr
          call shr_log_error(subName//': namelist read error '//trim(nlfilename), rc=rc)
          return
       end if
       read (nu,nml=datm_nml,iostat=ierr)
       close(nu)
       if (ierr > 0) then
          rc = ierr
          call shr_log_error(subName//': namelist read error '//trim(nlfilename), rc=rc)
          return
       end if

       ! write namelist input to standard out
       write(logunit,'(3a)')    subname,' case_name         = ',trim(case_name)
       write(logunit,'(3a)')    subname,' datamode          = ',trim(datamode)
       write(logunit,'(3a)')    subname,' model_meshfile    = ',trim(model_meshfile)
       write(logunit,'(3a)')    subname,' model_maskfile    = ',trim(model_maskfile)
       write(logunit,'(2a,i0)') subname,' nx_global         = ',nx_global
       write(logunit,'(2a,i0)') subname,' ny_global         = ',ny_global
       write(logunit,'(3a)')    subname,' restfilm          = ',trim(restfilm)
       write(logunit,'(2a,i0)') subname,' iradsw            = ',iradsw
       write(logunit,'(3a)')    subname,' nextsw_cday_calc  = ', trim(nextsw_cday_calc)
       write(logunit,'(3a)')    subname,' factorFn_data     = ',trim(factorFn_data)
       write(logunit,'(3a)')    subname,' factorFn_mesh     = ',trim(factorFn_mesh)
       write(logunit,'(2a,l6)') subname,' flds_presaero     = ',flds_presaero
       write(logunit,'(2a,l6)') subname,' flds_presndep     = ',flds_presndep
       write(logunit,'(2a,l6)') subname,' flds_preso3       = ',flds_preso3
       write(logunit,'(2a,l6)') subname,' flds_co2          = ',flds_co2
       write(logunit,'(2a,l6)') subname,' skip_restart_read = ',skip_restart_read
       write(logunit,'(2a,l6)') subname,' export_all        = ',export_all

       bcasttmp = 0
       bcasttmp(1) = nx_global
       bcasttmp(2) = ny_global
       bcasttmp(3) = iradsw
       if(flds_presaero)     bcasttmp(4) = 1
       if(flds_presndep)     bcasttmp(5) = 1
       if(flds_preso3)       bcasttmp(6) = 1
       if(flds_co2)          bcasttmp(7) = 1
       if(skip_restart_read) bcasttmp(8) = 1
       if(export_all)        bcasttmp(9) = 1
    end if

    ! Broadcast namelist input
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, datamode, CL, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, bias_correct, CL, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, anomaly_forcing, CL*8, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, model_meshfile, CX, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, model_maskfile, CX, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, factorFn_data, CL, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, factorFn_mesh, CL, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, restfilm, CX, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, nextsw_cday_calc, CL, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, bcasttmp, 10, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    nx_global         = bcasttmp(1)
    ny_global         = bcasttmp(2)
    iradsw            = bcasttmp(3)
    flds_presaero     = (bcasttmp(4) == 1)
    flds_presndep     = (bcasttmp(5) == 1)
    flds_preso3       = (bcasttmp(6) == 1)
    flds_co2          = (bcasttmp(7) == 1)
    skip_restart_read = (bcasttmp(8) == 1)
    export_all        = (bcasttmp(9) == 1)

    if (nextsw_cday_calc == 'cam7') then
       nextsw_cday_calc_cam7 = .true.
    else if (nextsw_cday_calc == 'cam6') then
       nextsw_cday_calc_cam7 = .false.
    else
       call shr_log_error(' ERROR illegal datm nextsw_cday_calc = '//trim(nextsw_cday_calc), rc=rc)
       return
    end if

    ! Validate sdat datamode
    if (mainproc) write(logunit,*) ' datm datamode = ',trim(datamode)
    select case (trim(datamode))
       case ('CORE2_NYF','CORE2_IAF','CORE_IAF_JRA', &
             'CORE_RYF6162_JRA','CORE_RYF8485_JRA','CORE_RYF9091_JRA','CORE_RYF0304_JRA', &
             'CLMNCEP','CPLHIST','GEFS','ERA5','SIMPLE')
       if (mainproc) write(logunit,'(3a)') subname,'datm datamode = ',trim(datamode)
    case default
       call shr_log_error(' ERROR illegal datm datamode = '//trim(datamode), rc=rc)
       return
    end select

    ! Advertise fields that ARE NOT datamode specific
    if (flds_co2) then
       call datm_pres_co2_advertise(fldsExport, datamode)
    end if
    if (flds_preso3) then
       call datm_pres_o3_advertise(fldsExport)
    end if
    if (flds_presndep) then
       call datm_pres_ndep_advertise(fldsExport)
    end if
    if (flds_presaero) then
       call datm_pres_aero_advertise(fldsExport)
    end if

    ! Advertise fields that ARE datamode specific
    select case (trim(datamode))
    case ('CORE2_NYF', 'CORE2_IAF')
       call datm_datamode_core2_advertise(exportState, fldsExport, flds_scalar_name, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case ('CORE_IAF_JRA', 'CORE_RYF6162_JRA', 'CORE_RYF8485_JRA', 'CORE_RYF9091_JRA', 'CORE_RYF0304_JRA')
       call datm_datamode_jra_advertise(exportState, fldsExport, flds_scalar_name, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case ('CLMNCEP')
       call datm_datamode_clmncep_advertise(exportState, fldsExport, flds_scalar_name, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case ('CPLHIST')
       call datm_datamode_cplhist_advertise(exportState, fldsExport, flds_scalar_name, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case ('ERA5')
       call datm_datamode_era5_advertise(exportState, fldsExport, flds_scalar_name, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case ('GEFS')
       call datm_datamode_gefs_advertise(exportState, fldsExport, flds_scalar_name, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case ('SIMPLE')
       call datm_datamode_simple_advertise(exportState, fldsExport, flds_scalar_name, &
            nlfilename, my_task, vm, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end select

  end subroutine InitializeAdvertise

  !===============================================================================

  subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_TIME)         :: currTime
    integer                 :: current_ymd   ! model date
    integer                 :: current_year  ! model year
    integer                 :: current_mon   ! model month
    integer                 :: current_day   ! model day
    integer                 :: current_tod   ! model sec into model date
    integer(i8)             :: stepno        ! step number
    real(r8)                :: nextsw_cday   ! calendar of next atm sw
    character(CL)           :: cvalue        ! character string for input config
    real(R8)                :: orbEccen      ! orb eccentricity (unist-less)
    real(R8)                :: orbMvelpp     ! orb moving vernal eqa (radians)
    real(R8)                :: orbLambm0     ! orb mean long of perhelion (radians)
    real(R8)                :: orbObliqr     ! orb obliquity (radians)
    logical                 :: isPresent, isSet
    real(R8)                :: dayofYear
    character(len=*), parameter :: subname = modName//':(InitializeRealize) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    ! Initialize mesh, restart flag, compid, and logunit
    call ESMF_TraceRegionEnter('datm_strdata_init')
    call dshr_mesh_init(gcomp, sdat, nullstr, logunit, 'ATM', nx_global, ny_global, &
         model_meshfile, model_maskfile, model_mesh, model_mask, model_frac, restart_read, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Initialize stream data type
    streamfilename = 'datm.streams'//trim(inst_suffix)
#ifndef DISABLE_FoX
    streamfilename = trim(streamfilename)//'.xml'
#endif
    call shr_strdata_init_from_config(sdat, streamfilename, model_mesh, clock, 'ATM', logunit, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TraceRegionExit('datm_strdata_init')

    ! NUOPC_Realize "realizes" a previously advertised field in the importState and exportState
    ! by replacing the advertised fields with the newly created fields of the same name.
    call dshr_fldlist_realize( exportState, fldsExport, flds_scalar_name, flds_scalar_num, model_mesh, &
         subname//':datmExport', export_all, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_fldlist_realize( importState, fldsImport, flds_scalar_name, flds_scalar_num, model_mesh, &
         subname//':datmImport', .false., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Get the time to interpolate the stream data to
    call ESMF_ClockGet( clock, currTime=currTime, timeStep=timeStep, advanceCount=stepno, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(currTime, yy=current_year, mm=current_mon, dd=current_day, s=current_tod, &
         dayofYear_r8=dayofYear, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_cal_ymd2date(current_year, current_mon, current_day, current_ymd)

    ! Get model timestep (idt is module variable)
    call ESMF_TimeIntervalGet( timeStep, s=idt, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Initialize and update orbital values
    call dshr_orbital_init(gcomp, logunit, my_task == main_task, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_orbital_update(currTime, logunit, my_task == main_task, &
         orbEccen, orbObliqr, orbLambm0, orbMvelpp, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Run datm
    call datm_comp_run(gcomp, importstate, exportstate, current_ymd, current_tod, current_mon, &
         orbEccen, orbMvelpp, orbLambm0, orbObliqr, restart_write=.false., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Add scalars to export state
    call dshr_state_SetScalar(dble(nx_global),flds_scalar_index_nx, exportState, flds_scalar_name, flds_scalar_num, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_SetScalar(dble(ny_global),flds_scalar_index_ny, exportState, flds_scalar_name, flds_scalar_num, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldIdxNextSwCday", value=cvalue, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       read (cvalue,*) flds_scalar_index_nextsw_cday
    else
       call shr_log_error(subname//'Need to set attribute ScalarFieldIdxNextSwCday', rc=rc)
       return
    endif

    nextsw_cday = getNextRadCDay(dayofYear, current_tod, stepno, idt, iradsw)
    call dshr_state_SetScalar(nextsw_cday, flds_scalar_index_nextsw_cday, exportState, flds_scalar_name, flds_scalar_num, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine InitializeRealize

  !===============================================================================
  subroutine ModelAdvance(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: clock
    type(ESMF_Time)         :: currTime
    type(ESMF_Time)         :: nextTime
    type(ESMF_TimeInterval) :: timeStep
    real(r8)                :: nextsw_cday
    logical                 :: restart_write         ! restart alarm is ringing
    integer                 :: next_ymd      ! model date
    integer                 :: next_tod      ! model sec into model date
    integer                 :: yr, mon, day  ! year, month, day
    integer(i8)             :: stepno        ! step number
    real(R8)                :: orbEccen      ! orb eccentricity (unit-less)
    real(R8)                :: orbMvelpp     ! orb moving vernal eq (radians)
    real(R8)                :: orbLambm0     ! orb mean long of perhelion (radians)
    real(R8)                :: orbObliqr     ! orb obliquity (radians)
    real(R8)                :: dayofYear
    character(len=*),parameter  :: subname = modName//':(ModelAdvance) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_TraceRegionEnter(subname)
    call shr_log_setLogUnit(logunit)
    call memcheck(subname, 5, my_task==main_task)

    ! Query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(gcomp, modelClock=clock, importState=importState, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! For nuopc - the component clock is advanced at the end of the time interval
    ! For these to match for now - need to advance nuopc one timestep ahead for
    ! shr_strdata time interpolation
    call ESMF_ClockGet( clock, currTime=currTime, timeStep=timeStep, advanceCount=stepno, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    nextTime = currTime + timeStep
    call ESMF_TimeGet( nextTime, yy=yr, mm=mon, dd=day, s=next_tod, dayofYear_r8=dayofYear, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_cal_ymd2date(yr, mon, day, next_ymd)

    ! Update the orbital values
    call dshr_orbital_update(nextTime, logunit, my_task == main_task, &
         orbEccen, orbObliqr, orbLambm0, orbMvelpp, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    restart_write = dshr_check_restart_alarm(clock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Run datm
    call ESMF_TraceRegionEnter('datm_run')
    call datm_comp_run(gcomp, importstate, exportstate, next_ymd, next_tod, mon, &
         orbEccen, orbMvelpp, orbLambm0, orbObliqr, restart_write,  rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TraceRegionExit('datm_run')

    ! Update nextsw_cday for scalar data
    ! Use nextYMD and nextTOD here since since the component - clock is advance at the END of the time interval

    nextsw_cday = getNextRadCDay(dayofYear, next_tod, stepno, idt, iradsw)

    call dshr_state_SetScalar(nextsw_cday, flds_scalar_index_nextsw_cday, exportState, flds_scalar_name, flds_scalar_num, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TraceRegionExit(subname)

  end subroutine ModelAdvance

  !===============================================================================
  subroutine datm_comp_run(gcomp, importState, exportState, target_ymd, target_tod, target_mon, &
       orbEccen, orbMvelpp, orbLambm0, orbObliqr, restart_write, rc)

    use nuopc_shr_methods, only : shr_get_rpointer_name

    ! ----------------------------------
    ! run method for datm model
    ! ----------------------------------

    ! input/output variables
    type(ESMF_GridComp)    , intent(inout) :: gcomp
    type(ESMF_State)       , intent(inout) :: importState
    type(ESMF_State)       , intent(inout) :: exportState
    integer                , intent(in)    :: target_ymd       ! model date
    integer                , intent(in)    :: target_tod       ! model sec into model date
    integer                , intent(in)    :: target_mon       ! model month
    real(R8)               , intent(in)    :: orbEccen         ! orb eccentricity (unit-less)
    real(R8)               , intent(in)    :: orbMvelpp        ! orb moving vernal eq (radians)
    real(R8)               , intent(in)    :: orbLambm0        ! orb mean long of perhelion (radians)
    real(R8)               , intent(in)    :: orbObliqr        ! orb obliquity (radians)
    logical                , intent(in)    :: restart_write
    integer                , intent(out)   :: rc

    ! local variables
    character(len=CL) :: rpfile
    character(len=*), parameter :: subName = '(datm_comp_run) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_TraceRegionEnter('DATM_RUN')

    !--------------------
    ! First time initialization
    !--------------------

    if_first_call: if (first_call) then
       ! Initialize data pointers for co2 (non datamode specific)
       if (flds_co2) then
          call datm_pres_co2_init_pointers(exportState, sdat, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       end if

       ! Initialize data pointers for o3 (non datamode specific)
       if (flds_preso3) then
          call datm_pres_o3_init_pointers(exportState, sdat, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       end if

       ! Initialize data pointers for nitrogen deposition (non datamode specific and use of ungridded dimensions)
       if (flds_presndep) then
          call datm_pres_ndep_init_pointers(exportState, sdat, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       end if

       ! Initialize data pointers for prescribed aerosols (non datamode specific and use of ungridded dimensions)
       if (flds_presaero) then
          call datm_pres_aero_init_pointers(exportState, sdat, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       end if

       ! Initialize datamode module pointers
       select case (trim(datamode))
       case('CORE2_NYF','CORE2_IAF')
          call datm_datamode_core2_init_pointers(exportState, sdat, datamode, factorfn_mesh, factorfn_data, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       case ('CORE_IAF_JRA', 'CORE_RYF6162_JRA', 'CORE_RYF8485_JRA', 'CORE_RYF9091_JRA', 'CORE_RYF0304_JRA')
          call datm_datamode_jra_init_pointers(exportState, sdat, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       case('CLMNCEP')
          call datm_datamode_clmncep_init_pointers(importState, exportState, sdat, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       case('CPLHIST')
          call datm_datamode_cplhist_init_pointers(importState, exportState, sdat, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       case('ERA5')
          call datm_datamode_era5_init_pointers(exportState, sdat, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       case('GEFS')
          call datm_datamode_gefs_init_pointers(exportState, sdat, logunit, mainproc, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       case('SIMPLE')
          call datm_datamode_simple_init_pointers(exportState, sdat, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       end select

       ! Read restart if needed
       if (restart_read .and. .not. skip_restart_read) then
          call shr_get_rpointer_name(gcomp, 'atm', target_ymd, target_tod, rpfile, 'read', rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          select case (trim(datamode))
          case('CORE2_NYF','CORE2_IAF','CORE_IAF_JRA',&
               'CORE_RYF6162_JRA','CORE_RYF8485_JRA' ,&
               'CORE_RYF9091_JRA','CORE_RYF0304_JRA' ,&
               'CLMNCEP','CPLHIST','ERA5','GEFS','SIMPLE')
             call dshr_restart_read(restfilm, rpfile, logunit, my_task, mpicom, sdat, rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
          case default
             call shr_log_error(subName//'datamode '//trim(datamode)//' not recognized', rc=rc)
             return
          end select
       end if

       first_call = .false.
    end if if_first_call

    !--------------------
    ! Advance datm streams
    !--------------------

    ! Set data needed for cosz t-interp method
    call shr_strdata_setOrbs(sdat, orbEccen, orbMvelpp, orbLambm0, orbObliqr, idt)

    ! Time and spatially interpolate to model time and grid
    call ESMF_TraceRegionEnter('datm_strdata_advance')
    call shr_strdata_advance(sdat, target_ymd, target_tod, logunit, 'datm', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TraceRegionExit('datm_strdata_advance')

    ! Update export state for non data-mode specific fields
    if (flds_co2) then
       call datm_pres_co2_advance()
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if
    if (flds_preso3) then
       call datm_pres_o3_advance()
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if
    if (flds_presndep) then
       call datm_pres_ndep_advance()
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if
    if (flds_presaero) then
       call datm_pres_aero_advance()
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    ! Determine data-mode specific behavior
    call ESMF_TraceRegionEnter('datm_datamode')
    select case (trim(datamode))
    case('CORE2_NYF','CORE2_IAF')
       call datm_datamode_core2_advance(datamode, target_ymd, target_tod, target_mon, &
            sdat%model_calendar, factorfn_mesh, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case('CORE_IAF_JRA','CORE_RYF6162_JRA','CORE_RYF8485_JRA','CORE_RYF9091_JRA','CORE_RYF0304_JRA')
       call datm_datamode_jra_advance(exportstate, target_ymd, target_tod, sdat%model_calendar, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case('CLMNCEP')
       call datm_datamode_clmncep_advance(mainproc, logunit, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case('CPLHIST')
       call datm_datamode_cplhist_advance(rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case('ERA5')
       call datm_datamode_era5_advance(exportstate, mainproc, logunit, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case('GEFS')
       call datm_datamode_gefs_advance(exportstate, sdat, mainproc, logunit, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case('SIMPLE')
       call datm_datamode_simple_advance(target_ymd, target_tod, target_mon, sdat%model_calendar, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end select

    ! Write restarts if needed
    if (restart_write) then
       call shr_get_rpointer_name(gcomp, 'atm', target_ymd, target_tod, rpfile, 'write', rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       select case (trim(datamode))
       case('CORE2_NYF','CORE2_IAF','CORE_IAF_JRA',&
            'CORE_RYF6162_JRA','CORE_RYF8485_JRA' ,&
            'CORE_RYF9091_JRA','CORE_RYF0304_JRA' ,&
            'CLMNCEP','CPLHIST','ERA5','GEFS','SIMPLE')
          call dshr_restart_write(rpfile, case_name, 'datm', inst_suffix, &
               target_ymd, target_tod, logunit, my_task, sdat, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       case default
          call shr_log_error(subName//'datamode '//trim(datamode)//' not recognized', rc=rc)
          return
       end select
    end if

    ! Log output for model date
    if (mainproc) write(logunit,*) 'atm : model date ', target_ymd, target_tod

    ! Diagnostics
    if (diagnose_data) then
       call dshr_state_diagnose(exportState, flds_scalar_name, subname//':ES', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    call ESMF_TraceRegionExit('datm_datamode')
    call ESMF_TraceRegionExit('DATM_RUN')

  end subroutine datm_comp_run

  !===============================================================================
  real(R8) function getNextRadCDay( julday, tod, stepno, dtime, iradsw )

    ! Return the calendar day of the next radiation time-step.
    ! General Usage: nextswday = getNextRadCDay(curr_date). iradsw is
    ! the frequency to update the next shortwave in number of steps
    ! (or hours if negative).
    ! -- values greater than 1 set the next radiation to the present time plus either 1 or
    !    2 timesteps (depending on the value of nextsw_cday_calc_cam7) every iradsw timesteps.
    ! -- values less than 0 set the next radiation to the present time plus either 1 or 2
    !    timesteps (depending on the value of nextsw_cday_calc_cam7) every -iradsw hours.
    ! -- if iradsw is either 0 or 1, the next radiation time is the present time plus 1 timestep.

    ! input/output variables
    real(r8)    , intent(in) :: julday
    integer     , intent(in) :: tod
    integer(i8) , intent(in) :: stepno
    integer     , intent(in) :: dtime
    integer     , intent(in) :: iradsw

    ! local variables
    real(R8) :: nextsw_cday
    integer  :: liradsw
    integer  :: delta_radsw
    character(len=*),parameter :: subName =  '(getNextRadCDay) '
    !-------------------------------------------------------------------------------

    ! Note that stepno is obtained via the advancecount argument to
    ! ESMF_GetClock and is the number of times the ESMF_Clock has been
    ! advanced. The ESMF clock is actually advanced an additional time
    ! (in order to trigger alarms) in the routine dshr_set_runclock
    ! which is the specialized routine for the model_lable_SetRunClock.
    ! This is called each time the ModelAdvance phase is called. Hence
    ! stepno is not used to trigger the calculation of nextsw_cday.

    liradsw = iradsw
    if (liradsw < 0) liradsw  = nint((-liradsw *3600._r8)/dtime)

    if (liradsw > 1) then
       delta_radsw = liradsw * dtime
       if (nextsw_cday_calc_cam7) then
          ! The logic in this block is consistent with the driver ordering in CAM7 and
          ! later. So this is appropriate when using cplhist forcings generated from CAM7
          ! or later.
          if (mod(tod,delta_radsw) == 0 .and. stepno > 0) then
             nextsw_cday = julday + 1*dtime/shr_const_cday
          elseif (stepno == 0) then
             nextsw_cday = julday + (1+liradsw)*dtime/shr_const_cday
          else
             nextsw_cday = -1._r8
          end if
       else
          if (mod(tod+dtime,delta_radsw) == 0 .and. stepno > 0) then
             nextsw_cday = julday + 2*dtime/shr_const_cday
          else
             nextsw_cday = -1._r8
          end if
       end if
    else
       nextsw_cday = julday + dtime/shr_const_cday
    end if
    getNextRadCDay = nextsw_cday

  end function getNextRadCDay

  !===============================================================================
  subroutine ModelFinalize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (my_task == main_task) then
       write(logunit,*)
       write(logunit,*) 'datm : end of main integration loop'
       write(logunit,*)
    end if
  end subroutine ModelFinalize

#ifdef CESMCOUPLED
end module atm_comp_nuopc
#else
end module cdeps_datm_comp
#endif
