#ifdef CESMCOUPLED
module glc_comp_nuopc
#else
module cdeps_dglc_comp
#endif

  !----------------------------------------------------------------------------
  ! This is the NUOPC cap for DGLC
  !----------------------------------------------------------------------------
  use ESMF             , only : ESMF_VM, ESMF_VMBroadcast, ESMF_GridCompGet
  use ESMF             , only : ESMF_Mesh, ESMF_GridComp, ESMF_State, ESMF_Clock, ESMF_Time
  use ESMF             , only : ESMF_SUCCESS, ESMF_LogWrite, ESMF_LOGMSG_INFO, ESMF_METHOD_INITIALIZE
  use ESMF             , only : ESMF_TraceRegionEnter, ESMF_TraceRegionExit, ESMF_ClockGet
  use ESMF             , only : ESMF_TimeGet, ESMF_TimeInterval, ESMF_Field, ESMF_MAXSTR
  use ESMF             , only : ESMF_Alarm, ESMF_MethodRemove, ESMF_MethodAdd
  use ESMF             , only : ESMF_GridCompSetEntryPoint, ESMF_ClockGetAlarm, ESMF_AlarmIsRinging
  use ESMF             , only : ESMF_StateGet, operator(+), ESMF_AlarmRingerOff, ESMF_LogWrite
  use ESMF             , only : ESMF_Field, ESMF_FieldGet, ESMF_VmLogMemInfo
  use NUOPC            , only : NUOPC_CompDerive, NUOPC_CompSetEntryPoint, NUOPC_CompSpecialize
  use NUOPC            , only : NUOPC_Advertise, NUOPC_CompAttributeGet, NUOPC_CompAttributeSet
  use NUOPC_Model      , only : model_routine_SS        => SetServices
  use NUOPC_Model      , only : model_label_Advance     => label_Advance
  use NUOPC_Model      , only : model_label_SetRunClock => label_SetRunClock
  use NUOPC_Model      , only : model_label_Finalize    => label_Finalize
  use NUOPC_Model      , only : NUOPC_ModelGet, SetVM
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_sys_mod      , only : shr_sys_abort
  use shr_cal_mod      , only : shr_cal_ymd2date
  use shr_log_mod      , only : shr_log_setLogUnit
  use dshr_methods_mod , only : dshr_state_diagnose, chkerr, memcheck
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_advance, shr_strdata_init_from_config
  use dshr_mod         , only : dshr_model_initphase, dshr_init, dshr_mesh_init
  use dshr_mod         , only : dshr_state_setscalar, dshr_set_runclock, dshr_check_restart_alarm
  use dshr_dfield_mod  , only : dfield_type, dshr_dfield_add, dshr_dfield_copy
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_realize

  ! Datamode specialized modules
  use dglc_datamode_noevolve_mod    , only : dglc_datamode_noevolve_advertise
  use dglc_datamode_noevolve_mod    , only : dglc_datamode_noevolve_init_pointers
  use dglc_datamode_noevolve_mod    , only : dglc_datamode_noevolve_advance
  use dglc_datamode_noevolve_mod    , only : dglc_datamode_noevolve_restart_read
  use dglc_datamode_noevolve_mod    , only : dglc_datamode_noevolve_restart_write

  implicit none
  private ! except

  public  :: SetServices
  public  :: SetVM
  private :: InitializeAdvertise
  private :: InitializeRealize
  private :: ModelAdvance
  private :: dglc_comp_run
  private :: ModelFinalize

  !--------------------------------------------------------------------------
  ! Private module data
  !--------------------------------------------------------------------------

  ! module variables for multiple ice sheets
  type(ESMF_State)       , allocatable :: NStateImp(:)
  type(ESMF_State)       , allocatable :: NStateExp(:)
  type(shr_strdata_type) , allocatable :: sdat(:)
  type(ESMF_Mesh)        , allocatable :: model_mesh(:)

  ! module variables common to all data models
  character(CS)                :: flds_scalar_name = ''
  integer                      :: flds_scalar_num = 0
  integer                      :: flds_scalar_index_nx = 0
  integer                      :: flds_scalar_index_ny = 0
  integer                      :: mpicom           ! mpi communicator
  integer                      :: my_task          ! my task in mpi communicator mpicom
  logical                      :: mainproc       ! true of my_task == main_task
  character(len=16)            :: inst_suffix = "" ! char string associated with instance (ie. "_0001" or "")
  integer                      :: logunit          ! logging unit number
  logical                      :: restart_read     ! start from restart
  character(CL)                :: case_name
  character(*) , parameter     :: nullstr = 'null'

  ! dglc_in namelist input
  integer         ,parameter :: max_icesheets = 10
  integer                    :: num_icesheets = 0
  character(CL)              :: streamfilename = nullstr                ! filename to obtain stream info from
  character(CL)              :: nlfilename = nullstr                    ! filename to obtain namelist info from
  character(CL)              :: datamode = nullstr                      ! flags physics options wrt input data
  character(CS)              :: icesheet_names(max_icesheets)
  integer                    :: nx_global(max_ice_sheets) = -999
  integer                    :: ny_global(max_ice_sheets) = -999
  character(CL)              :: model_meshfile(max_icesheets) = nullstr ! full pathname to model meshfile
  character(CL)              :: restfilm = nullstr                      ! model restart file namelist
  logical                    :: skip_restart_read = .false.             ! true => skip restart read in continuation run
  logical                    :: export_all = .false.                    ! true => export all fields, do not check connected or not

  ! linked lists
  type(fldList_type) , pointer :: fldsImport => null()
  type(fldList_type) , pointer :: fldsExport => null()

  type dfields_icesheets_type
     type(dfield_type), pointer :: dfields => null()
  end type dfields_icesheets_type
  type(dfields_icesheets_type), allocatable :: dfields_icesheets(:)

  ! constants
  logical                      :: diagnose_data = .true.
  integer      , parameter     :: main_task = 0                 ! task number of main task
#ifdef CESMCOUPLED
  character(*) , parameter     :: module_name = "(glc_comp_nuopc)"
#else
  character(*) , parameter     :: module_name = "(cdeps_dglc_comp)"
#endif
  character(*) , parameter     :: modelname = 'dglc'
  character(*) , parameter     :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Local varaibles
    character(len=*),parameter  :: subname=trim(module_name)//':(SetServices) '
    !--------------------------------

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
    integer           :: inst_index         ! number of current instance (ie. 1)
    integer           :: nu                 ! unit number
    integer           :: ierr               ! error code
    integer           :: bcasttmp(1)
    type(ESMF_VM)     :: vm
    integer           :: ns, n
    character(len=CS) :: cnum
    character(len=*),parameter :: subname=trim(module_name)//':(InitializeAdvertise) '
    !-------------------------------------------------------------------------------

    !icesheet_names = "ais", "gris"
    !num_icesheets = 2

    namelist / dglc_nml / datamode, &
         model_meshfile, ice_sheet_names, nx_global, ny_global,
         restfilm, skip_restart_read

    rc = ESMF_SUCCESS

    call NUOPC_CompAttributeGet(gcomp, name='case_name', value=case_name, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Determine logical mainproc
    mainproc = (my_task == main_task)

    ! Obtain flds_scalar values, mpi values, multi-instance values and
    ! set logunit and set shr logging to my log file
    call dshr_init(gcomp, 'GLC', mpicom, my_task, inst_index, inst_suffix, &
         flds_scalar_name, flds_scalar_num, flds_scalar_index_nx, flds_scalar_index_ny, logunit, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Read dglc_nml from nlfilename
    if (my_task == main_task) then
       nlfilename = "dglc_in"//trim(inst_suffix)
       open (newunit=nu,file=trim(nlfilename),status="old",action="read")
       call shr_nl_find_group_name(nu, 'dglc_nml', status=ierr)
       read (nu,nml=dglc_nml,iostat=ierr)
       close(nu)
       if (ierr > 0) then
          write(logunit,'(a,i8)') 'ERROR: reading input namelist, '//trim(nlfilename)//' iostat=',ierr
          call shr_sys_abort(subName//': namelist read error '//trim(nlfilename))
       end if

       ! write namelist input to standard out
       write(logunit,'(a,a)')' case_name         = ',trim(case_name)
       write(logunit,'(a,a)')' datamode          = ',trim(datamode)
       do ns = 1,max_ice_sheets
          if (model_meshfile(n) == nullstr) exit
          num_icesheets = num_icesheets + 1
          write(logunit,'(a,i4 )')' ice_sheet index = ',ns
          write(logunit,'(a,a  )')'   model_meshfile    = ',trim(model_meshfile(ns))
          write(logunit,'(a,i10)')'   nx_global         = ',nx_global(ns)
          write(logunit,'(a,i10)')'   ny_global         = ',ny_global(ns)
       end do
       write(logunit,'(a,a )')' restfilm          = ',trim(restfilm)
       write(logunit,'(a,l6)')' skip_restart_read = ',skip_restart_read
       if (skip_restart_read) then
          bcasttmp = 1
       else
          bcasttmp = 0
       end if
    endif

    ! Broadcast namelist input
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, datamode, CL, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, restfilm, CL, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    do ns = 1,num_icesheets
       call ESMF_VMBroadcast(vm, model_meshfile(n), CL, main_task, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_VMBroadcast(vm, nx_global(ns), 1, main_task, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_VMBroadcast(vm, ny_global(ns), 1, main_task, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end do
    call ESMF_VMBroadcast(vm, bcasttmp, 1, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Validate datamode
    if ( trim(datamode) == 'noevolve')  ! read stream, no import data
    else
       call shr_sys_abort(' ERROR illegal dglc datamode = '//trim(datamode))
    endif

    ! Allocate module variables
    allocate(NStateImp(num_icesheets))
    allocate(NStateExp(num_icesheets))
    allocate(sdat(num_icesheets))
    allocate(model_mesh(num_icesheets))
    allocate(dfields_icesheets(num_icesheets))

    ! Create nested states
    do ns = 1,num_icesheets
       write(cnum,'(i0)') ns
       call NUOPC_AddNestedState(importState, CplSet="GLC"//trim(cnum), nestedState=NStateImp(ns), rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call NUOPC_AddNestedState(exportState, CplSet="GLC"//trim(cnum), nestedState=NStateExp(ns), rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end do

    ! Advertise dglc fields
    if (trim(datamode)=='noevolve') then
       call dglc_datamode_noevolve_advertise(exportState, fldsExport, flds_scalar_name, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

  end subroutine InitializeAdvertise

  !===============================================================================
  subroutine InitializeRealize(gcomp, clock, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Time)        :: currTime
    integer                :: current_ymd  ! model date
    integer                :: current_year ! model year
    integer                :: current_mon  ! model month
    integer                :: current_day  ! model day
    integer                :: current_tod  ! model sec into model date
    type(ESMF_Field)       :: lfield
    character(CL) ,pointer :: lfieldnamelist(:) => null()
    integer                :: fieldcount
    real(r8), pointer      :: fldptr(:)
    integer                :: n
    integer                :: imask
    real(r8)               :: rmask
    character(CL)          :: cvalue
    character(len=*), parameter :: subname=trim(module_name)//':(InitializeRealize) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_VMLogMemInfo("Entering "//trim(subname))

    ! Initialize model mesh, restart flag, logunit, model_mask and model_frac
    call ESMF_TraceRegionEnter('dglc_strdata_init')

    ! Determine stream filename
    streamfilename = trim(modelname)//'.streams'//trim(inst_suffix)
#ifndef DISABLE_FoX
    streamfilename = trim(streamfilename)//'.xml'
#endif

    ! generate local mpi comm
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMGet(vm, localPet=my_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    mainproc = (my_task == main_task)
    call shr_log_setLogUnit(logunit)

    ! Set restart flag
    call NUOPC_CompAttributeGet(gcomp, name='read_restart', value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       read(cvalue,*) read_restart
    else
       call shr_sys_abort(subname//' ERROR: read restart flag must be present')
    end if

    ! Loop over ice sheets
    do ns = 1,num_icesheets
       write(cns,'(i0)') ns

       ! Initialize pio subsystem
#ifdef CESMCOUPLED
       sdat(ns)%pio_subsystem => shr_pio_getiosys('GLC')
       sdat(ns)%io_type       =  shr_pio_getiotype('GLC')
       sdat(ns)%io_format     =  shr_pio_getioformat('GLC')
#else
       call dshr_pio_init(gcomp, sdat(ns), logunit, rc)
#endif

       ! Check that model_meshfile exists
       if (my_task == main_task) then
          inquire(file=trim(model_meshfile(ns)), exist=exists)
          if (.not.exists) then
             write(logunit, *)' ERROR: model_meshfile '//trim(model_meshfile)//' does not exist'
             call shr_sys_abort(trim(subname)//' ERROR: model_meshfile '//trim(model_meshfile)//' does not exist')
          end if
       endif

       ! Read in model mesh
       model_mesh(ns) = ESMF_MeshCreate(trim(model_meshfile(ns)), fileformat=ESMF_FILEFORMAT_ESMFMESH, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Initialize stream data type
       call shr_strdata_init_from_config(sdat(ns), streamfilename, model_mesh(ns), clock, 'GLC', logunit, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_TraceRegionExit('dglc_strdata_init')

       ! Realize the actively coupled fields, now that a mesh is established and
       ! NUOPC_Realize "realizes" a previously advertised field in the importState and exportState
       ! by replacing the advertised fields with the newly created fields of the same name.

       call dshr_fldlist_realize( NStateExp(ns), fldsExport, flds_scalar_name, flds_scalar_num, model_mesh(ns), &
            subname//trim(modelname)//':Export', export_all, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       call dshr_fldlist_realize( NStateImp(ns), fldsImport, flds_scalar_name, flds_scalar_num, model_mesh(ns), &
            subname//trim(modelname)//':Import', .false., rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

    end do ! loop over ice sheets

    ! Get the time to interpolate the stream data to
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(currTime, yy=current_year, mm=current_mon, dd=current_day, s=current_tod, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_cal_ymd2date(current_year, current_mon, current_day, current_ymd)

    ! loop over ice sheets
    do ns = 1,num_icesheets
       ! Run dglc
       call dglc_comp_run(clock, current_ymd, current_tod, restart_write=.false., rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Add scalars to export state
       call dshr_state_SetScalar(dble(nx_global(ns)),flds_scalar_index_nx, &
            NStateExp(ns), flds_scalar_name, flds_scalar_num, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call dshr_state_SetScalar(dble(ny_global(ns)),flds_scalar_index_ny,&
            NStateExp(ns), flds_scalar_name, flds_scalar_num, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end do

    call ESMF_VMLogMemInfo("Leaving "//trim(subname))

   end subroutine InitializeRealize

  !===============================================================================
  subroutine ModelAdvance(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)        :: clock
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time)         :: currTime, nextTime
    integer                 :: next_ymd      ! model date
    integer                 :: next_tod      ! model sec into model date
    integer                 :: yr            ! year
    integer                 :: mon           ! month
    integer                 :: day           ! day in month
    logical                 :: restart_write
    character(len=*),parameter :: subname=trim(module_name)//':(ModelAdvance) '
    !-------------------------------------------------------------------------------


    rc = ESMF_SUCCESS
    call shr_log_setLogUnit(logunit)

    call memcheck(subname, 5, my_task == main_task)

    ! query the Component for its clock
    call NUOPC_ModelGet(gcomp, modelClock=clock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! For nuopc - the component clock is advanced at the end of the time interval
    ! Need to advance nuopc one timestep ahead for shr_strdata time interpolation
    call ESMF_ClockGet( clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    nextTime = currTime + timeStep
    call ESMF_TimeGet( nextTime, yy=yr, mm=mon, dd=day, s=next_tod, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_cal_ymd2date(yr, mon, day, next_ymd)

    ! determine if will write restart
    restart_write = dshr_check_restart_alarm(clock, rc=rc)

    ! run dglc
    call dglc_comp_run(clock, next_ymd, next_tod, restart_write, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine ModelAdvance

  !===============================================================================
  subroutine dglc_comp_run(clock, target_ymd, target_tod, restart_write, rc)

    ! --------------------------
    ! advance dglc
    ! --------------------------

    ! input/output variables:
    type(ESMF_Clock) , intent(in)    :: clock
    integer          , intent(in)    :: target_ymd       ! model date
    integer          , intent(in)    :: target_tod       ! model sec into model date
    logical          , intent(in)    :: restart_write
    integer          , intent(out)   :: rc

    ! local variables
    character(len=CS) :: cnum
    integer           :: ns ! ice sheet index
    logical           :: first_time = .true.
    character(*), parameter :: subName = "(dglc_comp_run) "
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_TraceRegionEnter('DGLC_RUN')

    ! Loop over ice sheets
    do ns = 1,size(NStateImp)

       !--------------------
       ! First time initialization
       !--------------------

       if (first_time) then
          ! Initialize dfields for given ice sheet
          call dglc_init_dfields(NStateImp(ns), NStateExp(ns), rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return

          ! Initialize datamode module ponters
          select case (trim(datamode))
          case('noevolve')
             call dglc_datamode_copyall_init_pointers(NStateExp(ns), rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
          end select

          ! Read restart if needed
          ! if (restart_read .and. .not. skip_restart_read) then
          !    select case (trim(datamode))
          !    case('noevolve')
          !       call dglc_datamode_copyall_restart_read(restfilm, inst_suffix, logunit, my_task, mpicom, sdat(ns))
          !    end select
          ! end if

          ! Reset first_time
          first_time = .false.
       end if

       !--------------------
       ! Update export (and possibly import data model states)
       !--------------------

       ! Advance data model streams - time and spatially interpolate to model time and grid
       call ESMF_TraceRegionEnter('dglc_strdata_advance')
       call shr_strdata_advance(sdat(ns), target_ymd, target_tod, logunit, 'dglc', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_TraceRegionExit('dglc_strdata_advance')

       ! Copy all fields from streams to export state as default
       ! This automatically will update the fields in the export state
       call ESMF_TraceRegionEnter('dglc_dfield_copy')
       call dshr_dfield_copy(dfields_icesheets(ns)%dfields, sdat(ns), rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_TraceRegionExit('dglc_dfield_copy')

       ! Perform data mode specific calculations
       select case (trim(datamode))
       case('noevolve')
          call  dglc_datamode_noevolve_advance(rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       end select

       ! Write restarts if needed
       if (restart_write) then
          select case (trim(datamode))
          case('noevolve')
             call dglc_datamode_noevolve_restart_write(case_name, inst_suffix, target_ymd, target_tod, &
                  logunit, my_task, sdat(ns))
          end select
       end if

       ! Write diagnostics
       if (diagnose_data) then
          write(cnum,'(i0)') ns
          call dshr_state_diagnose(NStateExp, flds_scalar_name, trim(subname)//':ES_'//trim(cnum), rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       end if

    end do ! end of loop over ice sheets

    call ESMF_TraceRegionExit('DGLC_RUN')

  contains

    subroutine dglc_init_dfields(ns, rc)
      ! -----------------------------
      ! Initialize dfields arrays
      ! -----------------------------

      ! input/output variables
      integer          , intent(in)    :: ns          ! ice sheet index
      integer          , intent(out)   :: rc

      ! local variables
      integer                         :: n
      integer                         :: fieldcount
      type(ESMF_Field)                :: lfield
      character(ESMF_MAXSTR) ,pointer :: lfieldnamelist(:)
      character(*), parameter   :: subName = "(dglc_init_dfields) "
      !-------------------------------------------------------------------------------

      rc = ESMF_SUCCESS

      ! Initialize dfields data type (to map streams to export state fields)
      ! Create dfields linked list - used for copying stream fields to export state fields
      call ESMF_StateGet(NStateExp(ns), itemCount=fieldCount, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      allocate(lfieldnamelist(fieldCount))
      call ESMF_StateGet(NStateExp(ns), itemNameList=lfieldnamelist, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      do n = 1, fieldCount
         call ESMF_StateGet(NStateExp(ns), itemName=trim(lfieldNameList(n)), field=lfield, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
         if (trim(lfieldnamelist(n)) /= flds_scalar_name) then
            call dshr_dfield_add( dfields, sdat(ns), trim(lfieldnamelist(n)), trim(lfieldnamelist(n)), NStateExp(ns), &
                 logunit, mainproc, rc)
            if (chkerr(rc,__LINE__,u_FILE_u)) return
         end if
      end do
    end subroutine dglc_init_dfields

  end subroutine dglc_comp_run

  !===============================================================================
  subroutine ModelFinalize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    !-------------------------------------------------------------------------------
    rc = ESMF_SUCCESS
    if (my_task == main_task) then
       write(logunit,*)
       write(logunit,*) 'dglc : end of main integration loop'
       write(logunit,*)
    end if
  end subroutine ModelFinalize

#ifdef CESMCOUPLED
end module glc_comp_nuopc
#else
end module cdeps_dglc_comp
#endif
