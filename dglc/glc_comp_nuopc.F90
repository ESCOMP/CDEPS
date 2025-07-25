#ifdef CESMCOUPLED
module glc_comp_nuopc
#else
module cdeps_dglc_comp
#endif

  !----------------------------------------------------------------------------
  ! This is the NUOPC cap for DGLC
  !----------------------------------------------------------------------------
  use ESMF             , only : ESMF_VM, ESMF_VmGet, ESMF_VMBroadcast, ESMF_GridCompGet
  use ESMF             , only : ESMF_Mesh, ESMF_GridComp, ESMF_State, ESMF_Clock, ESMF_Time
  use ESMF             , only : ESMF_SUCCESS, ESMF_FAILURE, ESMF_LogWrite, ESMF_LOGMSG_INFO, ESMF_METHOD_INITIALIZE
  use ESMF             , only : ESMF_TraceRegionEnter, ESMF_TraceRegionExit
  use ESMF             , only : ESMF_TimeGet, ESMF_TimeInterval, ESMF_TimeIntervalGet, ESMF_Field, ESMF_MAXSTR
  use ESMF             , only : ESMF_MethodRemove, ESMF_MethodAdd
  use ESMF             , only : ESMF_GridCompSetEntryPoint
  use ESMF             , only : ESMF_Alarm, ESMF_AlarmSet, ESMF_AlarmIsRinging, ESMF_ALARMLIST_ALL
  use ESMF             , only : ESMF_ClockGet, ESMF_ClockSet, ESMF_ClockAdvance, ESMF_ClockGetAlarm, ESMF_ClockGetAlarmList
  use ESMF             , only : ESMF_StateGet, operator(+), ESMF_AlarmRingerOff, ESMF_LogWrite
  use ESMF             , only : ESMF_Field, ESMF_FieldGet, ESMF_VmLogMemInfo
  use ESMF             , only : ESMF_MeshCreate, ESMF_FILEFORMAT_ESMFMESH
  use NUOPC            , only : NUOPC_CompDerive, NUOPC_CompSetEntryPoint, NUOPC_CompSpecialize
  use NUOPC            , only : NUOPC_Advertise, NUOPC_CompAttributeGet, NUOPC_CompAttributeSet
  use NUOPC            , only : NUOPC_AddNestedState, NUOPC_IsConnected
  use NUOPC_Model      , only : model_routine_SS        => SetServices
  use NUOPC_Model      , only : model_label_Advance     => label_Advance
  use NUOPC_Model      , only : model_label_SetRunClock => label_SetRunClock
  use NUOPC_Model      , only : model_label_Finalize    => label_Finalize
  use NUOPC_Model      , only : NUOPC_ModelGet, SetVM
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_kind_mod     , only : cx=>shr_kind_cx
  use shr_cal_mod      , only : shr_cal_ymd2date
  use shr_log_mod      , only : shr_log_setLogUnit, shr_log_error
  use shr_string_mod   , only : shr_string_listGetNum, shr_string_listGetName
#ifdef CESMCOUPLED
  use shr_pio_mod      , only : shr_pio_getiosys, shr_pio_getiotype, shr_pio_getioformat
#endif
  use dshr_methods_mod , only : dshr_state_diagnose, chkerr, memcheck
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_advance, shr_strdata_init_from_config
  use dshr_mod         , only : dshr_model_initphase, dshr_init, dshr_mesh_init
  use dshr_mod         , only : dshr_state_setscalar, dshr_set_runclock, dshr_check_restart_alarm
  use dshr_dfield_mod  , only : dfield_type, dshr_dfield_add, dshr_dfield_copy
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_realize
  use nuopc_shr_methods, only : shr_get_rpointer_name, alarmInit
  ! Datamode specialized modules
  use dglc_datamode_noevolve_mod, only : dglc_datamode_noevolve_advertise
  use dglc_datamode_noevolve_mod, only : dglc_datamode_noevolve_init_pointers
  use dglc_datamode_noevolve_mod, only : dglc_datamode_noevolve_advance
  use dglc_datamode_noevolve_mod, only : dglc_datamode_noevolve_restart_read
  use dglc_datamode_noevolve_mod, only : dglc_datamode_noevolve_restart_write

  implicit none
  private ! except

  public  :: SetServices
  public  :: SetVM
  private :: InitializeAdvertise
  private :: InitializeRealize
  private :: ModelSetRunClock
  private :: ModelAdvance
  private :: dglc_comp_run
  private :: ModelFinalize

  !--------------------------------------------------------------------------
  ! Private module data
  !--------------------------------------------------------------------------

  character(*) , parameter :: nullstr = 'null'
  integer      , parameter :: max_icesheets = 10 ! maximum number of ice sheets for namelist input
  integer                  :: num_icesheets      ! actual number of ice sheets

  ! namelist input
  character(CL) :: model_meshfiles(max_icesheets) = nullstr
  character(CL) :: model_datafiles(max_icesheets) = nullstr
  integer       :: nx_global(max_icesheets) = 0
  integer       :: ny_global(max_icesheets) = 0
  real(r8)      :: model_internal_gridsize(max_icesheets) = 1.e36

  ! module variables for multiple ice sheets
  type(shr_strdata_type) , allocatable :: sdat(:)
  type(ESMF_State)       , allocatable :: NStateImp(:)
  type(ESMF_State)       , allocatable :: NStateExp(:)
  type(ESMF_Mesh)        , allocatable :: model_meshes(:)

  ! module variables common to all data models
  character(CS)     :: flds_scalar_name = ''
  integer           :: flds_scalar_num = 0
  integer           :: flds_scalar_index_nx = 0
  integer           :: flds_scalar_index_ny = 0
  integer           :: mpicom           ! mpi communicator
  integer           :: my_task          ! my task in mpi communicator mpicom
  logical           :: mainproc         ! true of my_task == main_task
  character(len=16) :: inst_suffix = "" ! char string associated with instance (ie. "_0001" or "")
  integer           :: logunit          ! logging unit number
  logical           :: restart_read     ! start from restart
  character(CL)     :: case_name

  ! dglc_in namelist input
  character(CX) :: streamfilename = nullstr    ! filename to obtain stream info from
  character(CX) :: nlfilename = nullstr        ! filename to obtain namelist info from
  character(CL) :: datamode = nullstr          ! flags physics options wrt input data
  character(CX) :: restfilm = nullstr          ! model restart file namelist
  logical       :: skip_restart_read = .false. ! true => skip restart read in continuation run
  logical       :: export_all = .false.        ! true => export all fields, do not check connected or not

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
    ! The following specialization does not use dshr_set_runclock since we also need to add an alarm
    ! to determine if the model inputs are valid
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_SetRunClock, specRoutine=ModelSetRunClock, rc=rc)
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
    type(ESMF_VM)     :: vm
    integer           :: inst_index         ! number of current instance (ie. 1)
    integer           :: nu                 ! unit number
    integer           :: ierr               ! error code
    integer           :: bcasttmp(2)
    integer           :: ns
    character(len=CS) :: cnum
    character(len=ESMF_MAXSTR) :: model_datafiles_list ! colon separated string containing input datafiles
    character(len=ESMF_MAXSTR) :: model_meshfiles_list ! colon separated string containing model meshfiles
    character(len=*),parameter :: subname=trim(module_name)//':(InitializeAdvertise) '
    !-------------------------------------------------------------------------------

    ! Note that the suffix '-list' refers to a colon delimited string of names
    namelist / dglc_nml / datamode, &
         model_meshfiles_list, model_datafiles_list, model_internal_gridsize, nx_global, ny_global, &
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
         call shr_log_error(subName//': namelist read error '//trim(nlfilename), rc=rc)
         return
      end if

      ! Determine number of ice sheets
      num_icesheets = shr_string_listGetNum(model_meshfiles_list)

      ! Determine array of model meshfile(s) and model input datafile(s)
      do ns = 1,num_icesheets
        ! determine mesh filename(s)
        call shr_string_listGetName(model_meshfiles_list, ns, model_meshfiles(ns))
        ! determine input datafile name(s)
        call shr_string_listGetName(model_datafiles_list, ns, model_datafiles(ns))
      end do

      ! Write diagnostics
      write(logunit,'(a,a)')' case_name         = ',trim(case_name)
      write(logunit,'(a,a)')' datamode          = ',trim(datamode)
      do ns = 1,num_icesheets
        write(logunit,'(a,i4 )')  ' ice_sheet index           = ',ns
        write(logunit,'(a,a  )')  '   model_meshfile          = ',trim(model_meshfiles(ns))
        write(logunit,'(a,a  )')  '   model_datafile          = ',trim(model_datafiles(ns))
        write(logunit,'(a,d13.5)')'   model_internal_gridsize = ',model_internal_gridsize(ns)
        write(logunit,'(a,i10)')  '   nx_global               = ',nx_global(ns)
        write(logunit,'(a,i10)')  '   ny_global               = ',ny_global(ns)
      end do
      write(logunit,'(a,a )')' restfilm          = ',trim(restfilm)
      write(logunit,'(a,l6)')' skip_restart_read = ',skip_restart_read
      bcasttmp(1) = 0
      if(skip_restart_read) bcasttmp(1) = 1
      bcasttmp(2) = num_icesheets
    endif

    ! Broadcast namelist input
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, datamode, CL, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, restfilm, CX, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, model_meshfiles, CL*max_icesheets, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, model_datafiles, CL*max_icesheets, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, model_internal_gridsize, max_icesheets, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, nx_global, max_icesheets, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, ny_global, max_icesheets, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, bcasttmp, 3, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    skip_restart_read = (bcasttmp(1) == 1)
    num_icesheets = bcasttmp(2)

    ! Validate datamode
    if ( trim(datamode) == 'noevolve') then  ! read stream, no import data
      ! do nothing
    else
       call shr_log_error(' ERROR illegal dglc datamode = '//trim(datamode), rc=rc)
       return
    endif

    ! Allocate module variables
    allocate(sdat(num_icesheets))
    allocate(NStateImp(num_icesheets))
    allocate(NStateExp(num_icesheets))
    allocate(model_meshes(num_icesheets))

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
       call dglc_datamode_noevolve_advertise(NStateExp, fldsexport, NStateImp, fldsimport, &
            flds_scalar_name, rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

  end subroutine InitializeAdvertise

  !===============================================================================
  subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_VM)   :: vm
    type(ESMF_Time) :: currTime
    integer         :: current_ymd  ! model date
    integer         :: current_year ! model year
    integer         :: current_mon  ! model month
    integer         :: current_day  ! model day
    integer         :: current_tod  ! model sec into model date
    integer         :: ns
    character(CL)   :: cvalue
    character(CS)   :: cns
    logical         :: ispresent, isset
    logical         :: exists
    character(len=*), parameter :: subname=trim(module_name)//':(InitializeRealize) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Initialize model mesh, restart flag, logunit, model_mask and model_frac
    call ESMF_VMLogMemInfo("Entering "//trim(subname))
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

    ! Set restart flag (sets module variable restart_read)
    call NUOPC_CompAttributeGet(gcomp, name='read_restart', value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       read(cvalue,*) restart_read
    else
       call shr_log_error(subname//' ERROR: read restart flag must be present', rc=rc)
       return
    end if

    ! Get the time to interpolate the stream data to
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(currTime, yy=current_year, mm=current_mon, dd=current_day, s=current_tod, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_cal_ymd2date(current_year, current_mon, current_day, current_ymd)

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
          inquire(file=trim(model_meshfiles(ns)), exist=exists)
          if (.not.exists) then
             call shr_log_error(trim(subname)//' ERROR: model_meshfile '//trim(model_meshfiles(ns))//' does not exist', rc=rc)
             return
          end if
       endif

       ! Read in model mesh for given ice sheet
       model_meshes(ns) = ESMF_MeshCreate(trim(model_meshfiles(ns)), fileformat=ESMF_FILEFORMAT_ESMFMESH, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Initialize stream data type
       if (trim(datamode) /= 'noevolve') then
          call shr_strdata_init_from_config(sdat(ns), streamfilename, model_meshes(ns), clock, 'GLC', logunit, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       end if

       ! Realize the actively coupled fields, now that a mesh is established and
       ! NUOPC_Realize "realizes" a previously advertised field in the importState and exportState
       ! by replacing the advertised fields with the newly created fields of the same name.

       call ESMF_LogWrite(subname//' calling dshr_fldlist_realize export for ice sheet '//trim(cns), ESMF_LOGMSG_INFO)
       call dshr_fldlist_realize( NStateExp(ns), fldsExport, flds_scalar_name, flds_scalar_num, model_meshes(ns), &
            subname//trim(modelname)//':Export', export_all, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       call ESMF_LogWrite(subname//' calling dshr_fldlist_realize importfor ice sheet '//trim(cns), ESMF_LOGMSG_INFO)
       call dshr_fldlist_realize( NStateImp(ns), fldsImport, flds_scalar_name, flds_scalar_num, model_meshes(ns), &
            subname//trim(modelname)//':Import', .false., rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Add scalars to export state
       call dshr_state_SetScalar(dble(nx_global(ns)),flds_scalar_index_nx, &
            NStateExp(ns), flds_scalar_name, flds_scalar_num, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call dshr_state_SetScalar(dble(ny_global(ns)),flds_scalar_index_ny,&
            NStateExp(ns), flds_scalar_name, flds_scalar_num, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

    end do ! end loop over ice sheets

    ! Run dglc
    call dglc_comp_run(gcomp, clock, current_ymd, current_tod, restart_write=.false., valid_inputs=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TraceRegionExit('dglc_strdata_init')
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
    type(ESMF_Alarm)        :: valid_alarm
    logical                 :: valid_inputs ! if true, inputs from mediator are valid
    character(len=CS)       :: timestring
    character(len=*),parameter :: subname=trim(module_name)//':(ModelAdvance) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call shr_log_setLogUnit(logunit)

    call ESMF_TraceRegionEnter(subname)
    call memcheck(subname, 5, my_task == main_task)

    ! query the Component for its clock
    call NUOPC_ModelGet(gcomp, modelClock=clock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Determine if inputs from mediator are valid
    call ESMF_ClockGetAlarm(clock, alarmname='alarm_valid_inputs', alarm=valid_alarm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (ESMF_AlarmIsRinging(valid_alarm, rc=rc)) then
       valid_inputs = .true.
       call ESMF_AlarmRingerOff( valid_alarm, rc=rc )
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    else
       valid_inputs = .false.
    endif

    ! Need to advance nuopc one timestep ahead for shr_strdata time interpolation
    call ESMF_ClockGet( clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    nextTime = currTime + timeStep
    call ESMF_TimeGet( nextTime, yy=yr, mm=mon, dd=day, s=next_tod, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_cal_ymd2date(yr, mon, day, next_ymd)
    if (my_task == main_task) then
       call ESMF_TimeGet(currTime, timestring=timestring, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       write(logunit,'(a,l6)') trim(timestring)//': valid_input for dglc is ',valid_inputs
    end if

    ! determine if will write restart
    restart_write = dshr_check_restart_alarm(clock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (my_task == main_task) then
       write(logunit,'(a,l6)') trim(timestring)//': restart write is ',restart_write
    end if

    ! run dglc
    call dglc_comp_run(gcomp, clock, next_ymd, next_tod, restart_write, valid_inputs, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TraceRegionExit(subname)

  end subroutine ModelAdvance

  !===============================================================================
  subroutine dglc_comp_run(gcomp, clock, target_ymd, target_tod, restart_write, valid_inputs, rc)

    ! --------------------------
    ! advance dglc
    ! --------------------------

    ! input/output variables:
    type(ESMF_GridComp) ,intent(in)  :: gcomp
    type(ESMF_Clock) , intent(in)    :: clock
    integer          , intent(in)    :: target_ymd       ! model date
    integer          , intent(in)    :: target_tod       ! model sec into model date
    logical          , intent(in)    :: restart_write
    logical          , intent(in)    :: valid_inputs
    integer          , intent(out)   :: rc

    ! local variables
    character(len=CS) :: cnum
    integer           :: ns ! ice sheet index
    logical           :: first_time = .true.
    character(len=CS) :: rpfile
    character(*), parameter :: subName = "(dglc_comp_run) "
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_TraceRegionEnter('DGLC_RUN')

    !--------------------
    ! First time initialization
    !--------------------

    if (first_time) then
      ! Initialize dfields for all ice sheets
      if (trim(datamode) /= 'noevolve') then
        call dglc_init_dfields(rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      end if

      ! Initialize datamode module ponters
      select case (trim(datamode))
      case('noevolve')
        call dglc_datamode_noevolve_init_pointers(NStateExp, NStateImp, rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      end select

      ! Read restart if needed
      if (restart_read .and. .not. skip_restart_read) then
         call shr_get_rpointer_name(gcomp, 'glc', target_ymd, target_tod, rpfile, 'read', rc)
         if (ChkErr(rc,__LINE__,u_FILE_u)) return
         call dglc_datamode_noevolve_restart_read(model_meshes, restfilm, rpfile, &
              logunit, my_task, main_task, mpicom, &
              sdat(1)%pio_subsystem, sdat(1)%io_type, nx_global, ny_global, rc)
         if (ChkErr(rc,__LINE__,u_FILE_u)) return
      end if

      ! Reset first_time
      first_time = .false.
    end if

    !--------------------
    ! Update export (and possibly import data model states)
    !--------------------

    if (trim(datamode) /= 'noevolve') then
      if (.not. allocated(dfields_icesheets)) then
        allocate(dfields_icesheets(num_icesheets))
      end if

      ! Loop over ice sheets
      do ns = 1,num_icesheets
        ! Advance data model streams - time and spatially interpolate to model time and grid
        ! Note that loop over ice sheets is done inside shr_strdata_advance
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
      end do
    end if

    ! Perform data mode specific calculations
    select case (trim(datamode))
    case('noevolve')
      if (valid_inputs) then
         call dglc_datamode_noevolve_advance(gcomp, sdat(1)%pio_subsystem, sdat(1)%io_type, sdat(1)%io_format, &
              logunit, model_meshes, model_internal_gridsize, model_datafiles, rc)
         if (ChkErr(rc,__LINE__,u_FILE_u)) return
      end if
    end select

    ! Write restarts if needed

    if (restart_write) then
      if (trim(datamode) == 'noevolve') then
         if (my_task == main_task) then
            write(logunit,'(a)') 'calling dglc_datamode_noevolve_restart_write'
         end if
         call shr_get_rpointer_name(gcomp, 'glc', target_ymd, target_tod, rpfile, 'write', rc)
         if (ChkErr(rc,__LINE__,u_FILE_u)) return
         call dglc_datamode_noevolve_restart_write(model_meshes, case_name, rpfile, &
              inst_suffix, target_ymd, target_tod, logunit, my_task, main_task, &
              sdat(1)%pio_subsystem, sdat(1)%io_type, nx_global, ny_global, rc)
         if (ChkErr(rc,__LINE__,u_FILE_u)) return
      end if
    end if

    ! Write diagnostics
    if (diagnose_data) then
      do ns = 1,num_icesheets
        write(cnum,'(i0)') ns
        call dshr_state_diagnose(NStateExp(ns), flds_scalar_name, trim(subname)//':ES_'//trim(cnum), rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      end do
    end if

    call ESMF_TraceRegionExit('DGLC_RUN')

  contains

    subroutine dglc_init_dfields(rc)
      ! -----------------------------
      ! Initialize dfields arrays
      ! -----------------------------

      ! input/output variables
      integer, intent(out) :: rc

      ! local variables
      integer                         :: nf, ns
      integer                         :: fieldcount
      type(ESMF_Field)                :: lfield
      character(ESMF_MAXSTR) ,pointer :: lfieldnamelist(:)
      character(*), parameter         :: subName = "(dglc_init_dfields) "
      !-------------------------------------------------------------------------------

      rc = ESMF_SUCCESS

      ! Loop over ice sheets
      ! Initialize dfields data type (to map streams to export state fields)
      ! Create dfields linked list - used for copying stream fields to export state fields
      do ns = 1,num_icesheets
        call ESMF_StateGet(NStateExp(ns), itemCount=fieldCount, rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return
        allocate(lfieldnamelist(fieldCount))
        call ESMF_StateGet(NStateExp(ns), itemNameList=lfieldnamelist, rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return
        do nf = 1, fieldCount
          call ESMF_StateGet(NStateExp(ns), itemName=trim(lfieldNameList(nf)), field=lfield, rc=rc)
          if (chkerr(rc,__LINE__,u_FILE_u)) return
          if (trim(lfieldnamelist(nf)) /= flds_scalar_name) then
            call dshr_dfield_add( dfields_icesheets(ns)%dfields, sdat(ns), &
                 trim(lfieldnamelist(nf)), trim(lfieldnamelist(nf)), NStateExp(ns), logunit, mainproc, rc)
            if (chkerr(rc,__LINE__,u_FILE_u)) return
          end if
        end do
        deallocate(lfieldnamelist)
      end do
    end subroutine dglc_init_dfields

  end subroutine dglc_comp_run

  !===============================================================================

  subroutine ModelSetRunClock(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)         :: mclock, dclock
    type(ESMF_Time)          :: mcurrtime, dcurrtime
    type(ESMF_Time)          :: mstoptime
    type(ESMF_TimeInterval)  :: mtimestep, dtimestep
    character(len=256)       :: cvalue
    character(len=256)       :: restart_option ! Restart option units
    integer                  :: restart_n      ! Number until restart interval
    integer                  :: restart_ymd    ! Restart date (YYYYMMDD)
    type(ESMF_ALARM)         :: restart_alarm
    character(len=256)       :: stop_option    ! Stop option units
    integer                  :: stop_n         ! Number until stop interval
    integer                  :: stop_ymd       ! Stop date (YYYYMMDD)
    type(ESMF_ALARM)         :: stop_alarm
    integer                  :: alarmcount
    character(len=CS)        :: glc_avg_period ! averaging period in mediator
    type(ESMF_ALARM)         :: valid_alarm    ! model alarm
    integer                  :: dtime
    character(len=*),parameter :: subname='glc_comp_nuopc:(dglc_set_runclock) '
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

    ! determine number of alarms
    call ESMF_ClockGetAlarmList(mclock, alarmlistflag=ESMF_ALARMLIST_ALL, alarmCount=alarmCount, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (alarmCount == 0) then

       !----------------
       ! glc valid input alarm
       !----------------
       call NUOPC_CompAttributeGet(gcomp, name="glc_avg_period", value=glc_avg_period, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       if (trim(glc_avg_period) == 'hour') then
          call alarmInit(mclock, valid_alarm, 'nhours', opt_n=1, alarmname='alarm_valid_inputs', rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       else if (trim(glc_avg_period) == 'day') then
          call alarmInit(mclock, valid_alarm, 'ndays' , opt_n=1, alarmname='alarm_valid_inputs', rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       else if (trim(glc_avg_period) == 'yearly') then
          call alarmInit(mclock, valid_alarm, 'yearly', alarmname='alarm_valid_inputs', rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       else if (trim(glc_avg_period) == 'glc_coupling_period') then
          call ESMF_TimeIntervalGet(mtimestep, s=dtime, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          call alarmInit(mclock, valid_alarm, 'nseconds', opt_n=dtime, alarmname='alarm_valid_inputs', rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       else
          call ESMF_LogWrite(trim(subname)// ": ERROR glc_avg_period = "//trim(glc_avg_period)//" not supported", &
               ESMF_LOGMSG_INFO, rc=rc)
          rc = ESMF_FAILURE
          RETURN
       end if

       call ESMF_AlarmSet(valid_alarm, clock=mclock, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       !----------------
       ! Stop alarm
       !----------------
       call ESMF_LogWrite(subname//'setting stop alarm for dglc' , ESMF_LOGMSG_INFO)
       call NUOPC_CompAttributeGet(gcomp, name="stop_option", value=stop_option, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       call NUOPC_CompAttributeGet(gcomp, name="stop_n", value=cvalue, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       read(cvalue,*) stop_n

       call NUOPC_CompAttributeGet(gcomp, name="stop_ymd", value=cvalue, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       read(cvalue,*) stop_ymd

       call alarmInit(mclock, stop_alarm, stop_option, &
            opt_n   = stop_n,           &
            opt_ymd = stop_ymd,         &
            RefTime = mcurrTime,           &
            alarmname = 'alarm_stop', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       call ESMF_AlarmSet(stop_alarm, clock=mclock, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       !----------------
       ! Restart alarm
       !----------------
       call ESMF_LogWrite(subname//'setting restart alarm for dglc' , ESMF_LOGMSG_INFO)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       call NUOPC_CompAttributeGet(gcomp, name="restart_option", value=restart_option, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       call NUOPC_CompAttributeGet(gcomp, name="restart_n", value=cvalue, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       read(cvalue,*) restart_n

       call NUOPC_CompAttributeGet(gcomp, name="restart_ymd", value=cvalue, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       read(cvalue,*) restart_ymd

       call alarmInit(mclock, restart_alarm, restart_option, &
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

  end subroutine ModelSetRunClock

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
