#ifdef CESMCOUPLED
module rof_comp_nuopc
#else
module cdeps_drof_comp
#endif


  !----------------------------------------------------------------------------
  ! This is the NUOPC cap for DROF
  !----------------------------------------------------------------------------
  use ESMF             , only : ESMF_VM, ESMF_VMBroadcast, ESMF_GridCompGet
  use ESMF             , only : ESMF_Mesh, ESMF_GridComp, ESMF_Time, ESMF_TimeInterval
  use ESMF             , only : ESMF_State, ESMF_Clock, ESMF_SUCCESS, ESMF_LOGMSG_INFO
  use ESMF             , only : ESMF_TraceRegionEnter, ESMF_TraceRegionExit
  use ESMF             , only : ESMF_Alarm, ESMF_METHOD_INITIALIZE, ESMF_MethodAdd, ESMF_MethodRemove
  use ESMF             , only : ESMF_TimeGet, ESMF_ClockGet, ESMF_GridCompSetEntryPoint
  use ESMF             , only : ESMF_ClockGetAlarm, ESMF_AlarmIsRinging, ESMF_AlarmRingerOff
  use ESMF             , only : operator(+), ESMF_LogWrite
  use NUOPC            , only : NUOPC_CompDerive, NUOPC_CompSetEntryPoint, NUOPC_CompSpecialize
  use NUOPC            , only : NUOPC_CompAttributeGet, NUOPC_Advertise
  use NUOPC_Model      , only : model_routine_SS        => SetServices
  use NUOPC_Model      , only : model_label_Advance     => label_Advance
  use NUOPC_Model      , only : model_label_SetRunClock => label_SetRunClock
  use NUOPC_Model      , only : model_label_Finalize    => label_Finalize
  use NUOPC_Model      , only : NUOPC_ModelGet, SetVM
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_const_mod    , only : SHR_CONST_SPVAL
  use shr_sys_mod      , only : shr_sys_abort
  use shr_cal_mod      , only : shr_cal_ymd2date
  use shr_log_mod     , only : shr_log_setLogUnit
  use dshr_methods_mod , only : dshr_state_getfldptr, dshr_state_diagnose, chkerr, memcheck
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_advance, shr_strdata_get_stream_domain
  use dshr_strdata_mod , only : shr_strdata_init_from_config
  use dshr_mod         , only : dshr_model_initphase, dshr_init
  use dshr_mod         , only : dshr_state_setscalar, dshr_set_runclock, dshr_check_restart_alarm
  use dshr_mod         , only : dshr_restart_read, dshr_restart_write, dshr_mesh_init
  use dshr_dfield_mod  , only : dfield_type, dshr_dfield_add, dshr_dfield_copy
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add, dshr_fldlist_realize

  implicit none
  private ! except

  public  :: SetServices
  public  :: SetVM
  private :: InitializeAdvertise
  private :: InitializeRealize
  private :: ModelAdvance
  private :: drof_comp_run
  private :: ModelFinalize

  !--------------------------------------------------------------------------
  ! Private module data
  !--------------------------------------------------------------------------

  type(shr_strdata_type)       :: sdat
  type(ESMF_Mesh)              :: model_mesh                          ! model mesh
  character(len=CS)            :: flds_scalar_name = ''
  integer                      :: flds_scalar_num = 0
  integer                      :: flds_scalar_index_nx = 0
  integer                      :: flds_scalar_index_ny = 0
  integer                      :: mpicom                              ! mpi communicator
  integer                      :: my_task                             ! my task in mpi communicator mpicom
  logical                      :: mainproc                          ! true of my_task == main_task
  character(len=16)            :: inst_suffix = ""                    ! char string associated with instance (ie. "_0001" or "")
  integer                      :: logunit                             ! logging unit number
  logical                      :: restart_read
  character(CL)                :: case_name                           ! case name
  character(*) , parameter     :: nullstr = 'null'
                                                                      ! drof_in namelist input
  character(CL)                :: streamfilename = nullstr            ! filename to obtain stream info from
  character(CL)                :: nlfilename = nullstr                ! filename to obtain namelist info from
  character(CL)                :: dataMode = nullstr                  ! flags physics options wrt input data
  character(CL)                :: model_meshfile = nullstr            ! full pathname to model meshfile
  character(CL)                :: model_maskfile = nullstr            ! full pathname to obtain mask from
  character(CL)                :: restfilm = nullstr                  ! model restart file namelist
  integer                      :: nx_global
  integer                      :: ny_global
  logical                      :: skip_restart_read = .false.         ! true => skip restart read
  logical                      :: diagnose_data = .true.
  integer      , parameter     :: main_task=0                       ! task number of main task
  character(*) , parameter     :: rpfile = 'rpointer.rof'
#ifdef CESMCOUPLED
  character(*) , parameter     :: modName =  "(rof_comp_nuopc)"
#else
  character(*) , parameter     :: modName =  "(cdeps_drof_comp)"
#endif

  ! linked lists
  type(fldList_type) , pointer :: fldsExport => null()
  type(dfield_type)  , pointer :: dfields    => null()

  ! model mask and model fraction
  real(r8), pointer            :: model_frac(:) => null()
  integer , pointer            :: model_mask(:) => null()

  ! module pointer arrays
  real(r8), pointer            :: Forr_rofl(:) => null()
  real(r8), pointer            :: Forr_rofi(:) => null()

  character(*) , parameter     :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Local varaibles
    character(len=*),parameter  :: subname=trim(modName)//':(SetServices) '
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

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer           :: inst_index ! number of current instance (ie. 1)
    integer           :: nu         ! unit number
    integer           :: ierr       ! error code
    type(fldlist_type), pointer :: fldList
    type(ESMF_VM)     :: vm
    integer :: bcasttmp(3)
    character(len=*),parameter :: subname=trim(modName)//':(InitializeAdvertise) '
    character(*)    ,parameter :: F00 = "('(" // trim(modName) // ") ',8a)"
    character(*)    ,parameter :: F01 = "('(" // trim(modName) // ") ',a,2x,i8)"
    character(*)    ,parameter :: F02 = "('(" // trim(modName) // ") ',a,l6)"
    !-------------------------------------------------------------------------------

    namelist / drof_nml / datamode, model_meshfile, model_maskfile, &
         restfilm, nx_global, ny_global, skip_restart_read

    rc = ESMF_SUCCESS

    call NUOPC_CompAttributeGet(gcomp, name='case_name', value=case_name, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Obtain flds_scalar values, mpi values, multi-instance values and
    ! set logunit and set shr logging to my log file
    call dshr_init(gcomp, 'ROF', sdat, mpicom, my_task, inst_index, inst_suffix, &
         flds_scalar_name, flds_scalar_num, flds_scalar_index_nx, flds_scalar_index_ny, &
         logunit, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! determine logical mainproc
    mainproc = (my_task == main_task)

    ! Read drof_nml from nlfilename
    if (mainproc) then
       nlfilename = "drof_in"//trim(inst_suffix)
       open (newunit=nu,file=trim(nlfilename),status="old",action="read")
       read (nu,nml=drof_nml,iostat=ierr)
       close(nu)
       if (ierr > 0) then
          write(logunit,*) 'ERROR: reading input namelist, '//trim(nlfilename)//' iostat=',ierr
          call shr_sys_abort(subName//': namelist read error '//trim(nlfilename))
       end if

       ! write namelist input to standard out
       write(logunit,F00)' datamode = ',trim(datamode)
       write(logunit,F00)' model_meshfile = ',trim(model_meshfile)
       write(logunit,F00)' model_maskfile = ',trim(model_maskfile)
       write(logunit,F01)' nx_global = ',nx_global
       write(logunit,F01)' ny_global = ',ny_global
       write(logunit,F00)' restfilm = ',trim(restfilm)
       write(logunit,F02)' skip_restart_read = ',skip_restart_read
       bcasttmp = 0
       bcasttmp(1) = nx_global
       bcasttmp(2) = ny_global
       if(skip_restart_read) bcasttmp(3) = 1
    end if

    ! broadcast namelist input
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMBroadcast(vm, datamode, CL, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, model_meshfile, CL, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, model_maskfile, CL, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, restfilm, CL, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, bcasttmp, 3, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    nx_global = bcasttmp(1)
    ny_global = bcasttmp(2)
    skip_restart_read = (bcasttmp(3) == 1)


    ! Validate datamode
    if (trim(datamode) == 'copyall') then
       if (mainproc) write(logunit,*) 'drof datamode = ',trim(datamode)
    else
       call shr_sys_abort(' ERROR illegal drof datamode = '//trim(datamode))
    end if

    call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
    call dshr_fldlist_add(fldsExport, "Forr_rofl")
    call dshr_fldlist_add(fldsExport, "Forr_rofi")

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(drof_comp_advertise): Fr_rof '//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine InitializeAdvertise

  !===============================================================================
  subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_TIME) :: currTime
    integer         :: current_ymd  ! model date
    integer         :: current_year ! model year
    integer         :: current_mon  ! model month
    integer         :: current_day  ! model day
    integer         :: current_tod  ! model sec into model date
    character(len=*), parameter :: F00   = "('" // trim(modName) // ": ')',8a)"
    character(len=*), parameter :: subname=trim(modName)//':(InitializeRealize) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Initialize mesh, restart flag, logunit
    call ESMF_TraceRegionEnter('drof_strdata_init')
    call dshr_mesh_init(gcomp, sdat, nullstr, logunit, 'ROF', nx_global, ny_global, &
         model_meshfile, model_maskfile, model_mesh, model_mask, model_frac, restart_read, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Initialize stream data type
    streamfilename = 'drof.streams'//trim(inst_suffix)
#ifndef DISABLE_FOX
    streamfilename = trim(streamfilename)//'.xml'
#endif
    call shr_strdata_init_from_config(sdat, streamfilename, model_mesh, clock, 'ROF', logunit, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TraceRegionExit('drof_strdata_init')

    ! NUOPC_Realize "realizes" a previously advertised field in the importState and exportState
    ! by replacing the advertised fields with the newly created fields of the same name.
    call dshr_fldlist_realize( exportState, fldsExport, flds_scalar_name, flds_scalar_num, model_mesh, &
         subname//':drofExport', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Get the time to interpolate the stream data to
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(currTime, yy=current_year, mm=current_mon, dd=current_day, s=current_tod, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_cal_ymd2date(current_year, current_mon, current_day, current_ymd)

    ! Run drof
    call drof_comp_run(exportstate, current_ymd, current_tod, restart_write=.false., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Add scalars to export state
    call dshr_state_SetScalar(dble(nx_global),flds_scalar_index_nx, exportState, flds_scalar_name, flds_scalar_num, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_SetScalar(dble(ny_global),flds_scalar_index_ny, exportState, flds_scalar_name, flds_scalar_num, rc)
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
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time)         :: currTime, nextTime
    integer                 :: next_ymd      ! model date
    integer                 :: next_tod      ! model sec into model date
    integer                 :: yr            ! year
    integer                 :: mon           ! month
    integer                 :: day           ! day in month
    logical                 :: restart_write ! restart alarm is ringing
    character(len=*),parameter :: subname=trim(modName)//':(ModelAdvance) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call memcheck(subname, 5, mainproc)
    call shr_log_setLogUnit(logunit)
    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(gcomp, modelClock=clock, importState=importState, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! For nuopc - the component clock is advanced at the end of the time interval
    ! For these to match for now - need to advance nuopc one timestep ahead for
    ! shr_strdata time interpolation
    call ESMF_ClockGet( clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    nextTime = currTime + timeStep
    call ESMF_TimeGet( nextTime, yy=yr, mm=mon, dd=day, s=next_tod, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_cal_ymd2date(yr, mon, day, next_ymd)

    ! write restart if alarm is ringing
    restart_write = dshr_check_restart_alarm(clock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! run drof
    call drof_comp_run(exportState, next_ymd, next_tod, restart_write, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine ModelAdvance

  !===============================================================================
  subroutine drof_comp_run(exportState, target_ymd, target_tod, restart_write, rc)

    ! --------------------------
    ! advance drof
    ! --------------------------

    ! input/output variables:
    type(ESMF_State) , intent(inout) :: exportState
    integer          , intent(in)    :: target_ymd       ! model date
    integer          , intent(in)    :: target_tod       ! model sec into model date
    logical          , intent(in)    :: restart_write
    integer          , intent(out)   :: rc

    ! local variables
    logical :: first_time = .true.
    integer :: n
    character(*), parameter :: subName = "(drof_comp_run) "
    !-------------------------------------------------------------------------------

    call ESMF_TraceRegionEnter('DROF_RUN')

    !--------------------
    ! First time initialization
    !--------------------

    if (first_time) then
       ! Initialize dfields
       call dshr_dfield_add(dfields, sdat, 'Forr_rofl', 'Forr_rofl', exportState, logunit, mainproc, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call dshr_dfield_add(dfields, sdat, 'Forr_rofi', 'Forr_rofi', exportState, logunit, mainproc, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Initialize module ponters
       call dshr_state_getfldptr(exportState, 'Forr_rofl' , fldptr1=Forr_rofl , rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       call dshr_state_getfldptr(exportState, 'Forr_rofi' , fldptr1=Forr_rofi , rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       ! Read restart if needed
       if (restart_read .and. .not. skip_restart_read) then
          call dshr_restart_read(restfilm, rpfile, inst_suffix, nullstr, logunit, my_task, mpicom, sdat)
       end if

       first_time = .false.
    end if

    !--------------------
    ! advance drof streams and update export state
    !--------------------

    ! time and spatially interpolate to model time and grid
    call ESMF_TraceRegionEnter('drof_strdata_advance')
    call shr_strdata_advance(sdat, target_ymd, target_tod, logunit, 'drof', rc=rc)
    call ESMF_TraceRegionExit('drof_strdata_advance')

    ! copy all fields from streams to export state as default
    ! This automatically will update the fields in the export state
    call ESMF_TraceRegionEnter('drof_dfield_copy')
    call dshr_dfield_copy(dfields,  sdat, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TraceRegionExit('drof_dfield_copy')

    ! determine data model behavior based on the mode
    call ESMF_TraceRegionEnter('drof_datamode')
    select case (trim(datamode))
    case('copyall')
       ! zero out "special values" of export fields
       do n = 1, size(Forr_rofl)
          if (abs(Forr_rofl(n)) > 1.0e28) Forr_rofl(n) = 0.0_r8
          if (abs(Forr_rofi(n)) > 1.0e28) Forr_rofi(n) = 0.0_r8
       enddo
    end select

    ! write restarts if needed
    if (restart_write) then
       select case (trim(datamode))
       case('copyall')
          call dshr_restart_write(rpfile, case_name, 'drof', inst_suffix, target_ymd, target_tod, &
               logunit, my_task, sdat)
       end select
    end if

    ! write diagnostics
    if (diagnose_data) then
       call dshr_state_diagnose(exportState, flds_scalar_name, subname//':ES',rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    call ESMF_TraceRegionExit('drof_datamode')
    call ESMF_TraceRegionExit('DROF_RUN')

  end subroutine drof_comp_run

  !===============================================================================
  subroutine ModelFinalize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    if (mainproc) then
       write(logunit,*)
       write(logunit,*) 'drof : end of main integration loop'
       write(logunit,*)
    end if
  end subroutine ModelFinalize

#ifdef CESMCOUPLED
end module rof_comp_nuopc
#else
end module cdeps_drof_comp
#endif
