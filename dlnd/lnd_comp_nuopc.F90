#ifdef CESMCOUPLED
module lnd_comp_nuopc
#else
module cdeps_dlnd_comp
#endif

  !----------------------------------------------------------------------------
  ! This is the NUOPC cap for DLND
  !----------------------------------------------------------------------------
  use ESMF              , only : ESMF_VM, ESMF_VMBroadcast, ESMF_GridCompGet
  use ESMF              , only : ESMF_Mesh, ESMF_GridComp, ESMF_SUCCESS, ESMF_LOGMSG_INFO
  use ESMF              , only : ESMF_LogWrite, ESMF_TraceRegionExit, ESMF_TraceRegionEnter
  use ESMF              , only : ESMF_Clock, ESMF_Alarm, ESMF_State, ESMF_ClockGet, ESMF_timeGet
  use ESMF              , only : ESMF_Time, ESMF_TimeInterval, ESMF_METHOD_INITIALIZE
  use ESMF              , only : ESMF_MethodAdd, ESMF_MethodRemove
  use ESMF              , only : ESMF_ClockGetAlarm, ESMF_AlarmIsRinging, ESMF_AlarmRingerOff
  use ESMF              , only : ESMF_GridCompSetEntryPoint, operator(+)
  use NUOPC             , only : NUOPC_CompDerive, NUOPC_CompSetEntryPoint, NUOPC_CompSpecialize
  use NUOPC             , only : NUOPC_CompAttributeGet, NUOPC_Advertise
  use NUOPC_Model       , only : model_routine_SS        => SetServices
  use NUOPC_Model       , only : model_label_Advance     => label_Advance
  use NUOPC_Model       , only : model_label_SetRunClock => label_SetRunClock
  use NUOPC_Model       , only : model_label_Finalize    => label_Finalize
  use NUOPC_Model       , only : NUOPC_ModelGet, SetVM
  use shr_kind_mod      , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_kind_mod      , only : cx=>shr_kind_cx
  use shr_cal_mod       , only : shr_cal_ymd2date
  use shr_log_mod       , only : shr_log_setLogUnit, shr_log_error
  use dshr_methods_mod  , only : dshr_state_getfldptr, dshr_state_diagnose, chkerr, memcheck
  use dshr_strdata_mod  , only : shr_strdata_type, shr_strdata_advance, shr_strdata_get_stream_domain
  use dshr_strdata_mod  , only : shr_strdata_init_from_config
  use dshr_mod          , only : dshr_model_initphase, dshr_init, dshr_check_restart_alarm
  use dshr_mod          , only : dshr_state_setscalar, dshr_set_runclock, dshr_log_clock_advance
  use dshr_mod          , only : dshr_restart_read, dshr_restart_write, dshr_mesh_init
  use dshr_dfield_mod   , only : dfield_type, dshr_dfield_add, dshr_dfield_copy
  use dshr_fldlist_mod  , only : fldlist_type, dshr_fldlist_add, dshr_fldlist_realize
  use glc_elevclass_mod , only : glc_elevclass_as_string, glc_elevclass_init
  use nuopc_shr_methods , only : shr_get_rpointer_name
  
  implicit none
  private ! except

  public  :: SetServices
  public  :: SetVM
  private :: InitializeAdvertise
  private :: InitializeRealize
  private :: ModelAdvance
  private :: ModelFinalize
  private :: dlnd_comp_advertise
  private :: dlnd_comp_realize
  private :: dlnd_comp_run

  !--------------------------------------------------------------------------
  ! Private module data
  !--------------------------------------------------------------------------

  type(shr_strdata_type)   :: sdat                                ! instantiation of shr_strdata_type
  type(ESMF_Mesh)          :: model_mesh                          ! model mesh
  character(len=CS)        :: flds_scalar_name = ''
  integer                  :: flds_scalar_num = 0
  integer                  :: flds_scalar_index_nx = 0
  integer                  :: flds_scalar_index_ny = 0
  integer                  :: mpicom                              ! mpi communicator
  integer                  :: my_task                             ! my task in mpi communicator mpicom
  logical                  :: mainproc                          ! true of my_task == main_task
  integer                  :: inst_index                          ! number of current instance (ie. 1)
  character(len=16)        :: inst_suffix = ""                    ! char string associated with instance (ie. "_0001" or "")
  integer                  :: logunit                             ! logging unit number
  logical                  :: restart_read                        ! start from restart
  character(CL)            :: case_name                           ! case name
  character(*) , parameter :: nullstr = 'null'

  ! dlnd_in namelist input
  character(CL)            :: dataMode = nullstr                  ! flags physics options wrt input data
  character(CX)            :: model_meshfile = nullstr            ! full pathname to model meshfile
  character(CX)            :: model_maskfile = nullstr            ! full pathname to obtain mask from
  character(CX)            :: streamfilename                      ! filename to obtain stream info from
  character(CX)            :: nlfilename = nullstr                ! filename to obtain namelist info from
!  not currently used
!  logical                  :: force_prognostic_true = .false.     ! if true set prognostic true
  character(CX)            :: restfilm = nullstr                  ! model restart file namelist
  integer                  :: nx_global                           ! global nx dimension of model mesh
  integer                  :: ny_global                           ! global ny dimension of model mesh
  logical                  :: skip_restart_read = .false.         ! true => skip restart read in continuation
  logical                  :: export_all = .false.                ! true => export all fields, do not check connected or not

  ! linked lists
  type(fldList_type) , pointer :: fldsExport => null()
  type(dfield_type)  , pointer :: dfields    => null()

  ! model mask and model fraction
  real(r8), pointer            :: model_frac(:) => null()
  integer , pointer            :: model_mask(:) => null()

  ! module pointer arrays
  real(r8), pointer            :: lfrac(:)

  ! module constants
  integer                      :: glc_nec
  logical                      :: diagnose_data = .true.
  integer      , parameter     :: main_task=0                   ! task number of main task
#ifdef CESMCOUPLED
  character(*) , parameter     :: modName =  "(lnd_comp_nuopc)"
#else
  character(*) , parameter     :: modName =  "(cdeps_dlnd_comp)"
#endif
  character(*) , parameter     :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    character(len=*),parameter  :: subname=trim(modName)//':(SetServices) '
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
    type(ESMF_VM) :: vm
    character(CL) :: cvalue
    integer       :: nu         ! unit number
    integer       :: bcasttmp(4)
    integer       :: ierr       ! error code
    character(len=*) , parameter :: subname=trim(modName)//':(InitializeAdvertise) '
    character(*)     , parameter :: F00 = "('(" // trim(modName) // ") ',8a)"
    character(*)     , parameter :: F01 = "('(" // trim(modName) // ") ',a,2x,i8)"
    character(*)     , parameter :: F02 = "('(" // trim(modName) // ") ',a,l6)"
    !-------------------------------------------------------------------------------

    namelist / dlnd_nml / datamode, model_meshfile, model_maskfile, &
         nx_global, ny_global, restfilm, skip_restart_read, export_all

    rc = ESMF_SUCCESS

    call NUOPC_CompAttributeGet(gcomp, name='case_name', value=case_name, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Obtain flds_scalar values, mpi values, multi-instance values and
    ! set logunit and set shr logging to my log file
    call dshr_init(gcomp, 'LND', mpicom, my_task, inst_index, inst_suffix, &
         flds_scalar_name, flds_scalar_num, flds_scalar_index_nx, flds_scalar_index_ny, &
         logunit, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Determine logical mainproc
    mainproc = (my_task == main_task)

    ! Read dlnd_nml from nlfilename
    if (my_task == main_task) then
       nlfilename = "dlnd_in"//trim(inst_suffix)
       open (newunit=nu, file=trim(nlfilename), status="old", action="read")
       call shr_nl_find_group_name(nu, 'dlnd_nml', status=ierr)

       read (nu,nml=dlnd_nml,iostat=ierr)
       close(nu)
       if (ierr > 0) then
          rc = ierr
          call shr_log_error(subName//': namelist read error '//trim(nlfilename), rc=rc)
          return
       end if
       bcasttmp = 0
       bcasttmp(1) = nx_global
       bcasttmp(2) = ny_global
       if(skip_restart_read) bcasttmp(3) = 1
       if(export_all) bcasttmp(4) = 1
    end if

    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMBroadcast(vm, datamode, CL, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, model_meshfile, CX, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, model_maskfile, CX, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, restfilm, CX, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMBroadcast(vm, bcasttmp, 3, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    nx_global = bcasttmp(1)
    ny_global = bcasttmp(2)
    skip_restart_read = (bcasttmp(3) == 1)
    export_all = (bcasttmp(4) == 1)

    ! write namelist input to standard out
    if (my_task == main_task) then
       write(logunit,F00)' model_meshfile    = ',trim(model_meshfile)
       write(logunit,F00)' model_maskfile    = ',trim(model_maskfile)
       write(logunit,F00)' datamode          = ',datamode
       write(logunit,F01)' nx_global         = ',nx_global
       write(logunit,F01)' ny_global         = ',ny_global
       write(logunit,F00)' restfilm          = ',trim(restfilm)
       write(logunit,F02)' skip_restart_read = ',skip_restart_read
       write(logunit,F02)' export_all        = ',export_all
    endif

    ! Validate sdat datamode
    if (trim(datamode) == 'copyall') then
       if (my_task == main_task) write(logunit,*) 'dlnd datamode = ',trim(datamode)
    else
       call shr_log_error(' ERROR illegal dlnd datamode = '//trim(datamode), rc=rc)
       return
    end if
    call NUOPC_CompAttributeGet(gcomp, name='glc_nec', value=cvalue, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) glc_nec
    call ESMF_LogWrite('glc_nec = '// trim(cvalue), ESMF_LOGMSG_INFO)

    ! Advertise the export fields
    call dlnd_comp_advertise(importState, exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

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
    character(len=cl) :: rpfile     ! restart pointer file name
    character(len=*),parameter :: subname=trim(modName)//':(InitializeRealize) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Initialize sdat
    call ESMF_TraceRegionEnter('dlnd_strdata_init')
    call dshr_mesh_init(gcomp, sdat, nullstr, logunit, 'LND', nx_global, ny_global, &
         model_meshfile, model_maskfile, model_mesh, model_mask, model_frac, restart_read, rc=rc)

    ! Initialize stream data type
    streamfilename = 'dlnd.streams'//trim(inst_suffix)
#ifndef DISABLE_FoX
    streamfilename = trim(streamfilename)//'.xml'
#endif
    call shr_strdata_init_from_config(sdat, streamfilename, model_mesh, clock, 'LND', logunit, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TraceRegionExit('dlnd_strdata_init')

    ! Realize the actively coupled fields, now that a mesh is established and
    ! initialize dfields data type (to map streams to export state fields)
    call dlnd_comp_realize(importState, exportState, export_all, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! get the time to interpolate the stream data to
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(currTime, yy=current_year, mm=current_mon, dd=current_day, s=current_tod, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_cal_ymd2date(current_year, current_mon, current_day, current_ymd)

    ! Read restart if necessary
    if (restart_read .and. .not. skip_restart_read) then
       call shr_get_rpointer_name(gcomp, 'lnd', current_ymd, current_tod, rpfile, 'read', rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call dshr_restart_read(restfilm, rpfile, logunit, my_task, mpicom, sdat, rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
    end if


    ! Run dlnd to create export state
    call dlnd_comp_run(importState, exportState, current_ymd, current_tod, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! add scalars to export state
    call dshr_state_SetScalar(dble(nx_global),flds_scalar_index_nx, exportState, flds_scalar_name, flds_scalar_num, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_SetScalar(dble(ny_global),flds_scalar_index_ny, exportState, flds_scalar_name, flds_scalar_num, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! diagnostics
    if (diagnose_data) then
       call dshr_state_diagnose(exportState, flds_scalar_name, subname//':ES',rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

  end subroutine InitializeRealize

  !===============================================================================
  subroutine ModelAdvance(gcomp, rc)
    use nuopc_shr_methods, only : shr_get_rpointer_name
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
    logical                 :: write_restart
    character(len=CL)       :: rpfile
    character(len=*),parameter :: subname=trim(modName)//':(ModelAdvance) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call shr_log_setLogUnit(logunit)
    call memcheck(subname, 5, my_task==main_task)

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

    ! run dlnd
    call dlnd_comp_run(importState, exportState, next_ymd, next_tod, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! write_restart if alarm is ringing
    write_restart = dshr_check_restart_alarm(clock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (write_restart) then
       call ESMF_TraceRegionEnter('dlnd_restart')
       call shr_get_rpointer_name(gcomp, 'lnd', next_ymd, next_tod, rpfile, 'write', rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call dshr_restart_write(rpfile, case_name, 'dlnd', inst_suffix, next_ymd, next_tod, &
            logunit, my_task, sdat, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_TraceRegionExit('dlnd_restart')
    endif


    ! write diagnostics
    if (diagnose_data) then
       call dshr_state_diagnose(exportState, flds_scalar_name, subname//':ES',rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

  end subroutine ModelAdvance

  !===============================================================================
  subroutine ModelFinalize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(*), parameter :: F00   = "('(dlnd_comp_final) ',8a)"
    character(*), parameter :: F91   = "('(dlnd_comp_final) ',73('-'))"
    character(len=*),parameter  :: subname=trim(modName)//':(ModelFinalize) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)
    if (my_task == main_task) then
       write(logunit,F91)
       write(logunit,F00) ' dlnd : end of main integration loop'
       write(logunit,F91)
    end if
    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine ModelFinalize

  !===============================================================================
  subroutine dlnd_comp_advertise(importState, exportState, rc)

    ! determine export and import fields to advertise to mediator

    ! input/output arguments
    type(ESMF_State)               :: importState
    type(ESMF_State)               :: exportState
    integer          , intent(out) :: rc

    ! local variables
    type(fldlist_type), pointer :: fldList
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call glc_elevclass_init(glc_nec)

    !-------------------
    ! Advertise export fields
    !-------------------

    call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
    call dshr_fldlist_add(fldsExport, "Sl_lfrin")

    ! The following puts all of the elevation class fields as an
    ! undidstributed dimension in the export state field - index1 is bare land - and the total number of
    ! elevation classes not equal to bare land go from index2 -> glc_nec+1
    if (glc_nec > 0) then
       call dshr_fldList_add(fldsExport, 'Sl_tsrf_elev'  , ungridded_lbound=1, ungridded_ubound=glc_nec+1)
       call dshr_fldList_add(fldsExport, 'Sl_topo_elev'  , ungridded_lbound=1, ungridded_ubound=glc_nec+1)
       call dshr_fldList_add(fldsExport, 'Flgl_qice_elev', ungridded_lbound=1, ungridded_ubound=glc_nec+1)
    end if

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(dlnd_comp_advertise): Fr_lnd '//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

    ! TODO: Non snow fields that nead to be added if dlnd is in cplhist mode
    ! "Sl_t        " "Sl_tref     " "Sl_qref     " "Sl_avsdr    "
    ! "Sl_anidr    " "Sl_avsdf    " "Sl_anidf    " "Sl_snowh    "
    ! "Fall_taux   " "Fall_tauy   " "Fall_lat    " "Fall_sen    "
    ! "Fall_lwup   " "Fall_evap   " "Fall_swnet  " "Sl_landfrac "
    ! "Sl_fv       " "Sl_ram1     "
    ! "Fall_flxdst1" "Fall_flxdst2" "Fall_flxdst3" "Fall_flxdst4"

  end subroutine dlnd_comp_advertise

  !===============================================================================
  subroutine dlnd_comp_realize(importState, exportState, export_all, rc)

    ! input/output variables
    type(ESMF_State) , intent(inout) :: importState
    type(ESMF_State) , intent(inout) :: exportState
    logical          , intent(in)    :: export_all
    integer          , intent(out)   :: rc

    ! local variables
    character(*), parameter    :: subName = "(dlnd_comp_realize) "
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    ! -------------------------------------
    ! NUOPC_Realize "realizes" a previously advertised field in the importState and exportState
    ! by replacing the advertised fields with the newly created fields of the same name.
    ! -------------------------------------

    call dshr_fldlist_realize( exportState, fldsExport, flds_scalar_name, flds_scalar_num,  model_mesh, &
         subname//':dlndExport', export_all, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine dlnd_comp_realize

  !===============================================================================
  subroutine dlnd_comp_run(importState, exportState, target_ymd, target_tod, rc)

    ! --------------------------
    ! advance dlnd
    ! --------------------------

    ! input/output variables:
    type(ESMF_State) , intent(inout) :: importState
    type(ESMF_State) , intent(inout) :: ExportState
    integer          , intent(in)    :: target_ymd       ! model date
    integer          , intent(in)    :: target_tod       ! model sec into model date
    integer          , intent(out)   :: rc

    ! local variables
    logical                    :: first_time = .true.
    integer                    :: n
    character(len=2)           :: nec_str
    character(CS), allocatable :: strm_flds(:)
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_TraceRegionEnter('DLND_RUN')

    !--------------------
    ! set module pointers
    !--------------------

    if (first_time) then

       ! Set fractional land pointer in export state
       call dshr_state_getfldptr(exportState, fldname='Sl_lfrin', fldptr1=lfrac, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       lfrac(:) = model_frac(:)

       ! Create stream-> export state mapping
       ! Note that strm_flds is the model name for the stream field
       ! Note that state_fld is the model name for the export field
       allocate(strm_flds(0:glc_nec))
       do n = 0,glc_nec
          nec_str = glc_elevclass_as_string(n)
          strm_flds(n) = 'Sl_tsrf_elev' // trim(nec_str)
       end do
       call dshr_dfield_add(dfields, sdat, state_fld='Sl_tsrf_elev', strm_flds=strm_flds, state=exportState, &
            logunit=logunit, mainproc=mainproc, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       do n = 0,glc_nec
          nec_str = glc_elevclass_as_string(n)
          strm_flds(n) = 'Sl_topo_elev' // trim(nec_str)
       end do
       call dshr_dfield_add(dfields, sdat, state_fld='Sl_topo_elev', strm_flds=strm_flds, state=exportState, &
            logunit=logunit, mainproc=mainproc, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       do n = 0,glc_nec
          nec_str = glc_elevclass_as_string(n)
          strm_flds(n) = 'Flgl_qice_elev' // trim(nec_str)
       end do
       call dshr_dfield_add(dfields, sdat, state_fld='Flgl_qice_elev', strm_flds=strm_flds, state=exportState, &
            logunit=logunit, mainproc=mainproc, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       first_time = .false.
    end if

    !--------------------
    ! advance dlnd streams
    !--------------------

    ! time and spatially interpolate to model time and grid
    call ESMF_TraceRegionEnter('dlnd_strdata_advance')
    call shr_strdata_advance(sdat, target_ymd, target_tod, logunit, 'dlnd', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TraceRegionExit('dlnd_strdata_advance')

    ! copy all fields from streams to export state as default
    ! This automatically will update the fields in the export state
    call ESMF_TraceRegionEnter('dlnd_strdata_copy')
    call dshr_dfield_copy(dfields, sdat, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TraceRegionExit('dlnd_strdata_copy')

    ! determine data model behavior based on the mode
    call ESMF_TraceRegionEnter('dlnd_datamode')
    select case (trim(datamode))
    case('copyall')
       ! do nothing extra
    end select

    call ESMF_TraceRegionExit('dlnd_datamode')
    call ESMF_TraceRegionExit('DLND_RUN')

  end subroutine dlnd_comp_run

#ifdef CESMCOUPLED
end module lnd_comp_nuopc
#else
end module cdeps_dlnd_comp
#endif
