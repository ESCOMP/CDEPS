#ifdef CESMCOUPLED
module ice_comp_nuopc
#else
module cdeps_dice_comp
#endif

  !----------------------------------------------------------------------------
  ! This is the NUOPC cap for DICE
  !----------------------------------------------------------------------------

  use ESMF                 , only : ESMF_VM, ESMF_VMBroadcast, ESMF_GridCompGet
  use ESMF                 , only : ESMF_Mesh, ESMF_GridComp, ESMF_State, ESMF_Clock
  use ESMF                 , only : ESMF_SUCCESS, ESMF_Time, ESMF_LogWrite, ESMF_LOGMSG_INFO
  use ESMF                 , only : ESMF_TraceRegionEnter, ESMF_TraceRegionExit
  use ESMF                 , only : ESMF_Alarm, ESMF_TimeInterval, ESMF_TimeIntervalGet
  use ESMF                 , only : ESMF_AlarmIsRinging, ESMF_METHOD_INITIALIZE
  use ESMF                 , only : ESMF_ClockGet, ESMF_TimeGet, ESMF_MethodRemove, ESMF_MethodAdd
  use ESMF                 , only : ESMF_GridCompSetEntryPoint, operator(+), ESMF_AlarmRingerOff
  use ESMF                 , only : ESMF_ClockGetAlarm, ESMF_StateGet, ESMF_Field, ESMF_FieldGet, ESMF_MAXSTR
  use NUOPC                , only : NUOPC_CompDerive, NUOPC_CompSetEntryPoint, NUOPC_CompSpecialize
  use NUOPC                , only : NUOPC_CompAttributeGet, NUOPC_Advertise
  use NUOPC_Model          , only : model_routine_SS        => SetServices
  use NUOPC_Model          , only : model_label_Advance     => label_Advance
  use NUOPC_Model          , only : model_label_SetRunClock => label_SetRunClock
  use NUOPC_Model          , only : model_label_Finalize    => label_Finalize
  use NUOPC_Model          , only : NUOPC_ModelGet, SetVM
  use shr_kind_mod         , only : r8=>shr_kind_r8, cxx=>shr_kind_cxx, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_kind_mod         , only : cx=>shr_kind_cx
  use shr_const_mod        , only : shr_const_pi
  use shr_log_mod          , only : shr_log_setLogUnit, shr_log_error
  use shr_cal_mod          , only : shr_cal_ymd2date, shr_cal_ymd2julian
  use dshr_mod             , only : dshr_model_initphase, dshr_init, dshr_mesh_init, dshr_check_restart_alarm
  use dshr_mod             , only : dshr_state_setscalar, dshr_set_runclock, dshr_log_clock_advance
  use dshr_methods_mod     , only : dshr_state_diagnose, chkerr, memcheck
  use dshr_strdata_mod     , only : shr_strdata_type, shr_strdata_init_from_config, shr_strdata_advance
  use dshr_dfield_mod      , only : dfield_type, dshr_dfield_add, dshr_dfield_copy
  use dshr_fldlist_mod     , only : fldlist_type, dshr_fldlist_add, dshr_fldlist_realize

  use dice_datamode_ssmi_mod , only : dice_datamode_ssmi_advertise
  use dice_datamode_ssmi_mod , only : dice_datamode_ssmi_init_pointers
  use dice_datamode_ssmi_mod , only : dice_datamode_ssmi_advance
  use dice_datamode_ssmi_mod , only : dice_datamode_ssmi_restart_read
  use dice_datamode_ssmi_mod , only : dice_datamode_ssmi_restart_write
  !
  use dice_datamode_cplhist_mod , only : dice_datamode_cplhist_advertise
  use dice_datamode_cplhist_mod , only : dice_datamode_cplhist_init_pointers
  use dice_datamode_cplhist_mod , only : dice_datamode_cplhist_advance
  use dice_datamode_cplhist_mod , only : dice_datamode_cplhist_restart_read
  use dice_datamode_cplhist_mod , only : dice_datamode_cplhist_restart_write

  implicit none
  private ! except

  public  :: SetServices
  public  :: SetVM
  private :: InitializeAdvertise
  private :: InitializeRealize
  private :: ModelAdvance
  private :: dice_comp_run
  private :: ModelFinalize

  !--------------------------------------------------------------------------
  ! Module data
  !--------------------------------------------------------------------------

  type(shr_strdata_type)       :: sdat
  type(ESMF_Mesh)              :: model_mesh
  character(len=CS)            :: flds_scalar_name = ''
  integer                      :: flds_scalar_num = 0
  integer                      :: flds_scalar_index_nx = 0
  integer                      :: flds_scalar_index_ny = 0
  integer                      :: mpicom                              ! mpi communicator
  integer                      :: my_task                             ! my task in mpi communicator mpicom
  logical                      :: mainproc                          ! true of my_task == main_task
  character(len=16)            :: inst_suffix = ""                    ! char string associated with instance (ie. "_0001" or "")
  integer                      :: logunit                             ! logging unit number
  logical                      :: restart_read                        ! start from restart
  character(CL)                :: case_name     ! case name
  character(*) , parameter     :: nullstr = 'null'

  ! dice_in namelist input
  character(CX)                :: streamfilename = nullstr            ! filename to obtain stream info from
  character(CX)                :: nlfilename = nullstr                ! filename to obtain namelist info from
  character(CL)                :: dataMode                            ! flags physics options wrt input data
  character(CX)                :: model_meshfile = nullstr            ! full pathname to model meshfile
  character(CX)                :: model_maskfile = nullstr            ! full pathname to obtain mask from
  real(R8)                     :: flux_swpf                           ! short-wave penatration factor
  real(R8)                     :: flux_Qmin                           ! bound on melt rate
  logical                      :: flux_Qacc                           ! activates water accumulation/melt wrt Q
  real(R8)                     :: flux_Qacc0                          ! initial water accumulation value
  character(CX)                :: restfilm = nullstr                  ! model restart file namelist
  integer                      :: nx_global
  integer                      :: ny_global
  logical                      :: export_all = .false.                ! true => export all fields, do not check connected or not

  ! linked lists
  type(fldList_type) , pointer :: fldsImport => null()
  type(fldList_type) , pointer :: fldsExport => null()
  type(dfield_type)  , pointer :: dfields    => null()

  ! model mask and model fraction
  real(r8), pointer            :: model_frac(:) => null()
  integer , pointer            :: model_mask(:) => null()
  logical                      :: valid_ice = .true.                  ! used for single column logic (ocn mask > 0)

  ! constants
  logical                      :: flds_i2o_per_cat                    ! .true. if select per ice thickness
  real(R8)                     :: dt                                  ! real model timestep

  logical                      :: diagnose_data = .true.
  integer      , parameter     :: main_task=0                       ! task number of main task
#ifdef CESMCOUPLED
  character(*) , parameter     :: modName =  "(ice_comp_nuopc)"
#else
  character(*) , parameter     :: modName =  "(cdeps_dice_comp)"
#endif
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
    character(len=CL) :: cvalue             ! temporary
    integer           :: nu                 ! unit number
    integer           :: ierr               ! error code
    integer           :: bcasttmp(4)
    real(r8)          :: rbcasttmp(3)
    type(ESMF_VM)     :: vm
    character(len=*),parameter  :: subname=trim(modName)//':(InitializeAdvertise) '
    character(*)    ,parameter :: F00 = "('(" // trim(modName) // ") ',8a)"
    character(*)    ,parameter :: F01 = "('(" // trim(modName) // ") ',a,2x,i8)"
    character(*)    ,parameter :: F02 = "('(" // trim(modName) // ") ',a,l6)"
    character(*)    ,parameter :: F03 = "('(" // trim(modName) // ") ',a,d13.5)"
    !-------------------------------------------------------------------------------

    namelist / dice_nml /  datamode, &
         model_meshfile, model_maskfile, &
         restfilm, nx_global, ny_global, &
         flux_swpf, flux_Qmin, flux_Qacc, flux_Qacc0, export_all

    rc = ESMF_SUCCESS

    call NUOPC_CompAttributeGet(gcomp, name='case_name', value=case_name, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Obtain flds_scalar values, mpi values, multi-instance values and
    ! set logunit and set shr logging to my log file
    call dshr_init(gcomp, 'ICE', mpicom, my_task, inst_index, inst_suffix, &
         flds_scalar_name, flds_scalar_num, flds_scalar_index_nx, flds_scalar_index_ny, &
         logunit, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! determine logical mainproc
    mainproc = (my_task == main_task)

    ! Read dice_nml from nlfilename
    if (my_task == main_task) then
       nlfilename = "dice_in"//trim(inst_suffix)
       open (newunit=nu,file=trim(nlfilename),status="old",action="read")
       call shr_nl_find_group_name(nu, 'dice_nml', status=ierr)

       read (nu,nml=dice_nml,iostat=ierr)
       close(nu)
       if (ierr > 0) then
          rc = ierr
          call shr_log_error(subName//': namelist read error '//trim(nlfilename), rc=rc)
          return
       end if

       ! write namelist input to standard out
       write(logunit,F00)' datamode       = ',trim(datamode)
       write(logunit,F00)' model_meshfile = ',trim(model_meshfile)
       write(logunit,F00)' model_maskfile = ',trim(model_maskfile)
       write(logunit,F01)' nx_global  = ',nx_global
       write(logunit,F01)' ny_global  = ',ny_global
       write(logunit,F03)' flux_swpf  = ',flux_swpf
       write(logunit,F03)' flux_Qmin  = ',flux_Qmin
       write(logunit,F02)' flux_Qacc  = ',flux_Qacc
       write(logunit,F03)' flux_Qacc0 = ',flux_Qacc0
       write(logunit,F00)' restfilm = ',trim(restfilm)
       write(logunit,F02)' export_all = ',export_all
       bcasttmp = 0
       bcasttmp(1) = nx_global
       bcasttmp(2) = ny_global
       if(flux_Qacc) bcasttmp(3) = 1
       if(export_all) bcasttmp(4) = 1
       rbcasttmp(1) = flux_swpf
       rbcasttmp(2) = flux_Qmin
       rbcasttmp(3) = flux_Qacc0
    endif

    ! broadcast namelist input
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
    call ESMF_VMBroadcast(vm, rbcasttmp, 3, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    nx_global = bcasttmp(1)
    ny_global = bcasttmp(2)
    flux_Qacc = (bcasttmp(3) == 1)
    export_all= (bcasttmp(4) == 1)

    flux_swpf  = rbcasttmp(1)
    flux_Qmin  = rbcasttmp(2)
    flux_Qacc0 = rbcasttmp(3)

    ! Validate datamode
    if ( trim(datamode) == 'ssmi' .or. trim(datamode) == 'ssmi_iaf' .or. trim(datamode) == 'cplhist') then
       if (my_task == main_task) write(logunit,*) ' dice datamode = ',trim(datamode)
    else
       call shr_log_error(' ERROR illegal dice datamode = '//trim(datamode), rc=rc)
       return
    endif

    ! Advertise import and export fields
    if ( trim(datamode) == 'ssmi' .or. trim(datamode) == 'ssmi_iaf') then 
      call NUOPC_CompAttributeGet(gcomp, name='flds_i2o_per_cat', value=cvalue, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      read(cvalue,*) flds_i2o_per_cat  ! module variable
    endif

    !datamode already validated
    select case (trim(datamode))
    case('ssmi','ssmi_iaf')
       call dice_datamode_ssmi_advertise(importState, exportState, fldsimport, fldsexport, &
            flds_scalar_name, flds_i2o_per_cat, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case('cplhist')
       call dice_datamode_cplhist_advertise(exportState, fldsexport, flds_scalar_name, rc)
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
    type(ESMF_TimeInterval)     :: TimeStep
    type(ESMF_Time)             :: currTime
    integer                     :: current_ymd   ! model date
    integer                     :: current_year  ! model year
    integer                     :: current_mon   ! model month
    integer                     :: current_day   ! model day
    integer                     :: current_tod   ! model sec into model date
    real(R8)                    :: cosarg        ! for setting ice temp pattern
    real(R8)                    :: jday, jday0   ! elapsed day counters
    integer                     :: model_dt      ! integer model timestep
    type(ESMF_Field)            :: lfield
    character(CL) ,pointer      :: lfieldnamelist(:) => null()
    integer                     :: fieldcount
    real(r8), pointer           :: fldptr(:)
    integer                     :: n
    character(len=*), parameter :: F00   = "('" // trim(modName) // ": ')',8a)"
    character(len=*), parameter :: subname=trim(modName)//':(InitializeRealize) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Initialize mesh, restart flag, logunit
    call ESMF_TraceRegionEnter('dice_strdata_init')
    call dshr_mesh_init(gcomp, sdat, nullstr, logunit, 'ICE', nx_global, ny_global, &
         model_meshfile, model_maskfile, model_mesh, model_mask, model_frac, restart_read, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Initialize stream data type
    streamfilename = 'dice.streams'//trim(inst_suffix)
#ifndef DISABLE_FoX
    streamfilename = trim(streamfilename)//'.xml'
#endif
    call shr_strdata_init_from_config(sdat, streamfilename, model_mesh, clock, 'ICE', logunit, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TraceRegionExit('dice_strdata_init')

    ! NUOPC_Realize "realizes" a previously advertised field in the importState and exportState
    ! by replacing the advertised fields with the newly created fields of the same name.
    call dshr_fldlist_realize( exportState, fldsExport, flds_scalar_name, flds_scalar_num, model_mesh, &
         subname//':diceExport', export_all, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_fldlist_realize( importState, fldsImport, flds_scalar_name, flds_scalar_num, model_mesh, &
         subname//':diceImport', .false., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! for single column, the target point might not be a point where the ice/ocn mask is > 0
    if (size(model_frac) == 1 .and. model_frac(1) == 0._r8) then
       valid_ice = .false.
       call ESMF_StateGet(exportState, itemCount=fieldCount, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       allocate(lfieldnamelist(fieldCount))
       call ESMF_StateGet(exportState, itemNameList=lfieldnamelist, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       do n = 1, fieldCount
          if (trim(lfieldnamelist(n)) /= flds_scalar_name) then
             call ESMF_StateGet(exportState, itemName=trim(lfieldnamelist(n)), field=lfield, rc=rc)
             if (chkerr(rc,__LINE__,u_FILE_u)) return
             call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
             fldptr(:) = 0._r8
          end if
       enddo
       deallocate(lfieldnamelist)
       ! *******************
       ! *** RETURN HERE ***
       ! *******************
       RETURN
    end if

    ! Get the time to interpolate the stream data to
    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(currTime, yy=current_year, mm=current_mon, dd=current_day, s=current_tod, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_cal_ymd2date(current_year, current_mon, current_day, current_ymd)

    ! Get model timestep
    call ESMF_TimeIntervalGet( timeStep, s=model_dt, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    dt = model_dt * 1.0_r8

    ! Get cosarg
    call shr_cal_ymd2julian(0, current_mon, current_day, current_tod, jDay , sdat%model_calendar) ! julian day for model
    call shr_cal_ymd2julian(0, 9,           1,           0,           jDay0, sdat%model_calendar) ! julian day for Sept 1
    cosArg = 2.0_R8*shr_const_pi*(jday - jday0)/365.0_R8

    ! Run dice
    call dice_comp_run(gcomp, importState, exportState, current_ymd, current_tod, cosarg, restart_write=.false., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Add scalars to export state
    call dshr_state_SetScalar(dble(nx_global), flds_scalar_index_nx, exportState, flds_scalar_name, flds_scalar_num, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_SetScalar(dble(ny_global), flds_scalar_index_ny, exportState, flds_scalar_name, flds_scalar_num, rc)
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
    real(R8)                :: cosarg        ! for setting ice temp pattern
    real(R8)                :: jday, jday0   ! elapsed day counters
    integer                 :: next_ymd      ! model date
    integer                 :: next_tod      ! model sec into model date
    integer                 :: yr            ! year
    integer                 :: mon           ! month
    integer                 :: day           ! day in month
    logical                 :: restart_write
    character(len=*),parameter :: subname=trim(modName)//':(ModelAdvance) '
    !-------------------------------------------------------------------------------

    if (.not. valid_ice) then
       RETURN
    end if
    call shr_log_setLogUnit(logunit)

    rc = ESMF_SUCCESS

    call ESMF_TraceRegionEnter(subname)
    call memcheck(subname, 5, my_task == main_task)

    ! Query the Component for its clock, importState and exportState
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

    ! Get cosarg
    call shr_cal_ymd2julian(0, mon, day, next_tod, jDay , sdat%model_calendar) ! julian day for model
    call shr_cal_ymd2julian(0, 9,   1,   0,        jDay0, sdat%model_calendar) ! julian day for Sept 1
    cosArg = 2.0_R8*shr_const_pi*(jday - jday0)/365.0_R8

    ! Determine if will write restarts
    restart_write = dshr_check_restart_alarm(clock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Run dice
    call dice_comp_run(gcomp, importState, exportState, next_ymd, next_tod, cosarg, restart_write, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TraceRegionExit(subname)

  end subroutine ModelAdvance

  !===============================================================================
  subroutine dice_comp_run(gcomp, importstate, exportstate, target_ymd, target_tod, cosarg, restart_write, rc)
    use nuopc_shr_methods, only : shr_get_rpointer_name

    ! --------------------------
    ! advance dice
    ! --------------------------

    ! input/output variables:
    type(ESMF_GridComp), intent(in)  :: gcomp
    type(ESMF_State) , intent(inout) :: exportState
    type(ESMF_State) , intent(inout) :: importState
    integer          , intent(in)    :: target_ymd ! model date
    integer          , intent(in)    :: target_tod ! model sec into model date
    real(R8)         , intent(in)    :: cosarg     ! for setting ice temp pattern
    logical          , intent(in)    :: restart_write
    integer          , intent(out)   :: rc

    ! local variables
    logical :: first_time = .true.
    character(len=CL) :: rpfile
    character(*), parameter :: subName = "(dice_comp_run) "
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_TraceRegionEnter('DICE_RUN')

    !--------------------
    ! first time initialization
    !--------------------

    if (first_time) then

       ! Initialize dfields with export state data that has corresponding stream fieldi
       select case (trim(datamode))
       case('ssmi','ssmi_iaf')
         call dshr_dfield_add(dfields, sdat, state_fld='Si_ifrac', strm_fld='Si_ifrac', &
            state=exportState, logunit=logunit, mainproc=mainproc, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
       case('cplhist')
         call dice_init_dfields(importState, exportState, rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
       end select

       ! Initialize datamode module ponters
       select case (trim(datamode))
       case('ssmi', 'ssmi_iaf')
          call dice_datamode_ssmi_init_pointers(importState, exportState, sdat, flds_i2o_per_cat, rc)
          if (chkerr(rc,__LINE__,u_FILE_u)) return
       case('cplhist')
          call dice_datamode_cplhist_init_pointers(importState,exportState,sdat,rc)
          if (chkerr(rc,__LINE__,u_FILE_u)) return
       end select

       ! read restart if needed
       if (restart_read) then
          call shr_get_rpointer_name(gcomp, 'ice', target_ymd, target_tod, rpfile, 'read', rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          select case (trim(datamode))
          case('ssmi', 'ssmi_iaf')
             call dice_datamode_ssmi_restart_read(restfilm, rpfile, logunit, my_task, mpicom, sdat)
          case('cplhist')
             call dice_datamode_cplhist_restart_read(restfilm, rpfile, logunit, my_task, mpicom, sdat) 
          end select
       end if

       ! reset first_time
       first_time = .false.
    end if

    !--------------------
    ! advance dice streams
    !--------------------

    ! time and spatially interpolate to model time and grid

    call ESMF_TraceRegionEnter('dice_strdata_advance')
    call shr_strdata_advance(sdat, target_ymd, target_tod, logunit, 'dice', rc=rc)
    call ESMF_TraceRegionExit('dice_strdata_advance')

    !--------------------
    ! copy all fields from streams to export state as default
    !--------------------

    ! This automatically will update the fields in the export state

    call ESMF_TraceRegionEnter('dice_dfield_copy')
    call dshr_dfield_copy(dfields, sdat, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TraceRegionExit('dice_dfield_copy')

    !-------------------------------------------------
    ! Determine data model behavior based on the mode
    !-------------------------------------------------

    call ESMF_TraceRegionEnter('dice_datamode')

    ! Perform data mode specific calculations
    select case (trim(datamode))
    case ('ssmi', 'ssmi_iaf')
       call dice_datamode_ssmi_advance(exportState, importState, cosarg, flds_i2o_per_cat, &
            flux_swpf, flux_Qmin, flux_Qacc, flux_Qacc0, dt, logunit, restart_read, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case ('cplhist')
       call dice_datamode_cplhist_advance(rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return 
    end select

    ! Write restarts if needed
    if (restart_write) then
       call shr_get_rpointer_name(gcomp, 'ice', target_ymd, target_tod, rpfile, 'write', rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       select case (trim(datamode))
       case('ssmi', 'ssmi_iaf')
          call dice_datamode_ssmi_restart_write(rpfile, case_name, inst_suffix, target_ymd, target_tod, &
               logunit, my_task, sdat)
       case ('cplhist')
          call dice_datamode_cplhist_restart_write(rpfile, case_name, inst_suffix, target_ymd, target_tod, &
               logunit, my_task, sdat)
       end select
    end if

    ! Write diagnostics
    if (diagnose_data) then
       call dshr_state_diagnose(exportState, flds_scalar_name, subname//':ES',rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    call ESMF_TraceRegionExit('dice_datamode')
    call ESMF_TraceRegionExit('DICE_RUN')

  contains
    subroutine dice_init_dfields(importState, exportState, rc)
      ! -----------------------------
      ! Initialize dfields arrays
      ! -----------------------------

      ! input/output variables
      type(ESMF_State)       , intent(inout) :: importState
      type(ESMF_State)       , intent(inout) :: exportState
      integer                , intent(out)   :: rc

      ! local variables
      integer                         :: n
      integer                         :: fieldcount
      type(ESMF_Field)                :: lfield
      character(ESMF_MAXSTR) ,pointer :: lfieldnamelist(:)
      character(*), parameter   :: subName = "(dice_init_dfields) "
      !-------------------------------------------------------------------------------

      rc = ESMF_SUCCESS

      ! Initialize dfields data type (to map streams to export state fields)
      ! Create dfields linked list - used for copying stream fields to export
      ! state fields
      call ESMF_StateGet(exportState, itemCount=fieldCount, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      allocate(lfieldnamelist(fieldCount))
      call ESMF_StateGet(exportState, itemNameList=lfieldnamelist, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      do n = 1, fieldCount
         call ESMF_StateGet(exportState, itemName=trim(lfieldNameList(n)), field=lfield, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
         if (trim(lfieldnamelist(n)) /= flds_scalar_name) then
            call dshr_dfield_add( dfields, sdat, trim(lfieldnamelist(n)), trim(lfieldnamelist(n)), exportState, &
                 logunit, mainproc, rc)
            if (chkerr(rc,__LINE__,u_FILE_u)) return
         end if
      end do
    end subroutine dice_init_dfields

  end subroutine dice_comp_run

  !===============================================================================
  subroutine ModelFinalize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (my_task == main_task) then
       write(logunit,*)
       write(logunit,*) 'dice : end of main integration loop'
       write(logunit,*)
    end if
  end subroutine ModelFinalize

#ifdef CESMCOUPLED
end module ice_comp_nuopc
#else
end module cdeps_dice_comp
#endif
