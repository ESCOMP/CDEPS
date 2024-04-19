#ifdef CESMCOUPLED
module ocn_comp_nuopc
#else
module cdeps_docn_comp
#endif

  !----------------------------------------------------------------------------
  ! This is the NUOPC cap for DOCN
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
  use docn_datamode_copyall_mod    , only : docn_datamode_copyall_advertise
  use docn_datamode_copyall_mod    , only : docn_datamode_copyall_init_pointers
  use docn_datamode_copyall_mod    , only : docn_datamode_copyall_advance
  use docn_datamode_copyall_mod    , only : docn_datamode_copyall_restart_read
  use docn_datamode_copyall_mod    , only : docn_datamode_copyall_restart_write
  use docn_datamode_iaf_mod        , only : docn_datamode_iaf_advertise
  use docn_datamode_iaf_mod        , only : docn_datamode_iaf_init_pointers
  use docn_datamode_iaf_mod        , only : docn_datamode_iaf_advance
  use docn_datamode_iaf_mod        , only : docn_datamode_iaf_restart_read
  use docn_datamode_iaf_mod        , only : docn_datamode_iaf_restart_write
  use docn_datamode_som_mod        , only : docn_datamode_som_advertise
  use docn_datamode_som_mod        , only : docn_datamode_som_init_pointers
  use docn_datamode_som_mod        , only : docn_datamode_som_advance
  use docn_datamode_som_mod        , only : docn_datamode_som_restart_read
  use docn_datamode_som_mod        , only : docn_datamode_som_restart_write
  use docn_datamode_aquaplanet_mod , only : docn_datamode_aquaplanet_advertise
  use docn_datamode_aquaplanet_mod , only : docn_datamode_aquaplanet_init_pointers
  use docn_datamode_aquaplanet_mod , only : docn_datamode_aquaplanet_advance
  use docn_datamode_cplhist_mod    , only : docn_datamode_cplhist_advertise
  use docn_datamode_cplhist_mod    , only : docn_datamode_cplhist_init_pointers
  use docn_datamode_cplhist_mod    , only : docn_datamode_cplhist_advance
  use docn_datamode_cplhist_mod    , only : docn_datamode_cplhist_restart_read
  use docn_datamode_cplhist_mod    , only : docn_datamode_cplhist_restart_write
  use docn_datamode_multilev_mod   , only : docn_datamode_multilev_advertise
  use docn_datamode_multilev_mod   , only : docn_datamode_multilev_init_pointers
  use docn_datamode_multilev_mod   , only : docn_datamode_multilev_advance
  use docn_datamode_multilev_mod   , only : docn_datamode_multilev_restart_read
  use docn_datamode_multilev_mod   , only : docn_datamode_multilev_restart_write
  use docn_import_data_mod         , only : docn_import_data_advertise

  implicit none
  private ! except

  public  :: SetServices
  public  :: SetVM
  private :: InitializeAdvertise
  private :: InitializeRealize
  private :: ModelAdvance
  private :: docn_comp_run
  private :: ModelFinalize

  !--------------------------------------------------------------------------
  ! Private module data
  !--------------------------------------------------------------------------

  ! module variables common to all data models
  type(shr_strdata_type)       :: sdat
  type(ESMF_Mesh)              :: model_mesh
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

  ! docn_in namelist input
  character(CL)                :: streamfilename = nullstr            ! filename to obtain stream info from
  character(CL)                :: nlfilename = nullstr                ! filename to obtain namelist info from
  character(CL)                :: datamode = nullstr                  ! flags physics options wrt input data
  character(CL)                :: model_meshfile = nullstr            ! full pathname to model meshfile
  character(CL)                :: model_maskfile = nullstr            ! full pathname to obtain mask from
  real(R8)                     :: sst_constant_value                  ! sst constant value
  integer                      :: aquap_option                        ! if aqua-planet mode, option to use
  character(CL)                :: restfilm = nullstr                  ! model restart file namelist
  integer                      :: nx_global
  integer                      :: ny_global
  logical                      :: skip_restart_read = .false.         ! true => skip restart read in continuation run
  logical                      :: export_all = .false.                ! true => export all fields, do not check connected or not

  ! linked lists
  type(fldList_type) , pointer :: fldsImport => null()
  type(fldList_type) , pointer :: fldsExport => null()
  type(dfield_type)  , pointer :: dfields    => null()

  ! model mask and model fraction
  real(r8), pointer            :: model_frac(:) => null()
  integer , pointer            :: model_mask(:) => null()
  logical                      :: valid_ocn = .true. ! used for single column logic

  ! constants
  logical                      :: aquaplanet = .false.
  logical                      :: diagnose_data = .true.
  integer      , parameter     :: main_task = 0                 ! task number of main task
#ifdef CESMCOUPLED
  character(*) , parameter     :: module_name = "(ocn_comp_nuopc)"
#else
  character(*) , parameter     :: module_name = "(cdeps_docn_comp)"
#endif
  character(*) , parameter     :: modelname = 'docn'
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
    character(len=CL) :: import_data_fields ! colon deliminted strings of input data fields
    integer           :: bcasttmp(4)
    real(r8)          :: rtmp(1)
    type(ESMF_VM)     :: vm
    integer           :: nlev = 60  !DEBUG - remove this and put into namelist
    character(len=*),parameter :: subname=trim(module_name)//':(InitializeAdvertise) '
    character(*)    ,parameter :: F00 = "('(" // trim(module_name) // ") ',8a)"
    character(*)    ,parameter :: F01 = "('(" // trim(module_name) // ") ',a,2x,i8)"
    character(*)    ,parameter :: F02 = "('(" // trim(module_name) // ") ',a,l6)"
    character(*)    ,parameter :: F03 = "('(" // trim(module_name) // ") ',a,f8.5,2x,f8.5)"
    !-------------------------------------------------------------------------------

    namelist / docn_nml / datamode, &
         model_meshfile, model_maskfile, &
         restfilm,  nx_global, ny_global, sst_constant_value, skip_restart_read, &
         import_data_fields, export_all

    rc = ESMF_SUCCESS

    call NUOPC_CompAttributeGet(gcomp, name='case_name', value=case_name, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Obtain flds_scalar values, mpi values, multi-instance values and
    ! set logunit and set shr logging to my log file
    call dshr_init(gcomp, 'OCN', mpicom, my_task, inst_index, inst_suffix, &
         flds_scalar_name, flds_scalar_num, flds_scalar_index_nx, flds_scalar_index_ny, logunit, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Determine logical mainproc
    mainproc = (my_task == main_task)

    if (my_task == main_task) then

       ! Read docn_nml from nlfilename
       nlfilename = "docn_in"//trim(inst_suffix)
       open (newunit=nu,file=trim(nlfilename),status="old",action="read")
       call shr_nl_find_group_name(nu, 'docn_nml', status=ierr)
       read (nu,nml=docn_nml,iostat=ierr)
       close(nu)
       if (ierr > 0) then
          write(logunit,F00) 'ERROR: reading input namelist, '//trim(nlfilename)//' iostat=',ierr
          call shr_sys_abort(subName//': namelist read error '//trim(nlfilename))
       end if

       ! write namelist input to standard out
       write(logunit,F00)' case_name         = ',trim(case_name)
       write(logunit,F00)' datamode          = ',trim(datamode)
       write(logunit,F00)' model_meshfile    = ',trim(model_meshfile)
       write(logunit,F00)' model_maskfile    = ',trim(model_maskfile)
       write(logunit,F01)' nx_global         = ',nx_global
       write(logunit,F01)' ny_global         = ',ny_global
       write(logunit,F00)' restfilm          = ',trim(restfilm)
       write(logunit,F02)' skip_restart_read = ',skip_restart_read
       write(logunit,F00)' import_data_fields = ',trim(import_data_fields)
       write(logunit,*)  ' sst_constant_value = ',sst_constant_value
       write(logunit,F02)' export_all        = ', export_all

       bcasttmp = 0
       bcasttmp(1) = nx_global
       bcasttmp(2) = ny_global
       if(skip_restart_read) bcasttmp(3) = 1
       if(export_all) bcasttmp(4) = 1
       rtmp(1) = sst_constant_value
    endif

    ! Broadcast namelist input
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
    call ESMF_VMBroadcast(vm, import_data_fields, CL, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMBroadcast(vm, bcasttmp, 4, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMBroadcast(vm, rtmp, 1, main_task, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    nx_global = bcasttmp(1)
    ny_global = bcasttmp(2)
    skip_restart_read = (bcasttmp(3) == 1)
    export_all = (bcasttmp(4) == 1)
    sst_constant_value = rtmp(1)

    ! Special logic for prescribed aquaplanet
    if (datamode(1:9) == 'sst_aquap' .and. trim(datamode) /= 'sst_aquap_constant') then
       ! First determine the prescribed aquaplanet option
       if (len_trim(datamode) == 10) then
          read(datamode(10:10),'(i1)') aquap_option
       else if (len_trim(datamode) == 11) then
          read(datamode(10:11),'(i2)') aquap_option
       end if
       ! Now remove the index from the datamode value, to have a generic setting for later use
       datamode = "sst_aquap_analytic"
    end if

    ! Validate datamode
    if ( trim(datamode) == 'sstdata'            .or. & ! read stream, no import data
         trim(datamode) == 'iaf'                .or. & ! read stream, needs import data?
         trim(datamode) == 'sst_aquap_file'     .or. & ! read stream, no import data
         trim(datamode) == 'som'                .or. & ! read stream, needs import data
         trim(datamode) == 'som_aquap'          .or. & ! read stream, needs import data
         trim(datamode) == 'cplhist'            .or. & ! read stream, needs import data
         trim(datamode) == 'sst_aquap_analytic' .or. & ! analytic, no streams, import or export data
         trim(datamode) == 'sst_aquap_constant' .or. & ! analytic, no streams, import or export data
         trim(datamode) == 'multilev') then            ! multilevel ocean input
       ! success do nothing
    else
       call shr_sys_abort(' ERROR illegal docn datamode = '//trim(datamode))
    endif

    ! Advertise docn fields
    if (trim(datamode)=='sst_aquap_analytic' .or. trim(datamode)=='sst_aquap_constant') then
       aquaplanet = .true.
       call docn_datamode_aquaplanet_advertise(exportState, fldsExport, flds_scalar_name, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    else if (trim(datamode(1:3)) == 'som') then
       call docn_datamode_som_advertise(importState, exportState, fldsImport, fldsExport, flds_scalar_name, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    else if (trim(datamode) == 'sstdata' .or. trim(datamode) == 'sst_aquap_file') then
       call docn_datamode_copyall_advertise(exportState, fldsExport, flds_scalar_name, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    else if (trim(datamode) == 'iaf') then
       call docn_datamode_iaf_advertise(importState, exportState, fldsImport, fldsExport, flds_scalar_name, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    else if (trim(datamode) == 'cplhist') then
       call docn_datamode_cplhist_advertise(exportState, fldsExport, flds_scalar_name, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    else if (trim(datamode) == 'multilev') then
       call docn_datamode_multilev_advertise(exportState, fldsExport, flds_scalar_name, nlev, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    if (trim(import_data_fields) /= 'none') then
       call docn_import_data_advertise(importState, fldsImport, flds_scalar_name, import_data_fields, rc)
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
    call ESMF_TraceRegionEnter('docn_strdata_init')

    ! If aquaplanet overwrite  model mask and model frac to 1
    if (aquaplanet) then
       imask = 1
       write(cvalue,*) imask
       call NUOPC_CompAttributeSet(gcomp, name='scol_ocnmask', value=cvalue, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       rmask = 1._r8
       write(cvalue,*) rmask
       call NUOPC_CompAttributeSet(gcomp, name='scol_ocnfrac', value=cvalue, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    call dshr_mesh_init(gcomp, sdat, nullstr, logunit, 'OCN', nx_global, ny_global, &
         model_meshfile, model_maskfile, model_mesh, model_mask, model_frac, restart_read, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Initialize stream data type if not aqua planet
    if (.not. aquaplanet) then
       streamfilename = trim(modelname)//'.streams'//trim(inst_suffix)
#ifndef DISABLE_FoX
       streamfilename = trim(streamfilename)//'.xml'
#endif
       call shr_strdata_init_from_config(sdat, streamfilename, model_mesh, clock, 'OCN', logunit, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if
    call ESMF_TraceRegionExit('docn_strdata_init')

    ! Realize the actively coupled fields, now that a mesh is established and
    ! NUOPC_Realize "realizes" a previously advertised field in the importState and exportState
    ! by replacing the advertised fields with the newly created fields of the same name.
    call dshr_fldlist_realize( exportState, fldsExport, flds_scalar_name, flds_scalar_num, model_mesh, &
         subname//trim(modelname)//':Export', export_all, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_fldlist_realize( importState, fldsImport, flds_scalar_name, flds_scalar_num, model_mesh, &
         subname//trim(modelname)//':Import', .false., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! for single column, the target point might not be a valid ocn point
    if (size(model_mask) == 1 .and. model_mask(1) == 0) then
       valid_ocn = .false.
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
       call ESMF_VMLogMemInfo("Leaving "//trim(subname))
       RETURN
    end if

    ! Get the time to interpolate the stream data to
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(currTime, yy=current_year, mm=current_mon, dd=current_day, s=current_tod, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_cal_ymd2date(current_year, current_mon, current_day, current_ymd)

    ! Run docn
    call docn_comp_run(importState, exportState, clock, current_ymd, current_tod, restart_write=.false., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Add scalars to export state
    call dshr_state_SetScalar(dble(nx_global),flds_scalar_index_nx, exportState, flds_scalar_name, flds_scalar_num, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_SetScalar(dble(ny_global),flds_scalar_index_ny, exportState, flds_scalar_name, flds_scalar_num, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMLogMemInfo("Leaving "//trim(subname))
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
    logical                 :: restart_write
    character(len=*),parameter :: subname=trim(module_name)//':(ModelAdvance) '
    !-------------------------------------------------------------------------------


    rc = ESMF_SUCCESS
    call shr_log_setLogUnit(logunit)

    if (.not. valid_ocn) then
       RETURN
    end if

    call memcheck(subname, 5, my_task == main_task)

    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(gcomp, modelClock=clock, importState=importState, exportState=exportState, rc=rc)
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

    ! run docn
    call docn_comp_run(importState, exportState, clock, next_ymd, next_tod, restart_write, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine ModelAdvance

  !===============================================================================
  subroutine docn_comp_run(importState, exportState, clock, target_ymd, target_tod, restart_write, rc)

    ! --------------------------
    ! advance docn
    ! --------------------------

    ! input/output variables:
    type(ESMF_Clock) , intent(in)    :: clock
    type(ESMF_State) , intent(inout) :: importState
    type(ESMF_State) , intent(inout) :: exportState
    integer          , intent(in)    :: target_ymd       ! model date
    integer          , intent(in)    :: target_tod       ! model sec into model date
    logical          , intent(in)    :: restart_write
    integer          , intent(out)   :: rc

    ! local variables
    logical :: first_time = .true.
    character(*), parameter :: subName = "(docn_comp_run) "
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_TraceRegionEnter('DOCN_RUN')

    !--------------------
    ! First time initialization
    !--------------------

    if (first_time) then

       ! Initialize dfields
       call docn_init_dfields(importState, exportState, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Initialize datamode module ponters
       select case (trim(datamode))
       case('sstdata', 'sst_aquap_file')
          call docn_datamode_copyall_init_pointers(exportState, model_frac, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       case('iaf')
          call docn_datamode_iaf_init_pointers(importState, exportState, model_frac, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       case('som', 'som_aquap')
          call docn_datamode_som_init_pointers(importState, exportState, sdat, model_frac, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       case('sst_aquap_analytic', 'sst_aquap_constant')
          call  docn_datamode_aquaplanet_init_pointers(exportState, model_frac, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       case('cplhist')
          call docn_datamode_cplhist_init_pointers(exportState, model_frac, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       case('multilev')
          call docn_datamode_multilev_init_pointers(exportState, model_frac, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       end select

       ! Read restart if needed
       if (restart_read .and. .not. skip_restart_read) then
          select case (trim(datamode))
          case('sstdata', 'sst_aquap_file')
             call docn_datamode_copyall_restart_read(restfilm, inst_suffix, logunit, my_task, mpicom, sdat)
          case('iaf')
             call docn_datamode_iaf_restart_read(restfilm, inst_suffix, logunit, my_task, mpicom, sdat)
          case('som', 'som_aquap')
             call docn_datamode_som_restart_read(restfilm, inst_suffix, logunit, my_task, mpicom, sdat)
          end select
       end if

       ! Reset first_time
       first_time = .false.
    end if

    !--------------------
    ! Update export (and possibly import data model states)
    !--------------------

    ! Advance data model streams - time and spatially interpolate to model time and grid
    call ESMF_TraceRegionEnter('docn_strdata_advance')
    call shr_strdata_advance(sdat, target_ymd, target_tod, logunit, 'docn', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TraceRegionExit('docn_strdata_advance')

    ! Copy all fields from streams to export state as default
    ! This automatically will update the fields in the export state
    call ESMF_TraceRegionEnter('docn_dfield_copy')
    if(.not. aquaplanet) then
       call dshr_dfield_copy(dfields, sdat, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    endif
    call ESMF_TraceRegionExit('docn_dfield_copy')

    ! Perform data mode specific calculations
    select case (trim(datamode))
    case('sstdata','sst_aquap_file')
       call  docn_datamode_copyall_advance(rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case('iaf')
       call  docn_datamode_iaf_advance(rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case('som','som_aquap')
       call docn_datamode_som_advance(importState, exportState, clock, restart_read, datamode, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case('sst_aquap_analytic')
       call  docn_datamode_aquaplanet_advance(exportstate, model_mesh, sst_option=aquap_option, rc=rc)
       if (ChkErr(rc,__LINE__,u_file_u)) return
    case('sst_aquap_constant')
       call  docn_datamode_aquaplanet_advance(exportState, model_mesh, sst_constant_value=sst_constant_value, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case('cplhist')
       call  docn_datamode_cplhist_advance(rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    case('multilev')
       call  docn_datamode_multilev_advance(rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end select

    ! Write restarts if needed (no restarts for aquaplanet analytic or aquaplanet input file)
    if (restart_write) then
       select case (trim(datamode))
       case('sstdata','sst_aquap_file')
          call docn_datamode_copyall_restart_write(case_name, inst_suffix, target_ymd, target_tod, &
               logunit, my_task, sdat)
       case('iaf')
          call docn_datamode_iaf_restart_write(case_name, inst_suffix, target_ymd, target_tod, &
               logunit, my_task, sdat)
       case('som','som_aquap')
          call docn_datamode_som_restart_write(case_name, inst_suffix, target_ymd, target_tod, &
               logunit, my_task, sdat)
       case('cplhist')
          call docn_datamode_cplhist_restart_write(case_name, inst_suffix, target_ymd, target_tod, &
               logunit, my_task, sdat)
       end select
    end if

    call ESMF_TraceRegionExit('DOCN_RUN')

    ! write diagnostics
    if (diagnose_data) then
       call dshr_state_diagnose(exportState, flds_scalar_name, subname//':ES',rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

  contains

    subroutine docn_init_dfields(importState, exportState, rc)
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
      integer                         :: dimcount
      type(ESMF_Field)                :: lfield
      character(ESMF_MAXSTR) ,pointer :: lfieldnamelist(:)
      character(ESMF_MAXSTR)          :: fieldname(1)
      character(*), parameter   :: subName = "(docn_init_dfields) "
      !-------------------------------------------------------------------------------

      rc = ESMF_SUCCESS

      ! Initialize dfields data type (to map streams to export state fields)
      ! Create dfields linked list - used for copying stream fields to export state fields
      call ESMF_StateGet(exportState, itemCount=fieldCount, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      allocate(lfieldnamelist(fieldCount))
      call ESMF_StateGet(exportState, itemNameList=lfieldnamelist, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      do n = 1, fieldCount
         call ESMF_StateGet(exportState, itemName=trim(lfieldNameList(n)), field=lfield, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
         if (trim(lfieldnamelist(n)) /= flds_scalar_name) then
            call ESMF_FieldGet(lfield, dimcount=dimCount, rc=rc)
            if (chkerr(rc,__LINE__,u_FILE_u)) return
            if (dimcount == 2) then
               fieldname(1) = trim(lfieldnamelist(n))
               call dshr_dfield_add( dfields, sdat, trim(lfieldnamelist(n)), fieldname, exportState, &
                    logunit, mainproc, rc)
               if (chkerr(rc,__LINE__,u_FILE_u)) return
            else
               call dshr_dfield_add( dfields, sdat, trim(lfieldnamelist(n)), trim(lfieldnamelist(n)), exportState, &
                    logunit, mainproc, rc)
               if (chkerr(rc,__LINE__,u_FILE_u)) return
            endif
         end if
      end do
    end subroutine docn_init_dfields

  end subroutine docn_comp_run

  !===============================================================================
  subroutine ModelFinalize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    !-------------------------------------------------------------------------------
    rc = ESMF_SUCCESS
    if (my_task == main_task) then
       write(logunit,*)
       write(logunit,*) 'docn : end of main integration loop'
       write(logunit,*)
    end if
  end subroutine ModelFinalize

#ifdef CESMCOUPLED
end module ocn_comp_nuopc
#else
end module cdeps_docn_comp
#endif
