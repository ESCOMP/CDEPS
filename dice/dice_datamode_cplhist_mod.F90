module dice_datamode_cplhist_mod

  use ESMF             , only : ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
  use ESMF             , only : ESMF_TraceRegionEnter, ESMF_TraceRegionExit
  use ESMF             , only : ESMF_State, ESMF_StateGet, ESMF_Field
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_const_mod    , only : shr_const_TkFrzsw
  use shr_sys_mod      , only : shr_sys_abort
  use dshr_methods_mod , only : dshr_state_getfldptr, dshr_fldbun_getfldptr, chkerr
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
  use dshr_mod         , only : dshr_restart_read, dshr_restart_write
  use dshr_strdata_mod , only : shr_strdata_type
  use dshr_dfield_mod  , only : dfield_type, dshr_dfield_add, dshr_dfield_copy

  implicit none
  private ! except

  public  :: dice_datamode_cplhist_advertise
  public  :: dice_datamode_cplhist_init_pointers
  public  :: dice_datamode_cplhist_advance
  public  :: dice_datamode_cplhist_restart_read
  public  :: dice_datamode_cplhist_restart_write

  ! export fields
  ! ice to atm in CMEPS/mediator/esmFldsExchange_ufs_mod.F90
  real(r8), pointer :: Si_ifrac(:)  => null()
  real(r8), pointer :: Si_imask(:)  => null()
  real(r8), pointer :: Faii_taux(:) => null()
  real(r8), pointer :: Faii_tauy(:) => null()
  real(r8), pointer :: Faii_lat(:)  => null()
  real(r8), pointer :: Faii_sen(:)  => null()
  real(r8), pointer :: Faii_lwup(:) => null()
  real(r8), pointer :: Faii_evap(:) => null()
  real(r8), pointer :: Si_vice(:)   => null()
  real(r8), pointer :: Si_vsno(:)   => null()
  real(r8), pointer :: Si_t(:)      => null()
  real(r8), pointer :: Si_avsdr(:)  => null()
  real(r8), pointer :: Si_avsdf(:)  => null()
  real(r8), pointer :: Si_anidr(:)  => null()
  real(r8), pointer :: Si_anidf(:)  => null()

  type(dfield_type)  , pointer :: dfields => null()

  character(*) , parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine dice_datamode_cplhist_advertise(exportState, fldsexport, flds_scalar_name, rc)

    ! input/output variables
    type(esmf_State)   , intent(inout) :: exportState
    type(fldlist_type) , pointer       :: fldsexport
    character(len=*)   , intent(in)    :: flds_scalar_name
    integer            , intent(out)   :: rc

    ! local variables
    type(fldlist_type), pointer :: fldList
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Advertise export fields
    call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
    call dshr_fldList_add(fldsExport, 'Si_ifrac'            )
    call dshr_fldList_add(fldsExport, 'Si_imask'            )
    call dshr_fldList_add(fldsExport, 'Faii_taux'           )
    call dshr_fldList_add(fldsExport, 'Faii_tauy'           )
    call dshr_fldList_add(fldsExport, 'Faii_lat'            )
    call dshr_fldList_add(fldsExport, 'Faii_sen'            )
    call dshr_fldList_add(fldsExport, 'Faii_lwup'           )
    call dshr_fldList_add(fldsExport, 'Faii_evap'           )
    call dshr_fldList_add(fldsExport, 'Si_vice'             )
    call dshr_fldList_add(fldsExport, 'Si_vsno'             )
    call dshr_fldList_add(fldsExport, 'Si_t'                )
    call dshr_fldList_add(fldsExport, 'Si_avsdr'            )
    call dshr_fldList_add(fldsExport, 'Si_avsdf'            )
    call dshr_fldList_add(fldsExport, 'Si_anidr'            )
    call dshr_fldList_add(fldsExport, 'Si_anidf'            )

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(dice_comp_advertise): Fr_ice'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine dice_datamode_cplhist_advertise

  !===============================================================================
  subroutine dice_datamode_cplhist_init_pointers(importState, exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: importState
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    integer                :: n
    type(ESMF_Field)       :: lfield
    character(CL) ,pointer :: lfieldnamelist(:) => null()
    integer                :: fieldcount
    character(len=*), parameter :: subname='(dice_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Initialize dfields data type (to map streams to export state
    ! fields) Create dfields linked list - used for copying stream
    ! fields with ungridded dimensions to export state fields
    call ESMF_StateGet(exportState, itemCount=fieldCount, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    allocate(lfieldnamelist(fieldCount))
    call ESMF_StateGet(exportState, itemNameList=lfieldnamelist, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    do n = 1, fieldCount
       call ESMF_StateGet(exportState, itemName=trim(lfieldNameList(n)), field=lfield, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       if (trim(lfieldnamelist(n)) /= flds_scalar_name) then
          call dshr_dfield_add( dfields, sdat, trim(lfieldnamelist(n)), trim(lfieldnamelist(n)), &
               exportState, logunit, mainproc, rc)
          if (chkerr(rc,__LINE__,u_FILE_u)) return
       end if
    end do
    
    ! initialize pointers to export fields
    call dshr_state_getfldptr(exportState,'Si_ifrac',fldptr1=Si_ifrac, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState,'Si_imask', fldptr1=Si_imask, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState,'Faii_taux', fldptr1=Faii_taux, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faii_tauy', fldptr1=Faii_tauy, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faii_lat', fldptr1=Faii_lat, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faii_sen', fldptr1=Faii_sen, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faii_lwup', fldptr1=Faii_lwup, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faii_evap', fldptr1=Faii_evap, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Si_vice', fldptr1=Si_vice, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Si_vsno', fldptr1=Si_vsno, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Si_t', fldptr1=Si_t, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Si_avsdr', fldptr1=Si_avsdr, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Si_avsdf', fldptr1=Si_avsdf, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Si_anidr', fldptr1=Si_anidr, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Si_anidf', fldptr1=Si_anidf, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    !Initialize (e.g., =0)?

  end subroutine dice_datamode_cplhist_init_pointers

  !===============================================================================
  subroutine dice_datamode_cplhist_advance(sdat, rc)

    ! input/output variables
    type(shr_strdata_type) , intent(in)    :: sdat
    integer,                 intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(dice_datamode_cplhist_advance): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Automatically update the fields in the export state
    call ESMF_TraceRegionEnter('dice_dfield_copy')
    call dshr_dfield_copy(dfields, sdat, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TraceRegionExit('dice_dfield_copy')

    ! Unit conversions, calculations,....  Where aice=0, Si_t=0K (as
    ! missing value). Interpolation in time between ice that comes or
    ! goes then has issues
    where (Si_t < 10) Si_t = shr_const_TkFrzsw

  end subroutine dice_datamode_cplhist_advance

  !===============================================================================
  subroutine dice_datamode_cplhist_restart_write(rpfile, case_name, inst_suffix, ymd, tod, &
       logunit, my_task, sdat)

    ! input/output variables
    character(len=*)            , intent(in)    :: rpfile
    character(len=*)            , intent(in)    :: case_name
    character(len=*)            , intent(in)    :: inst_suffix
    integer                     , intent(in)    :: ymd       ! model date
    integer                     , intent(in)    :: tod       ! model sec into model date
    integer                     , intent(in)    :: logunit
    integer                     , intent(in)    :: my_task
    type(shr_strdata_type)      , intent(inout) :: sdat

    ! local variables
    integer :: rc
    !-------------------------------------------------------------------------------

    call dshr_restart_write(rpfile, case_name, 'dice', inst_suffix, ymd, tod, &
         logunit, my_task, sdat, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine dice_datamode_cplhist_restart_write

  !===============================================================================
  subroutine dice_datamode_cplhist_restart_read(rest_filem, rpfile, logunit, my_task, mpicom, sdat)

    ! input/output arguments
    character(len=*)            , intent(inout) :: rest_filem
    character(len=*)            , intent(inout) :: rpfile
    integer                     , intent(in)    :: logunit
    integer                     , intent(in)    :: my_task
    integer                     , intent(in)    :: mpicom
    type(shr_strdata_type)      , intent(inout) :: sdat

    ! local variables
    integer :: rc
    !-------------------------------------------------------------------------------

    call dshr_restart_read(rest_filem, rpfile, logunit, my_task, mpicom, sdat, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine dice_datamode_cplhist_restart_read

end module dice_datamode_cplhist_mod
