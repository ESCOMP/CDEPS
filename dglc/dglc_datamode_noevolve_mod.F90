module dglc_datamode_noevolve_mod

  use ESMF             , only : ESMF_State, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_sys_mod      , only : shr_sys_abort
  use dshr_methods_mod , only : dshr_state_getfldptr, dshr_fldbun_getfldptr, chkerr
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
  use dshr_mod         , only : dshr_restart_read, dshr_restart_write
  use dshr_strdata_mod , only : shr_strdata_type

  implicit none
  private ! except

  public  :: dglc_datamode_noevolve_advertise
  public  :: dglc_datamode_noevolve_init_pointers
  public  :: dglc_datamode_noevolve_advance
  public  :: dglc_datamode_noevolve_restart_read
  public  :: dglc_datamode_noevolve_restart_write

  integer :: num_icesheets

  ! export fields
  type icesheet_ptr_t
     real(r8), pointer :: ptr => null() ! pointer to array
  endtype icesheet_ptr_t
  type(icesheet_ptr_t), allocatable :: Sg_area(:)
  type(icesheet_ptr_t), allocatable :: Sg_ice_covered(:)
  type(icesheet_ptr_t), allocatable :: Sg_topo(:)
  type(icesheet_ptr_t), allocatable :: Sg_icemask(:)

  character(len=*), parameter :: field_in_tsrf                    = 'Sl_tsrf'
  character(len=*), parameter :: field_in_qice                    = 'Flgl_qice'

  character(len=*), parameter :: field_out_area                   = 'Sg_area'
  character(len=*), parameter :: field_out_ice_covered            = 'Sg_ice_covered'
  character(len=*), parameter :: field_out_topo                   = 'Sg_topo'
  character(len=*), parameter :: field_out_icemask                = 'Sg_icemask'
  character(len=*), parameter :: field_out_icemask_coupled_fluxes = 'Sg_icemask_coupled_fluxes'

  character(len=*), parameter :: field_out_hflx_to_lnd            = 'Flgg_hflx'
  character(len=*), parameter :: field_out_rofi_to_ice            = 'Figg_rofi'
  character(len=*), parameter :: field_out_rofi_to_ocn            = 'Fogg_rofi'
  character(len=*), parameter :: field_out_rofl_to_ocn            = 'Fogg_rofl'

  character(*) , parameter :: nullstr = 'null'
  character(*) , parameter :: rpfile  = 'rpointer.glc'
  character(*) , parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

   subroutine dglc_datamode_noevolve_advertise(NStateExp, fldsexport, flds_scalar_name, rc)

    ! input/output variables
    type(esmf_State)   , intent(inout) :: NstateExp(num_icesheets_in)
    type(fldlist_type) , pointer       :: fldsexport
    character(len=*)   , intent(in)    :: flds_scalar_name
    integer            , intent(out)   :: rc

    ! local variables
    integer             :: nf,ns
    character(len=CS)   :: cnum
    character(len=CL)   :: cvalue
    character(len=CL)   :: logmsg
    logical             :: isPresent, isSet
    type(fldlist_type), pointer :: fldList
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    !--------------------------------
    ! Create nested state for active ice sheets only
    !--------------------------------

    num_icesheets = size(NStateExp)

    do ns = 1,num_icesheets
       write(cnum,'(i0)') ns
       call NUOPC_AddNestedState(importState, CplSet="GLC"//trim(cnum), nestedState=NStateImp(ns), rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call NUOPC_AddNestedState(exportState, CplSet="GLC"//trim(cnum), nestedState=NStateExp(ns), rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end do

    ! Advertise export fields
    call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
    call dshr_fldList_add(fldsExport, field_out_ice_covered)
    call dshr_fldList_add(fldsExport, field_out_topo)
    call dshr_fldList_add(fldsExport, field_out_icemask)
    call dshr_fldList_add(fldsExport, field_out_icemask_coupled_fluxes)

    do ns = 1,num_icesheets
       fldlist => fldsExport ! the head of the linked list
       do while (associated(fldlist))
          call NUOPC_Advertise(NStateExp(ns), standardName=fldlist%stdname, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          if (ns == 1) then
             call ESMF_LogWrite('(dglc_comp_advertise): Fr_glc'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
          end if
          fldList => fldList%next
       end do
    enddo

  end subroutine dglc_datamode_noevolve_advertise

  !===============================================================================
  subroutine dglc_datamode_noevolve_init_pointers(NStateExport, rc)

    ! input/output variables
    type(ESMF_State) , intent(inout) :: NStateExport(:)
    integer          , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(dglc_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! So this is tricky since you need pointers to fields in the nested state
    ! So this will have to be done below in a loop

    ! initialize pointers to export fields
    allocate(Sg_area(num_icesheets))
    allocate(Sg_topo(num_icesheets))
    allocate(Sg_ice_covered(num_icesheets))
    allocate(Sg_icemask(num_icesheets))

    do ns = 1,num_icesheets
       call dshr_state_getfldptr(NStateExport, 'Sg_topo', fldptr1=Sg_topo(ns)%ptr, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
    end do

  end subroutine dglc_datamode_noevolve_init_pointers

  !===============================================================================
  subroutine dglc_datamode_noevolve_advance(sdat, mesh, fileName_data, thickness, topo, rc)

     ! Assume that the model mesh is the same as the input data mesh

    ! input/output variables
    integer, intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(dglc_datamode_noevolve_advance): '
    !-------------------------------------------------------------------------------

    ! input/output variables
    type(shr_strdata_type) , intent(in)  :: sdat
    type(ESMF_Mesh)        , intent(in)  :: mesh          ! mesh read in from fileName_mesh
    character(*)           , intent(in)  :: fileName_data ! file name string
    real(R8)               , pointer     :: thickness(:)  ! ice thickness
    real(R8)               , pointer     :: topo(:)       ! topography
    integer                , intent(out) :: rc

    ! local variables
    type(ESMF_DistGrid)    :: distgrid
    type(ESMF_FieldBundle) :: fldbun_src
    type(ESMF_Field)       :: field_src
    integer                :: lsize
    integer, pointer       :: gindex(:) ! domain decomposition of data
    integer                :: ndims     ! number of dims
    integer, allocatable   :: dimid(:)
    type(var_desc_t)       :: varid
    type(file_desc_t)      :: pioid
    type(io_desc_t)        :: pio_iodesc
    integer                :: rcode
    integer                :: nxg, nyg
    real(r8), pointer      :: data(:)
    integer                :: srcTermProcessing_Value = 0
    character(*) ,parameter :: subName =  '(datm_get_adjustment_factors) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Create input
    fldbun_src = ESMF_FieldBundleCreate(rc=rc) ! input field bundle

    ! "ice thickness" ;
    field_src = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8, name='thk', meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldBundleAdd(fldbun_src, (/field_src/), rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! "bed topography" ;
    field_src = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8, name='topg', meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldBundleAdd(fldbun_src, (/field_src/), rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! "area"
    field_src = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8, name='area', meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldBundleAdd(fldbun_src, (/field_src/), rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Get mesh info
    call ESMF_MeshGet(mesh, elementdistGrid=distGrid, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_DistGridGet(distGrid, localDe=0, elementCount=lsize, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    allocate(gindex(lsize))
    call ESMF_DistGridGet(distGrid, localDe=0, seqIndexList=gindex, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Create_pio_iodesc
    rcode = pio_openfile(sdat%pio_subsystem, pioid, sdat%io_type, trim(filename_data), pio_nowrite)
    call pio_seterrorhandling(pioid, PIO_BCAST_ERROR)
    rcode = pio_inq_varid(pioid, 'thk', varid)
    rcode = pio_inq_varndims(pioid, varid, ndims)
    allocate(dimid(ndims))
    rcode = pio_inq_varid(pioid, 'topg', varid)
    rcode = pio_inq_vardimid(pioid, varid, dimid(1:ndims))
    rcode = pio_inq_dimlen(pioid, dimid(1), nxg)
    rcode = pio_inq_dimlen(pioid, dimid(2), nyg)
    call pio_initdecomp(sdat%pio_subsystem, pio_double, (/nxg,nyg/), gindex, pio_iodesc)
    deallocate(gindex)

    ! Read in the data into the appropriate field bundle pointers
    call dshr_fldbun_getFldPtr(fldbun_src, 'thk', data, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    rcode = pio_inq_varid(pioid, 'thk', varid)
    call pio_read_darray(pioid, varid, pio_iodesc, data, rcode)

    call dshr_fldbun_getFldPtr(fldbun_src, 'topg', data, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    rcode = pio_inq_varid(pioid, 'topg', varid)
    call pio_read_darray(pioid, varid, pio_iodesc, data, rcode)

    call pio_closefile(pioid)
    call pio_freedecomp(sdat%pio_subsystem, pio_iodesc)
    call ESMF_RouteHandleDestroy(route_handle, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return






  end subroutine dglc_datamode_noevolve_advance

  !===============================================================================
  subroutine dglc_datamode_noevolve_restart_write(case_name, inst_suffix, ymd, tod, &
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

    call dshr_restart_write(rpfile, case_name, 'dglc', inst_suffix, ymd, tod, &
         logunit, my_task, sdat)

  end subroutine dglc_datamode_noevolve_restart_write

  !===============================================================================
  subroutine dglc_datamode_noevolve_restart_read(rest_filem, inst_suffix, logunit, my_task, mpicom, sdat)

    ! input/output arguments
    character(len=*)            , intent(inout) :: rest_filem
    character(len=*)            , intent(in)    :: inst_suffix
    integer                     , intent(in)    :: logunit
    integer                     , intent(in)    :: my_task
    integer                     , intent(in)    :: mpicom
    type(shr_strdata_type)      , intent(inout) :: sdat
    !-------------------------------------------------------------------------------

    call dshr_restart_read(rest_filem, rpfile, inst_suffix, nullstr, logunit, my_task, mpicom, sdat)

  end subroutine dglc_datamode_noevolve_restart_read

end module dglc_datamode_noevolve_mod
