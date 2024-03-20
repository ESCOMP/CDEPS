module dglc_datamode_noevolve_mod

   use ESMF             , only : ESMF_State, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
   use ESMF             , only : ESMF_Mesh, ESMF_DistGrid, ESMF_FieldBundle, ESMF_Field
   use ESMF             , only : ESMF_FieldBundleCreate, ESMF_FieldCreate, ESMF_MeshLoc_Element
   use ESMF             , only : ESMF_FieldBundleAdd, ESMF_MeshGet, ESMF_DistGridGet, ESMF_Typekind_R8
   use NUOPC            , only : NUOPC_Advertise, NUOPC_AddNestedState
   use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
   use shr_sys_mod      , only : shr_sys_abort
   use dshr_methods_mod , only : dshr_state_getfldptr, dshr_fldbun_getfldptr, chkerr
   use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
   use dshr_strdata_mod , only : shr_strdata_type
   use pio              , only : file_desc_t, io_desc_t, var_desc_t, iosystem_desc_t
   use pio              , only : pio_openfile, pio_inq_varid, pio_inq_varndims, pio_inq_vardimid
   use pio              , only : pio_inq_dimlen, pio_initdecomp, pio_read_darray, pio_double
   use pio              , only : pio_closefile, pio_freedecomp, PIO_BCAST_ERROR, PIO_NOWRITE
   use pio              , only : pio_seterrorhandling

   implicit none
   private ! except

   public  :: dglc_datamode_noevolve_advertise
   public  :: dglc_datamode_noevolve_init_pointers
   public  :: dglc_datamode_noevolve_advance

   integer :: num_icesheets

   ! export fields
   type icesheet_ptr_t
      real(r8), pointer :: ptr(:) => null() ! pointer to array
   endtype icesheet_ptr_t
   type(icesheet_ptr_t), allocatable :: Sg_area(:)
   type(icesheet_ptr_t), allocatable :: Sg_topo(:)
   type(icesheet_ptr_t), allocatable :: Sg_ice_covered(:)
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
      type(ESMF_State)   , intent(inout) :: NStateExp(:)
      type(fldlist_type) , pointer       :: fldsexport
      character(len=*)   , intent(in)    :: flds_scalar_name
      integer            , intent(out)   :: rc

      ! local variables
      integer :: ns
      logical :: isPresent, isSet
      character(len=CS) :: cnum
      type(fldlist_type), pointer :: fldList
      !-------------------------------------------------------------------------------

      rc = ESMF_SUCCESS

      !--------------------------------
      ! Create nested state for active ice sheets only
      !--------------------------------

      ! Set module variable for number of ice sheets
      num_icesheets = size(NStateExp)

      ! Advertise export fields
      call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
      call dshr_fldList_add(fldsExport, field_out_ice_covered)
      call dshr_fldList_add(fldsExport, field_out_topo)
      call dshr_fldList_add(fldsExport, field_out_icemask)
      call dshr_fldList_add(fldsExport, field_out_icemask_coupled_fluxes)

      do ns = 1,num_icesheets
        write(cnum,'(i0)') ns
        fldlist => fldsExport ! the head of the linked list
        do while (associated(fldlist))
          call NUOPC_Advertise(NStateExp(ns), standardName=fldlist%stdname, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          call ESMF_LogWrite('(dglc_comp_advertise): Fr_glc'//trim(cnum)//"_"//trim(fldList%stdname), ESMF_LOGMSG_INFO)
          fldList => fldList%next
        end do
      enddo

   end subroutine dglc_datamode_noevolve_advertise

   !===============================================================================
   subroutine dglc_datamode_noevolve_init_pointers(NStateExp, rc)

      ! input/output variables
      type(ESMF_State) , intent(inout) :: NStateExp(:)
      integer          , intent(out)   :: rc

      ! local variables
      integer :: ns
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
         call dshr_state_getfldptr(NStateExp(ns), 'Sg_topo'        , fldptr1=Sg_topo(ns)%ptr        , rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return

         call dshr_state_getfldptr(NStateExp(ns), 'Sg_ice_covered' , fldptr1=Sg_ice_covered(ns)%ptr , rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return

         call dshr_state_getfldptr(NStateExp(ns), 'Sg_icemask'     , fldptr1=Sg_icemask(ns)%ptr     , rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
      end do

   end subroutine dglc_datamode_noevolve_init_pointers

   !===============================================================================
   subroutine dglc_datamode_noevolve_advance(pio_subsystem, io_type, io_format, &
        model_meshes, input_files, rc)

      ! Assume that the model mesh is the same as the input data mesh

      ! input/output variables
      type(iosystem_desc_t) , pointer     :: pio_subsystem   ! pio info
      integer               , intent(in)  :: io_type         ! pio info
      integer               , intent(in)  :: io_format       ! pio info
      type(ESMF_Mesh)       , intent(in)  :: model_meshes(:) ! ice sheets meshes
      character(len=*)      , intent(in)  :: input_files(:)  ! input file names
      integer               , intent(out) :: rc

      ! local variables
      type(ESMF_FieldBundle) :: fldbun_noevolve
      type(ESMF_DistGrid)    :: distgrid
      type(ESMF_Field)       :: field_noevolve
      type(file_desc_t)      :: pioid
      type(io_desc_t)        :: pio_iodesc
      integer                :: ns  ! ice sheet index
      integer                :: lsize
      integer, pointer       :: gindex(:) ! domain decomposition of data
      integer                :: ndims     ! number of dims
      integer, allocatable   :: dimid(:)
      type(var_desc_t)       :: varid
      integer                :: rcode
      integer                :: nxg, nyg
      real(r8), pointer      :: data(:)
      character(len=*), parameter :: subname='(dglc_datamode_noevolve_advance): '
      !-------------------------------------------------------------------------------

      rc = ESMF_SUCCESS

      do ns = 1,num_icesheets

        ! Create module level field bundle
        fldbun_noevolve = ESMF_FieldBundleCreate(rc=rc) ! input field bundle

        ! "ice thickness" ;
        field_noevolve = ESMF_FieldCreate(model_meshes(ns), ESMF_TYPEKIND_R8, name='thk', meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return
        call ESMF_FieldBundleAdd(fldbun_noevolve, (/field_noevolve/), rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return

        ! "bed topography" ;
        field_noevolve = ESMF_FieldCreate(model_meshes(ns), ESMF_TYPEKIND_R8, name='topg', meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return
        call ESMF_FieldBundleAdd(fldbun_noevolve, (/field_noevolve/), rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return

        ! Get mesh info
        call ESMF_MeshGet(model_meshes(ns), elementdistGrid=distGrid, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        call ESMF_DistGridGet(distGrid, localDe=0, elementCount=lsize, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        allocate(gindex(lsize))
        call ESMF_DistGridGet(distGrid, localDe=0, seqIndexList=gindex, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        ! Create pioid and pio_iodesc at the module level
        rcode = pio_openfile(pio_subsystem, pioid, io_type, trim(input_files(ns)), pio_nowrite)
        call pio_seterrorhandling(pioid, PIO_BCAST_ERROR)
        rcode = pio_inq_varid(pioid, 'thk', varid)
        rcode = pio_inq_varndims(pioid, varid, ndims)
        allocate(dimid(ndims))
        rcode = pio_inq_vardimid(pioid, varid, dimid(1:ndims))
        rcode = pio_inq_dimlen(pioid, dimid(1), nxg)
        rcode = pio_inq_dimlen(pioid, dimid(2), nyg)
        call pio_initdecomp(pio_subsystem, pio_double, (/nxg,nyg/), gindex, pio_iodesc)
        deallocate(gindex)

        ! Read in the data into the appropriate field bundle pointers
        ! Note that Sg_ice_covered(ns)%ptr points into the data for
        ! the Sg_ice_covered field in NStateExp(ns)
        ! Note that Sg_topo(ns)%ptr points into the data for
        ! the Sg_topon NStateExp(ns)

        call dshr_fldbun_getFldPtr(fldbun_noevolve, 'thk', data, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        rcode = pio_inq_varid(pioid, 'thk', varid)
        call pio_read_darray(pioid, varid, pio_iodesc, data, rcode)
        Sg_ice_covered(ns)%ptr(:) = data(:)

        call dshr_fldbun_getFldPtr(fldbun_noevolve, 'topg', data, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        rcode = pio_inq_varid(pioid, 'topg', varid)
        call pio_read_darray(pioid, varid, pio_iodesc, data, rcode)
        Sg_topo(ns)%ptr(:) = data(:)

        call pio_closefile(pioid)
        call pio_freedecomp(pio_subsystem, pio_iodesc)

      end do ! end loop over ice sheets

   end subroutine dglc_datamode_noevolve_advance

end module dglc_datamode_noevolve_mod
