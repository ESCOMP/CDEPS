module dglc_datamode_noevolve_mod

   use ESMF             , only : ESMF_State, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
   use ESMF             , only : ESMF_Mesh, ESMF_DistGrid, ESMF_FieldBundle, ESMF_Field
   use ESMF             , only : ESMF_FieldBundleCreate, ESMF_FieldCreate, ESMF_MeshLoc_Element
   use ESMF             , only : ESMF_FieldBundleAdd, ESMF_MeshGet, ESMF_DistGridGet, ESMF_Typekind_R8
   use NUOPC            , only : NUOPC_Advertise, NUOPC_IsConnected
   use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
   use shr_sys_mod      , only : shr_sys_abort
   use shr_const_mod    , only : SHR_CONST_RHOICE, SHR_CONST_RHOSW, SHR_CONST_REARTH
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

   logical  :: initialized_noevolve = .false.
   integer  :: num_icesheets
   real(r8) :: thk0 = 1._r8

   ! Data structure to enable multiple ice sheets
   type icesheet_ptr_t
      real(r8), pointer :: ptr(:) => null() ! pointer to array
   endtype icesheet_ptr_t

   ! Export fields
   type(icesheet_ptr_t), allocatable :: Sg_area(:)
   type(icesheet_ptr_t), allocatable :: Sg_topo(:)
   type(icesheet_ptr_t), allocatable :: Sg_ice_covered(:)
   type(icesheet_ptr_t), allocatable :: Sg_icemask(:)
   type(icesheet_ptr_t), allocatable :: Sg_icemask_coupled_fluxes(:)

   ! Import fields
   type(icesheet_ptr_t), allocatable :: Sl_tsrf(:)
   type(icesheet_ptr_t), allocatable :: Flgl_qice(:)

   ! Export Field names
   character(len=*), parameter :: field_out_area                   = 'Sg_area'
   character(len=*), parameter :: field_out_topo                   = 'Sg_topo'
   character(len=*), parameter :: field_out_ice_covered            = 'Sg_ice_covered'
   character(len=*), parameter :: field_out_icemask                = 'Sg_icemask'
   character(len=*), parameter :: field_out_icemask_coupled_fluxes = 'Sg_icemask_coupled_fluxes'

   ! Import Field names
   character(len=*), parameter :: field_in_tsrf                    = 'Sl_tsrf'
   character(len=*), parameter :: field_in_qice                    = 'Flgl_qice'

   character(*) , parameter :: rpfile  = 'rpointer.glc'
   character(*) , parameter :: u_FILE_u = &
        __FILE__

!===============================================================================
contains
!===============================================================================

   subroutine dglc_datamode_noevolve_advertise(NStateExp, fldsexport, NStateImp, fldsimport, &
        flds_scalar_name, rc)

      ! input/output variables
      type(ESMF_State)  , intent(inout) :: NStateExp(:)
      type(fldlist_type), pointer       :: fldsexport
      type(ESMF_State)  , intent(inout) :: NStateImp(:)
      type(fldlist_type), pointer       :: fldsimport
      character(len=*)  , intent(in)    :: flds_scalar_name
      integer           , intent(out)   :: rc

      ! local variables
      integer                     :: ns
      character(len=CS)           :: cnum
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
      call dshr_fldList_add(fldsExport, field_out_area)
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

      ! Advertise import fields if appropriate
      call dshr_fldList_add(fldsImport, trim(flds_scalar_name))
      call dshr_fldList_add(fldsImport, field_in_tsrf)
      call dshr_fldList_add(fldsImport, field_in_qice)

      do ns = 1,num_icesheets
         write(cnum,'(i0)') ns
         fldlist => fldsImport ! the head of the linked list
         do while (associated(fldlist))
            call NUOPC_Advertise(NStateImp(ns), standardName=fldlist%stdname, rc=rc)
            if (ChkErr(rc,__LINE__,u_FILE_u)) return
            call ESMF_LogWrite('(dglc_comp_advertise): To_glc'//trim(cnum)//"_"//trim(fldList%stdname), ESMF_LOGMSG_INFO)
            fldList => fldList%next
         end do
      enddo

   end subroutine dglc_datamode_noevolve_advertise

   !===============================================================================
   subroutine dglc_datamode_noevolve_init_pointers(NStateExp, NstateImp, rc)

      ! input/output variables
      type(ESMF_State) , intent(inout) :: NStateExp(:)
      type(ESMF_State) , intent(inout) :: NStateImp(:)
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
      allocate(Sg_icemask_coupled_fluxes(num_icesheets))

      do ns = 1,num_icesheets
         call dshr_state_getfldptr(NStateExp(ns), field_out_area, fldptr1=Sg_area(ns)%ptr, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
         call dshr_state_getfldptr(NStateExp(ns), field_out_topo, fldptr1=Sg_topo(ns)%ptr, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
         call dshr_state_getfldptr(NStateExp(ns), field_out_ice_covered, fldptr1=Sg_ice_covered(ns)%ptr, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
         call dshr_state_getfldptr(NStateExp(ns), field_out_icemask, fldptr1=Sg_icemask(ns)%ptr, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
         call dshr_state_getfldptr(NStateExp(ns), field_out_icemask_coupled_fluxes, fldptr1=Sg_icemask_coupled_fluxes(ns)%ptr, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
      end do

      ! initialize pointers to import fields if appropriate
      allocate(Sl_tsrf(num_icesheets))
      allocate(Flgl_qice(num_icesheets))

      do ns = 1,num_icesheets
         ! NOTE: the field is connected ONLY if the MED->GLC entry is in the nuopc.runconfig file
         ! This restriction occurs even if the field was advertised
         if (NUOPC_IsConnected(NStateImp(ns), fieldName=field_in_tsrf)) then
            call dshr_state_getfldptr(NStateImp(ns), field_in_tsrf, fldptr1=Sl_tsrf(ns)%ptr, rc=rc)
            if (chkerr(rc,__LINE__,u_FILE_u)) return
         end if
         if (NUOPC_IsConnected(NStateImp(ns), fieldName=field_in_qice)) then
            call dshr_state_getfldptr(NStateImp(ns), field_in_qice, fldptr1=Flgl_qice(ns)%ptr, rc=rc)
            if (chkerr(rc,__LINE__,u_FILE_u)) return
         end if
      end do

   end subroutine dglc_datamode_noevolve_init_pointers

   !===============================================================================
   subroutine dglc_datamode_noevolve_advance(pio_subsystem, io_type, io_format, &
        model_meshes, model_internal_gridsize, model_datafiles, rc)

      ! Assume that the model mesh is the same as the input data mesh

      ! input/output variables
      type(iosystem_desc_t) , pointer     :: pio_subsystem              ! pio info
      integer               , intent(in)  :: io_type                    ! pio info
      integer               , intent(in)  :: io_format                  ! pio info
      type(ESMF_Mesh)       , intent(in)  :: model_meshes(:)            ! ice sheets meshes
      real(r8)              , intent(in)  :: model_internal_gridsize(:) ! internal model gridsizes (m)
      character(len=*)      , intent(in)  :: model_datafiles(:)         ! input file names
      integer               , intent(out) :: rc

      ! local variables
      type(ESMF_FieldBundle) :: fldbun_noevolve
      type(ESMF_DistGrid)    :: distgrid
      type(ESMF_Field)       :: field_noevolve
      type(file_desc_t)      :: pioid
      type(io_desc_t)        :: pio_iodesc
      integer                :: ns  ! ice sheet index
      integer                :: ng  ! grid cell index
      integer                :: lsize
      integer, pointer       :: gindex(:) ! domain decomposition of data
      integer                :: ndims     ! number of dims
      integer, allocatable   :: dimid(:)
      type(var_desc_t)       :: varid
      integer                :: rcode
      integer                :: nxg, nyg
      real(r8), pointer      :: topog(:)
      real(r8), pointer      :: thck(:)
      logical                :: exists
      real(r8)               :: rhoi   ! density of ice ~ kg/m^3
      real(r8)               :: rhoo   ! density of sea water ~ kg/m^3
      real(r8)               :: eus    ! eustatic sea level
      real(r8), allocatable  :: lsrf(:)
      real(r8), allocatable  :: usrf(:)
      character(len=*), parameter :: subname='(dglc_datamode_noevolve_advance): '
      !-------------------------------------------------------------------------------

      rc = ESMF_SUCCESS

      if (initialized_noevolve) then
         RETURN
      end if

      do ns = 1,num_icesheets

        ! Get mesh info
        call ESMF_MeshGet(model_meshes(ns), elementdistGrid=distGrid, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        call ESMF_DistGridGet(distGrid, localDe=0, elementCount=lsize, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        allocate(gindex(lsize))
        call ESMF_DistGridGet(distGrid, localDe=0, seqIndexList=gindex, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        ! Determine "glc_area" ;
        ! Sg_areas is in radians
        ! SHR_CONST_REARTH is the radius of earth in m
        ! model_internal_gridsize is the internal model gridsize in m
        do ng = 1,lsize
           Sg_area(ns)%ptr(ng) = (model_internal_gridsize(ns)/SHR_CONST_REARTH)**2
        end do

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

        ! Create pioid and pio_iodesc at the module level
        inquire(file=trim(model_datafiles(ns)), exist=exists)
        if (.not.exists) then
           write(6,'(a)')' ERROR: model input file '//trim(model_datafiles(ns))//' does not exist'
           call shr_sys_abort()
        end if
        rcode = pio_openfile(pio_subsystem, pioid, io_type, trim(model_datafiles(ns)), pio_nowrite)
        call pio_seterrorhandling(pioid, PIO_BCAST_ERROR)
        rcode = pio_inq_varid(pioid, 'thk', varid)
        rcode = pio_inq_varndims(pioid, varid, ndims)
        allocate(dimid(ndims))
        rcode = pio_inq_vardimid(pioid, varid, dimid(1:ndims))
        rcode = pio_inq_dimlen(pioid, dimid(1), nxg)
        rcode = pio_inq_dimlen(pioid, dimid(2), nyg)
        call pio_initdecomp(pio_subsystem, pio_double, (/nxg,nyg/), gindex, pio_iodesc)
        deallocate(dimid)
        deallocate(gindex)

        ! Read in the data into the appropriate field bundle pointers
        ! Note that Sg_ice_covered(ns)%ptr points into the data for
        ! the Sg_ice_covered field in NStateExp(ns)
        ! Note that Sg_topo(ns)%ptr points into the data for
        ! the Sg_topon NStateExp(ns)
        ! Note that topog is bedrock topography

        call dshr_fldbun_getFldPtr(fldbun_noevolve, 'topg', topog, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        rcode = pio_inq_varid(pioid, 'topg', varid)
        call pio_read_darray(pioid, varid, pio_iodesc, topog, rcode)

        call dshr_fldbun_getFldPtr(fldbun_noevolve, 'thk', thck, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        rcode = pio_inq_varid(pioid, 'thk', varid)
        call pio_read_darray(pioid, varid, pio_iodesc, thck,  rcode)

        allocate(lsrf(lsize))
        allocate(usrf(lsize))

        rhoi = SHR_CONST_RHOICE   ! 0.917e3
        rhoo = SHR_CONST_RHOSW    ! 1.026e3
        eus = 0
        do ng = 1,lsize
           if (topog(ng) - eus < (-rhoi/rhoo) * thck(ng)) then
              lsrf(ng) = (-rhoi/rhoo) * thck(ng)
           else
              lsrf(ng) = topog(ng)
           end if
           usrf(ng) = max(0.d0, thck(ng) + lsrf(ng))

           if (is_in_active_grid(usrf(ng))) then
              Sg_icemask(ns)%ptr(ng) = 1.d0
              Sg_icemask_coupled_fluxes(ns)%ptr(ng) = 0.d0
              if (is_ice_covered(thck(ng))) then
                 Sg_ice_covered(ns)%ptr(ng) = 1.d0
              else
                 Sg_ice_covered(ns)%ptr(ng) = 0.d0
              end if
              ! Note that we use the same method for computing topo whether this point is
              ! ice-covered or ice-free. This is in contrast to the method for computing
              ! ice-free topo in glint_upscaling_gcm.
              Sg_topo(ns)%ptr(ng) = thk0 * usrf(ng)
           else
              ! Note that this logic implies that if (in theory) we had an ice-covered
              ! point outside the "active grid", it will get classified as ice-free for
              ! these purposes. This mimics the logic currently in glint_upscaling_gcm.
              Sg_icemask(ns)%ptr(ng) = 0.d0
              Sg_icemask_coupled_fluxes(ns)%ptr(ng) = 0.d0
              Sg_ice_covered(ns)%ptr(ng) = 0.d0
              Sg_topo(ns)%ptr(ng) = 0.d0
           end if
        end do

        deallocate(lsrf)
        deallocate(usrf)

        call pio_closefile(pioid)
        call pio_freedecomp(pio_subsystem, pio_iodesc)

      end do ! end loop over ice sheets

      initialized_noevolve = .true.

   end subroutine dglc_datamode_noevolve_advance

   !===============================================================================
   logical function is_in_active_grid(usrf)
      ! Return true if the given point is inside the "active grid". The active grid includes
      ! any point that can receive a positive surface mass balance, which includes any
      ! point classified as land or ice sheet.

      real(r8), intent(in) :: usrf  ! surface elevation (m)

      if (thk0 * usrf > 0.d0) then
         ! points not at sea level are assumed to be land or ice sheet
         is_in_active_grid = .true.
      else
         is_in_active_grid = .false.
      end if
   end function is_in_active_grid

   !===============================================================================
   logical function is_ice_covered(thck)
      ! Return true if the given point is ice-covered

      real(r8), intent(in) :: thck     ! ice thickness (m)
      real(r8), parameter :: min_thck = 0.d0

      if (thk0 * thck > min_thck) then
         is_ice_covered = .true.
      else
         is_ice_covered = .false.
      end if
   end function is_ice_covered

end module dglc_datamode_noevolve_mod
