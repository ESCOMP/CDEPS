module dglc_datamode_noevolve_mod

   use ESMF             , only : ESMF_State, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
   use ESMF             , only : ESMF_Mesh, ESMF_DistGrid, ESMF_FieldBundle, ESMF_Field
   use ESMF             , only : ESMF_FieldBundleCreate, ESMF_FieldCreate, ESMF_MeshLoc_Element
   use ESMF             , only : ESMF_FieldBundleAdd, ESMF_MeshGet, ESMF_DistGridGet, ESMF_Typekind_R8
   use ESMF             , only : ESMF_GridComp, ESMF_GridCompGet
   use ESMF             , only : ESMF_VM, ESMF_VMAllreduce, ESMF_REDUCE_SUM
   use ESMF             , only : ESMF_VMGetCurrent, ESMF_VMBroadCast
   use NUOPC            , only : NUOPC_Advertise, NUOPC_IsConnected
   use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
   use shr_log_mod      , only : shr_log_error
   use shr_const_mod    , only : SHR_CONST_RHOICE, SHR_CONST_RHOSW, SHR_CONST_REARTH, SHR_CONST_TKFRZ
   use shr_const_mod    , only : SHR_CONST_SPVAL
   use shr_cal_mod      , only : shr_cal_datetod2string
   use dshr_methods_mod , only : dshr_state_getfldptr, dshr_fldbun_getfldptr, chkerr
   use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
   use dshr_strdata_mod , only : shr_strdata_type
   use pio              , only : file_desc_t, io_desc_t, var_desc_t, iosystem_desc_t
   use pio              , only : pio_openfile, pio_inq_varid, pio_inq_varndims, pio_inq_vardimid
   use pio              , only : pio_inq_dimlen, pio_initdecomp, pio_read_darray, pio_double
   use pio              , only : pio_closefile, pio_freedecomp, PIO_BCAST_ERROR, PIO_NOWRITE, PIO_CLOBBER
   use pio              , only : pio_createfile, pio_def_dim, pio_def_var, pio_put_att, pio_fill
   use pio              , only : pio_set_fill, pio_put_att, pio_enddef, pio_write_darray, PIO_GLOBAL
   use pio              , only : pio_seterrorhandling

   implicit none
   private ! except

   public  :: dglc_datamode_noevolve_advertise
   public  :: dglc_datamode_noevolve_init_pointers
   public  :: dglc_datamode_noevolve_advance
   public  :: dglc_datamode_noevolve_restart_write
   public  :: dglc_datamode_noevolve_restart_read

   logical  :: initialized_noevolve = .false.
   integer  :: num_icesheets
   real(r8) :: thk0 = 1._r8

   ! Data structure to enable multiple ice sheets
   type icesheet_ptr_t
      real(r8), pointer :: ptr(:) => null()     ! pointer to array
      real(r8), pointer :: ptr2d(:,:) => null() ! pointer to 2d array
   endtype icesheet_ptr_t

   ! Export fields
   type(icesheet_ptr_t), allocatable :: Sg_area(:)
   type(icesheet_ptr_t), allocatable :: Sg_topo(:)
   type(icesheet_ptr_t), allocatable :: Sg_ice_covered(:)
   type(icesheet_ptr_t), allocatable :: Sg_icemask(:)
   type(icesheet_ptr_t), allocatable :: Sg_icemask_coupled_fluxes(:)
   type(icesheet_ptr_t), allocatable :: Fgrg_rofi(:)

   ! Import fields
   integer, parameter :: nlev_import = 30
   type(icesheet_ptr_t), allocatable :: Sl_tsrf(:)
   type(icesheet_ptr_t), allocatable :: Flgl_qice(:)
   ! type(icesheet_ptr_t), allocatable :: So_t(:)
   ! type(icesheet_ptr_t), allocatable :: So_q(:)

   ! Export Field names
   character(len=*), parameter :: field_out_area                   = 'Sg_area'
   character(len=*), parameter :: field_out_topo                   = 'Sg_topo'
   character(len=*), parameter :: field_out_ice_covered            = 'Sg_ice_covered'
   character(len=*), parameter :: field_out_icemask                = 'Sg_icemask'
   character(len=*), parameter :: field_out_icemask_coupled_fluxes = 'Sg_icemask_coupled_fluxes'
   character(len=*), parameter :: field_out_rofi                   = 'Fgrg_rofi'

   ! Import Field names
   character(len=*), parameter :: field_in_tsrf                    = 'Sl_tsrf'
   character(len=*), parameter :: field_in_qice                    = 'Flgl_qice'
   character(len=*), parameter :: field_in_so_t_depth              = 'So_t_depth'
   character(len=*), parameter :: field_in_so_s_depth              = 'So_s_depth'

   character(*) , parameter :: nullstr = 'null'
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
      call dshr_fldList_add(fldsExport, field_out_rofi)

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
      ! TODO: Add namelist for this
      call dshr_fldList_add(fldsImport, field_in_so_t_depth, ungridded_lbound=1, ungridded_ubound=nlev_import)
      call dshr_fldList_add(fldsImport, field_in_so_s_depth, ungridded_lbound=1, ungridded_ubound=nlev_import)

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
      allocate(Fgrg_rofi(num_icesheets))

      do ns = 1,num_icesheets
         call dshr_state_getfldptr(NStateExp(ns), field_out_area, &
              fldptr1=Sg_area(ns)%ptr, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
         call dshr_state_getfldptr(NStateExp(ns), field_out_topo, &
              fldptr1=Sg_topo(ns)%ptr, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
         call dshr_state_getfldptr(NStateExp(ns), field_out_ice_covered, &
              fldptr1=Sg_ice_covered(ns)%ptr, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
         call dshr_state_getfldptr(NStateExp(ns), field_out_icemask, &
              fldptr1=Sg_icemask(ns)%ptr, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
         call dshr_state_getfldptr(NStateExp(ns), field_out_icemask_coupled_fluxes, &
              fldptr1=Sg_icemask_coupled_fluxes(ns)%ptr, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
         call dshr_state_getfldptr(NStateExp(ns), field_out_rofi, &
              fldptr1=Fgrg_rofi(ns)%ptr, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return

         Fgrg_rofi(ns)%ptr(:) = 0._r8
      end do

      ! initialize pointers to import fields if appropriate
      allocate(Sl_tsrf(num_icesheets))
      allocate(Flgl_qice(num_icesheets))

      do ns = 1,num_icesheets
         if (.not. NUOPC_IsConnected(NStateImp(ns), fieldName=field_in_tsrf)) then
            ! NOTE: the field is connected ONLY if the MED->GLC entry is in the nuopc.runconfig file
            ! This restriction occurs even if the field was advertised
            call shr_log_error(trim(subname)//": MED->GLC must appear in run sequence", rc=rc)
            return
         end if
         call dshr_state_getfldptr(NStateImp(ns), field_in_tsrf, fldptr1=Sl_tsrf(ns)%ptr, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
         call dshr_state_getfldptr(NStateImp(ns), field_in_qice, fldptr1=Flgl_qice(ns)%ptr, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return

         Sl_tsrf(ns)%ptr(:) = SHR_CONST_TKFRZ
         Flgl_qice(ns)%ptr(:) = 0._r8
      end do

   end subroutine dglc_datamode_noevolve_init_pointers

   !===============================================================================
   subroutine dglc_datamode_noevolve_advance(gcomp, pio_subsystem, io_type, io_format, &
        logunit, model_meshes, model_internal_gridsize, model_datafiles, rc)

      ! Assume that the model mesh is the same as the input data mesh

     ! input/output variables
      type(ESMF_GridComp)                 :: gcomp
      type(iosystem_desc_t) , pointer     :: pio_subsystem              ! pio info
      integer               , intent(in)  :: io_type                    ! pio info
      integer               , intent(in)  :: io_format                  ! pio info
      integer               , intent(in)  :: logunit                    ! For writing logs
      type(ESMF_Mesh)       , intent(in)  :: model_meshes(:)            ! ice sheets meshes
      real(r8)              , intent(in)  :: model_internal_gridsize(:) ! internal model gridsizes (m)
      character(len=*)      , intent(in)  :: model_datafiles(:)         ! input file names
      integer               , intent(out) :: rc

      ! local variables
      type(ESMF_FieldBundle) :: fldbun_noevolve
      type(ESMF_DistGrid)    :: distgrid
      type(ESMF_Field)       :: field_noevolve
      type(ESMF_VM)          :: vm
      type(file_desc_t)      :: pioid
      type(io_desc_t)        :: pio_iodesc
      integer                :: ns        ! ice sheet index
      integer                :: ng        ! grid cell index
      integer                :: lsize     ! local size
      integer, pointer       :: gindex(:) ! domain decomposition of data
      integer                :: ndims     ! number of dims
      integer, allocatable   :: dimid(:)
      type(var_desc_t)       :: varid
      integer                :: rcode
      integer                :: nxg, nyg
      real(r8), pointer      :: topog(:)
      real(r8), pointer      :: thck(:)
      logical                :: exists
      real(r8)               :: rhoi      ! density of ice ~ kg/m^3
      real(r8)               :: rhoo      ! density of sea water ~ kg/m^3
      real(r8)               :: eus       ! eustatic sea level
      real(r8)               :: lsrf      ! lower surface elevation (m) on ice grid
      real(r8)               :: usrf      ! upper surface elevation (m) on ice grid
      real(r8)               :: loc_pos_smb(1), Tot_pos_smb(1) ! Sum of positive smb values on each ice sheet for hole-filling
      real(r8)               :: loc_neg_smb(1), Tot_neg_smb(1) ! Sum of negative smb values on each ice sheet for hole-filling
      real(r8)               :: rat     ! Ratio of hole-filling flux to apply

      character(len=*), parameter :: subname='(dglc_datamode_noevolve_advance): '
      !-------------------------------------------------------------------------------

      rc = ESMF_SUCCESS

      if (.not. initialized_noevolve) then

         ! Loop over ice sheets
         do ns = 1,num_icesheets

            ! Determine lsize and gindex
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
            field_noevolve = ESMF_FieldCreate(model_meshes(ns), ESMF_TYPEKIND_R8, &
                 name='thk', meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
            if (chkerr(rc,__LINE__,u_FILE_u)) return
            call ESMF_FieldBundleAdd(fldbun_noevolve, (/field_noevolve/), rc=rc)
            if (chkerr(rc,__LINE__,u_FILE_u)) return

            ! "bed topography" ;
            field_noevolve = ESMF_FieldCreate(model_meshes(ns), ESMF_TYPEKIND_R8, &
                 name='topg', meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
            if (chkerr(rc,__LINE__,u_FILE_u)) return
            call ESMF_FieldBundleAdd(fldbun_noevolve, (/field_noevolve/), rc=rc)
            if (chkerr(rc,__LINE__,u_FILE_u)) return

            ! Create pioid and pio_iodesc at the module level
            inquire(file=trim(model_datafiles(ns)), exist=exists)
            if (.not.exists) then
               call shr_log_error(' ERROR: model input file '//trim(model_datafiles(ns))//' does not exist', rc=rc)
               return
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

            rhoi = SHR_CONST_RHOICE   ! 0.917e3
            rhoo = SHR_CONST_RHOSW    ! 1.026e3
            eus = 0
            do ng = 1,lsize
               if (topog(ng) - eus < (-rhoi/rhoo) * thck(ng)) then
                  lsrf = (-rhoi/rhoo) * thck(ng)
               else
                  lsrf = topog(ng)
               end if
               usrf = max(0.d0, thck(ng) + lsrf)

               ! The export field 'ice_mask_coupled_fluxes' determines who is handling the
               ! runoff associated with the surface mass balance
               ! If its 0 -then ctsm needs to handle it.
               ! Since we want dglc to handle it no evolve mode - then
               ! ice_mask_coupled_fluxes to be identical to the mask

               if (is_in_active_grid(usrf)) then
                  Sg_icemask(ns)%ptr(ng) = 1.d0
                  Sg_icemask_coupled_fluxes(ns)%ptr(ng) = 1.d0
                  if (is_ice_covered(thck(ng))) then
                     Sg_ice_covered(ns)%ptr(ng) = 1.d0
                  else
                     Sg_ice_covered(ns)%ptr(ng) = 0.d0
                  end if
                  ! Note that we use the same method for computing topo whether this point is
                  ! ice-covered or ice-free. This is in contrast to the method for computing
                  ! ice-free topo in glint_upscaling_gcm.
                  Sg_topo(ns)%ptr(ng) = thk0 * usrf
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

            call pio_closefile(pioid)
            call pio_freedecomp(pio_subsystem, pio_iodesc)

         end do ! end loop over ice sheets

      end if

      if (initialized_noevolve) then

         ! Compute Fgrg_rofi
         do ns = 1,num_icesheets

            ! Get number of grid cells per ice sheet
            lsize = size(Fgrg_rofi(ns)%ptr)

            ! reset output variables to zero
            Fgrg_rofi(ns)%ptr(:) = 0.d0
            loc_pos_smb(1) = 0.d0
            Tot_pos_smb(1) = 0.d0
            loc_neg_smb(1) = 0.d0
            Tot_neg_smb(1) = 0.d0
            rat = 0.d0

            ! For No Evolve to reduce negative ice fluxes from DGLC, we will
            ! Calculate the total positive and total negative fluxes on each
            ! processor first (local totals).
            do ng = 1,lsize
               if (Sg_icemask_coupled_fluxes(ns)%ptr(ng) > 0.d0) then
                  if(Flgl_qice(ns)%ptr(ng) > 0.d0) then
                     loc_pos_smb(1) = loc_pos_smb(1)+Flgl_qice(ns)%ptr(ng)*Sg_area(ns)%ptr(ng)
                  end if
                  ! Ignore places that are exactly 0.d0
                  if(Flgl_qice(ns)%ptr(ng) < 0.d0) then
                     loc_neg_smb(1) = loc_neg_smb(1)+Flgl_qice(ns)%ptr(ng)*Sg_area(ns)%ptr(ng)
                  end if
               end if
            end do
            ! Now do two global sums to get the ice sheet total positive
            ! and negative ice fluxes
            call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
            if (ChkErr(rc,__LINE__,u_FILE_u)) return
            call ESMF_VMAllreduce(vm, senddata=loc_pos_smb, recvdata=Tot_pos_smb, count=1, &
                 reduceflag=ESMF_REDUCE_SUM, rc=rc)
            if (ChkErr(rc,__LINE__,u_FILE_u)) return
            call ESMF_VMAllreduce(vm, senddata=loc_neg_smb, recvdata=Tot_neg_smb, count=1, &
                 reduceflag=ESMF_REDUCE_SUM, rc=rc)
            if (ChkErr(rc,__LINE__,u_FILE_u)) return

            ! If there's more positive than negative, then set all
            ! negative to zero and destribute the negative flux amount
            ! across the positive values, scaled by the size of the
            ! positive value. This section also applies to any chunks
            ! where there is no negative smb. In that case the ice
            ! runoff is exactly equal to the input smb.
            if(abs(Tot_pos_smb(1)) >= abs(Tot_neg_smb(1))) then
               do ng = 1,lsize             
                  if (Sg_icemask_coupled_fluxes(ns)%ptr(ng) > 0.d0) then
                     if(Flgl_qice(ns)%ptr(ng) > 0.d0) then
                        rat = Flgl_qice(ns)%ptr(ng)/Tot_pos_smb(1)
                        Fgrg_rofi(ns)%ptr(ng) = Flgl_qice(ns)%ptr(ng) + rat*Tot_neg_smb(1)
                     else
                        Fgrg_rofi(ns)%ptr(ng) = 0.d0
                     end if
                  else
                     Fgrg_rofi(ns)%ptr(ng) = 0.d0
                  end if
               end do
            else
               ! If there's more negative than positive, set the positive to zero
               ! and distribute the positive amount to the negative spaces to
               ! reduce their negativity a bit. This shouldn't happen often.
               ! This section of code also applies if Tot_pos_smb is zero.
               do ng = 1,lsize
                  if (Sg_icemask_coupled_fluxes(ns)%ptr(ng) > 0.d0) then
                     if(Flgl_qice(ns)%ptr(ng) < 0.d0) then
                        rat = Flgl_qice(ns)%ptr(ng)/Tot_neg_smb(1)
                        Fgrg_rofi(ns)%ptr(ng) = Flgl_qice(ns)%ptr(ng) + rat*Tot_pos_smb(1)
                     else
                        Fgrg_rofi(ns)%ptr(ng) = 0.d0
                     end if
                  else
                     Fgrg_rofi(ns)%ptr(ng) = 0.d0
                  end if
               end do
                  
            end if ! More neg or pos smb

         end do ! Each ice sheet
      end if ! if initialized

      ! Set initialized flag
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

  !===============================================================================
  subroutine dglc_datamode_noevolve_restart_write(model_meshes, case_name, &
       rpfile, inst_suffix, ymd, tod, logunit, my_task, main_task, &
       pio_subsystem, io_type, nx_global, ny_global, rc)

    ! input/output variables
    type(ESMF_Mesh)        , intent(in)    :: model_meshes(:) ! ice sheets meshes
    character(len=*)       , intent(in)    :: case_name
    character(len=*)       , intent(in)    :: rpfile
    character(len=*)       , intent(in)    :: inst_suffix
    integer                , intent(in)    :: ymd             ! model date
    integer                , intent(in)    :: tod             ! model sec into model date
    integer                , intent(in)    :: logunit
    integer                , intent(in)    :: my_task
    integer                , intent(in)    :: main_task
    type(iosystem_desc_t)  , pointer       :: pio_subsystem   ! pio info
    integer                , intent(in)    :: io_type         ! pio info
    integer                , intent(in)    :: nx_global(:)
    integer                , intent(in)    :: ny_global(:)
    integer                , intent(out)   :: rc

    ! local variables
    type(ESMF_DistGrid) :: distgrid
    integer             :: ns
    character(len=CS)   :: cnum
    integer             :: lsize
    integer, pointer    :: gindex(:) ! domain decomposition of data
    integer             :: nu
    character(len=CL)   :: rest_file_model
    character(len=CS)   :: date_str
    type(file_desc_t)   :: pioid
    integer             :: dimid2(2)
    type(var_desc_t), allocatable    :: varid(:)
    type(io_desc_t), allocatable     :: pio_iodesc(:)
    integer             :: oldmode
    integer             :: rcode
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call shr_cal_datetod2string(date_str, ymd, tod)
    write(rest_file_model ,"(7a)") trim(case_name),'.','dglc',trim(inst_suffix),'.r.',trim(date_str),'.nc'
    ! write restart info to rpointer file
    if (my_task == main_task) then
       open(newunit=nu, file=trim(rpfile), form='formatted')
       write(nu,'(a)') rest_file_model
       close(nu)
       write(logunit,'(a,2x,i0,2x,i0)')' writing with no streams '//trim(rest_file_model), ymd, tod
    endif

    ! write data model restart data
    rcode = pio_createfile(pio_subsystem, pioid, io_type, trim(rest_file_model), pio_clobber)
    allocate(varid(num_icesheets))
    do ns = 1,num_icesheets
       ! Need to explicitly write restart since noevolve mode does not read a stream
       write(cnum,'(i0)') ns

       rcode = pio_def_dim(pioid, '_nx'//trim(cnum), nx_global(ns), dimid2(1))
       rcode = pio_def_dim(pioid, '_ny'//trim(cnum), ny_global(ns), dimid2(2))
       rcode = pio_def_var(pioid, 'flgl_rofi'//cnum, PIO_DOUBLE, (/dimid2/), varid(ns))
       rcode = pio_put_att(pioid, varid(ns), "_FillValue", shr_const_spval)
       rcode = pio_set_fill(pioid, PIO_FILL, oldmode)
       rcode = pio_put_att(pioid, pio_global, "version", "nuopc_data_models_v0")
    enddo
    rcode = pio_enddef(pioid)
    allocate(pio_iodesc(num_icesheets))
    do ns = 1,num_icesheets

       ! Determine gindex for this ice sheet
       call ESMF_MeshGet(model_meshes(ns), elementdistGrid=distGrid, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_DistGridGet(distGrid, localDe=0, elementCount=lsize, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       allocate(gindex(lsize))
       call ESMF_DistGridGet(distGrid, localDe=0, seqIndexList=gindex, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call pio_initdecomp(pio_subsystem, pio_double, (/nx_global(ns),ny_global(ns)/), gindex, pio_iodesc(ns))
       call pio_write_darray(pioid, varid(ns), pio_iodesc(ns), Fgrg_rofi(ns)%ptr, rcode, fillval=shr_const_spval)
       
       ! Deallocate gindex
       deallocate (gindex)
    end do
    call pio_closefile(pioid)
    do ns = 1,num_icesheets
       call pio_freedecomp(pio_subsystem, pio_iodesc(ns))
    enddo

  end subroutine dglc_datamode_noevolve_restart_write

  !===============================================================================
  subroutine dglc_datamode_noevolve_restart_read(model_meshes, restfilem, rpfile, &
       logunit, my_task, main_task, mpicom, &
       pio_subsystem, io_type, nx_global, ny_global, rc)

    ! input/output arguments
    type(ESMF_Mesh)        , intent(in)    :: model_meshes(:) ! ice sheets meshes
    character(len=*)       , intent(inout) :: restfilem
    character(len=*)       , intent(in)    :: rpfile
    integer                , intent(in)    :: logunit
    integer                , intent(in)    :: my_task
    integer                , intent(in)    :: main_task
    integer                , intent(in)    :: mpicom
    type(iosystem_desc_t)  , pointer       :: pio_subsystem   ! pio info
    integer                , intent(in)    :: io_type         ! pio info
    integer                , intent(in)    :: nx_global(:)
    integer                , intent(in)    :: ny_global(:)
    integer                , intent(out)   :: rc

    ! local variables
    type(ESMF_DistGrid) :: distgrid
    integer             :: ns
    character(len=CS)   :: cnum
    integer             :: lsize
    integer, pointer    :: gindex(:) ! domain decomposition of data
    type(ESMF_VM)       :: vm
    integer             :: nu
    logical             :: exists  ! file existance
    type(file_desc_t)   :: pioid
    type(var_desc_t)    :: varid
    type(io_desc_t)     :: pio_iodesc
    integer             :: rcode
    integer             :: tmp(1)
    character(*), parameter :: subName = "(dglc_datamode_noevolve_restart_read) "
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    ! Determine restart file


    if (trim(restfilem) == trim(nullstr)) then
       exists = .false.
       call ESMF_VMGetCurrent(vm, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       if (my_task == main_task) then
          write(logunit,'(a)') trim(subname)//' restart filename from rpointer '//trim(rpfile)
          open(newunit=nu, file=trim(rpfile), form='formatted')
          read(nu,'(a)') restfilem
          close(nu)
          inquire(file=trim(restfilem), exist=exists)
       endif
       call ESMF_VMBroadCast(vm, restfilem, CL, main_task, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    else
       ! use namelist already read
       if (my_task == main_task) then
          write(logunit, '(a)') trim(subname)//' restart filenames from namelist '
          inquire(file=trim(restfilem), exist=exists)
       endif
    endif
    tmp = 0
    if(exists) tmp=1
    exists = (tmp(1) == 1)
    if (.not. exists .and. my_task == main_task) then
       write(logunit, '(a)') trim(subname)//' file not found, skipping '//trim(restfilem)
       return
    end if
    
    ! Read restart file
    if (my_task == main_task) then
       write(logunit, '(a)') trim(subname)//' reading data model restart '//trim(restfilem)
    end if
    
    rcode = pio_openfile(pio_subsystem, pioid, io_type, trim(restfilem), pio_nowrite)
    do ns = 1,num_icesheets

       write(cnum,'(i0)') ns

       ! Determine gindex for this ice sheet
       call ESMF_MeshGet(model_meshes(ns), elementdistGrid=distGrid, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_DistGridGet(distGrid, localDe=0, elementCount=lsize, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       allocate(gindex(lsize))
       call ESMF_DistGridGet(distGrid, localDe=0, seqIndexList=gindex, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return


       call pio_initdecomp(pio_subsystem, pio_double, (/nx_global(ns),ny_global(ns)/), gindex, pio_iodesc)
       rcode = pio_inq_varid(pioid, 'flgl_rofi'//cnum, varid)
       call pio_read_darray(pioid, varid, pio_iodesc, Fgrg_rofi(ns)%ptr, rcode)
       call pio_freedecomp(pio_subsystem, pio_iodesc)

       ! Deallocate gindex
       deallocate(gindex)

    end do ! loop over ice sheets
    call pio_closefile(pioid)

 end subroutine dglc_datamode_noevolve_restart_read

end module dglc_datamode_noevolve_mod
