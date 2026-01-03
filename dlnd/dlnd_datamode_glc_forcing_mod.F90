module dlnd_datamode_glc_forcing_mod

   use ESMF             , only : ESMF_SUCCESS, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_State
   use ESMF             , only : ESMF_StateItem_Flag, ESMF_GridComp
   use NUOPC            , only : NUOPC_CompAttributeGet, NUOPC_Advertise
   use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
   use shr_log_mod      , only : shr_log_error
   use shr_const_mod    , only : SHR_CONST_SPVAL
   use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
   use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
   use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
   use glc_elevclass_mod, only : glc_elevclass_as_string, glc_elevclass_init

   implicit none
   private ! except

   public :: dlnd_datamode_glc_forcing_advertise
   public :: dlnd_datamode_glc_forcing_init_pointers
   public :: dlnd_datamode_glc_forcing_advance

   ! export state pointer
   real(r8), pointer :: lfrac(:)            => null()
   real(r8), pointer :: Sl_tsrf_elev(:,:)   => null()
   real(r8), pointer :: Sl_topo_elev(:,:)   => null()
   real(r8), pointer :: Flgl_qice_elev(:,:) => null()

   ! stream pointers (1d)
   type, public :: stream_pointer_type
      real(r8), pointer :: strm_ptr(:) => null()
   end type stream_pointer_type
   type(stream_pointer_type), allocatable :: strm_Sl_tsrf_elev(:)
   type(stream_pointer_type), allocatable :: strm_Sl_topo_elev(:)
   type(stream_pointer_type), allocatable :: strm_Flgl_qice_elev(:)

   integer :: glc_nec

   character(*), parameter :: nullstr = 'null'
   character(*), parameter :: u_FILE_u = &
        __FILE__

!===============================================================================
contains
!===============================================================================

   subroutine dlnd_datamode_glc_forcing_advertise(gcomp, exportState, fldsExport, flds_scalar_name, &
        logunit, mainproc, rc)

      ! determine export state to advertise to mediator

      ! input/output arguments
      type(ESMF_GridComp), intent(inout) :: gcomp
      type(ESMF_State)   , intent(inout) :: exportState
      type(fldList_type) , pointer       :: fldsExport
      character(len=*)   , intent(in)    :: flds_scalar_name
      integer            , intent(in)    :: logunit
      logical            , intent(in)    :: mainproc
      integer            , intent(out)   :: rc

      ! local variables
      type(fldlist_type), pointer :: fldList
      character(cl)               :: cvalue
      !-------------------------------------------------------------------------------

      rc = ESMF_SUCCESS

      ! Determine glc_nec
      call NUOPC_CompAttributeGet(gcomp, name='glc_nec', value=cvalue, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      read(cvalue,*) glc_nec
      call ESMF_LogWrite('glc_nec = '// trim(cvalue), ESMF_LOGMSG_INFO)

      ! Initialize GLC elevation class data to default boundaries, based on given glc_nec
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

   end subroutine dlnd_datamode_glc_forcing_advertise

   !===============================================================================
   subroutine dlnd_datamode_glc_forcing_init_pointers(exportState, sdat, model_frac, datamode, rc)

      ! input/output variables
      type(ESMF_State)      , intent(inout) :: exportState
      type(shr_strdata_type), intent(in)    :: sdat
      real(r8)              , intent(in)    :: model_frac(:)
      character(len=*)      , intent(in)    :: datamode
      integer               , intent(out)   :: rc

      ! local variables
      integer          :: ng
      character(len=2) :: nec_str
      character(CS)    :: strm_fld
      integer          :: istat
      character(len=*), parameter :: subname='(dlnd_datamode_glc_forcing_init_pointers): '
      !-------------------------------------------------------------------------------

      rc = ESMF_SUCCESS

      ! Set fractional land pointer in export state
      call dshr_state_getfldptr(exportState, fldname='Sl_lfrin', fldptr1=lfrac, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      lfrac(:) = model_frac(:)
      call dshr_state_getfldptr(exportState, 'Sl_tsrf_elev', fldptr2=Sl_tsrf_elev, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      call dshr_state_getfldptr(exportState, 'Sl_topo_elev', fldptr2=Sl_topo_elev, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      call dshr_state_getfldptr(exportState, 'Flgl_qice_elev', fldptr2=Flgl_qice_elev, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return

<<<<<<< HEAD
      ! Create stream-> export state mapping
      ! Note that strm_flds is the model name for the stream field
      ! Note that state_fld is the model name for the export field

      if (trim(datamode) == 'glc_forcing_mct') then
         allocate(strm_flds_tsrf(0:glc_nec))
         allocate(strm_flds_topo(0:glc_nec))
         allocate(strm_flds_qice(0:glc_nec))
         do n = 0,glc_nec
            write(nec_str, '(i2.2)') n
            strm_flds_tsrf(n) = 'Sl_tsrf_elev'   // trim(nec_str)
            strm_flds_topo(n) = 'Sl_topo_elev'   // trim(nec_str)
            strm_flds_qice(n) = 'Flgl_qice_elev' // trim(nec_str)
         end do

      else if (trim(datamode) == 'glc_forcing' ) then
         allocate(strm_flds_tsrf(1:glc_nec+1))
         allocate(strm_flds_topo(1:glc_nec+1))
         allocate(strm_flds_qice(1:glc_nec+1))
         do n = 1,glc_nec+1
            write(nec_str, '(i0)') n
            strm_flds_tsrf(n) = 'Sl_tsrf_elev'   // trim(nec_str)
            strm_flds_topo(n) = 'Sl_topo_elev'   // trim(nec_str)
            strm_flds_qice(n) = 'Flgl_qice_elev' // trim(nec_str)
         end do
      else
         call shr_log_error(subname//'ERROR illegal datamode = '//trim(datamode), rc=rc)
=======
      ! Obtain pointers to stream fields

      allocate(strm_Sl_tsrf_elev(glc_nec+1), &
               strm_Sl_topo_elev(glc_nec+1), &
               strm_Flgl_qice_elev(glc_nec+1), stat=istat)
      if ( istat /= 0 ) then
         call shr_log_error(subName//&
              ': allocation error for strm_Sl_tsrf_elev, Strm_Sl_topo_elev and strm_Flgl_qice_elev',rc=rc)
>>>>>>> mvertens/feature/new_datm_optional_streams
         return
      end if

      do ng = 1,glc_nec+1
         if (trim(datamode) == 'glc_forcing_mct') then
            write(nec_str,'(i2.2)') ng-1
         else
            write(nec_str,'(i0)') ng
         end if
         strm_fld = 'Sl_tsrf_elev'//trim(nec_str)
         call shr_strdata_get_stream_pointer( sdat, trim(strm_fld), strm_Sl_tsrf_elev(ng)%strm_ptr, requirePointer=.true., &
              errmsg=trim(subname)//'ERROR: '//trim(strm_fld)//' must be associated for dlnd glc_forcing datamode', rc=rc)
         if (ChkErr(rc,__LINE__,u_FILE_u)) return

         strm_fld = 'Sl_topo_elev'//trim(nec_str)
         call shr_strdata_get_stream_pointer( sdat, trim(strm_fld), strm_Sl_topo_elev(ng)%strm_ptr, requirePointer=.true., &
              errmsg=trim(subname)//'ERROR: '//trim(strm_fld)//' must be associated for dlnd glc_forcing datamode', rc=rc)
         if (ChkErr(rc,__LINE__,u_FILE_u)) return

         strm_fld = 'Flgl_qice_elev'//trim(nec_str)
         call shr_strdata_get_stream_pointer( sdat, trim(strm_fld), strm_Flgl_qice_elev(ng)%strm_ptr, requirePointer=.true., &
              errmsg=trim(subname)//'ERROR: '//trim(strm_fld)//' must be associated for dlnd glc_forcing datamode', rc=rc)
         if (ChkErr(rc,__LINE__,u_FILE_u)) return
      end do

   end subroutine dlnd_datamode_glc_forcing_init_pointers

   !===============================================================================
   subroutine dlnd_datamode_glc_forcing_advance()

      ! local variables
      integer :: ni,ng
      character(len=*), parameter :: subname='(dlnd_datamode_glc_forcing_advance): '
      !-------------------------------------------------------------------------------

      ! Set special value over masked points
      ! Note that the inner dimension is the elevation class

      elev_class_loop: do ng = 1,glc_nec+1
         do ni = 1,size(Sl_tsrf_elev,dim=2)
            if (lfrac(ni) == 0._r8) then
               Sl_tsrf_elev(ng,ni) = SHR_CONST_SPVAL
            else
               Sl_tsrf_elev(ng,ni) = strm_Sl_tsrf_elev(ng)%strm_ptr(ni)
            end if
         end do

         do ni = 1,size(Sl_topo_elev,dim=2)
            if (lfrac(ni) == 0._r8) then
               Sl_topo_elev(ng,ni) = SHR_CONST_SPVAL
            else
               Sl_topo_elev(ng,ni) = strm_Sl_topo_elev(ng)%strm_ptr(ni)
            end if
         end do

         do ni = 1,size(Flgl_qice_elev,dim=2)
            if (lfrac(ni) == 0._r8) then
               Flgl_qice_elev(ng,ni) = SHR_CONST_SPVAL
            else
               Flgl_qice_elev(ng,ni) = strm_Flgl_qice_elev(ng)%strm_ptr(ni)
            end if
         end do
      end do elev_class_loop

   end subroutine dlnd_datamode_glc_forcing_advance

end module dlnd_datamode_glc_forcing_mod
