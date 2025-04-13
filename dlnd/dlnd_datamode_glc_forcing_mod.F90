module dlnd_datamode_glc_forcing_mod

   use ESMF             , only : ESMF_SUCCESS, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_State
   use ESMF             , only : ESMF_StateItem_Flag, ESMF_GridComp
   use NUOPC            , only : NUOPC_CompAttributeGet, NUOPC_Advertise
   use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
   use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
   use dshr_strdata_mod , only : shr_strdata_type
   use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
   use dshr_dfield_mod  , only : dfield_type, dshr_dfield_add
   use glc_elevclass_mod, only : glc_elevclass_as_string, glc_elevclass_init

   implicit none
   private ! except

   public :: dlnd_datamode_glc_forcing_advertise
   public :: dlnd_datamode_glc_forcing_init_pointers

   ! module pointer arrays
   real(r8), pointer :: lfrac(:)

   integer :: glc_nec

   character(*), parameter :: nullstr = 'null'
   character(*), parameter :: u_FILE_u = &
        __FILE__

!===============================================================================
contains
!===============================================================================

   subroutine dlnd_datamode_glc_forcing_advertise(gcomp, exportState, fldsExport, flds_scalar_name, logunit, mainproc, rc)

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

   subroutine dlnd_datamode_glc_forcing_init_pointers(exportState, sdat, dfields, model_frac, logunit, mainproc, rc)

      ! input/output variables
      type(ESMF_State)      , intent(inout) :: exportState
      type(shr_strdata_type), intent(in)    :: sdat
      type(dfield_type)     , pointer       :: dfields
      real(r8)              , intent(in)    :: model_frac(:)
      integer               , intent(in)    :: logunit
      logical               , intent(in)    :: mainproc
      integer               , intent(out)   :: rc

      ! local variables
      integer                     :: n
      character(len=2)            :: nec_str
      character(CS), allocatable  :: strm_flds(:)
      character(len=*), parameter :: subname='(dlnd_datamode_glc_forcing_init_pointers): '
      !-------------------------------------------------------------------------------

      rc = ESMF_SUCCESS

      ! Set fractional land pointer in export state
      call dshr_state_getfldptr(exportState, fldname='Sl_lfrin', fldptr1=lfrac, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      lfrac(:) = model_frac(:)

      ! Create stream-> export state mapping for 2d fields
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

   end subroutine dlnd_datamode_glc_forcing_init_pointers

end module dlnd_datamode_glc_forcing_mod
