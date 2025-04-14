module docn_datamode_multilev_cplhist_mod

  use ESMF             , only : ESMF_State, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_const_mod    , only : shr_const_TkFrz, shr_const_pi, shr_const_ocn_ref_sal, shr_const_spval
  use shr_sys_mod      , only : shr_sys_abort
  use dshr_methods_mod , only : dshr_state_getfldptr, dshr_fldbun_getfldptr, chkerr
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
  use dshr_strdata_mod , only : shr_strdata_get_stream_pointer, shr_strdata_type
  use dshr_dfield_mod  , only : dfield_type, dshr_dfield_add

  implicit none
  private ! except

  public :: docn_datamode_multilev_cplhist_advertise
  public :: docn_datamode_multilev_cplhist_init_pointers
  public :: docn_datamode_multilev_cplhist_advance

  ! pointers to export fields
  real(r8), pointer :: So_omask(:)     => null()    ! real ocean fraction sent to mediator
  real(r8), pointer :: So_t_depth(:,:) => null()
  real(r8), pointer :: So_s_depth(:,:) => null()

  ! pointers to stream fields
  real(r8), pointer :: stream_So_t_depth(:,:) => null()
  real(r8), pointer :: stream_So_s_depth(:,:) => null()

  integer, parameter :: nlev_export = 30
  real(r8) :: vertical_levels(nlev_export) = (/  &
         30._r8 ,  90._r8,  150._r8,  210._r8,  270._r8,  330._r8,  390._r8,  450._r8,  510._r8,  570._r8, &
        630._r8,  690._r8,  750._r8,  810._r8,  870._r8,  930._r8,  990._r8, 1050._r8, 1110._r8, 1170._r8, &
       1230._r8, 1290._r8, 1350._r8, 1410._r8, 1470._r8, 1530._r8, 1590._r8, 1650._r8, 1710._r8, 1770._r8 /)

  ! constants
  character(*) , parameter :: nullstr = 'null'
  character(*) , parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine docn_datamode_multilev_cplhist_advertise(exportState, fldsexport, flds_scalar_name, rc)

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
    call dshr_fldList_add(fldsExport, 'So_omask')
    call dshr_fldList_add(fldsExport, 'So_t_depth', ungridded_lbound=1, ungridded_ubound=nlev_export)
    call dshr_fldList_add(fldsExport, 'So_s_depth', ungridded_lbound=1, ungridded_ubound=nlev_export)

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(docn_comp_advertise): Fr_ocn'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine docn_datamode_multilev_cplhist_advertise

  !===============================================================================
  subroutine docn_datamode_multilev_cplhist_init_pointers(dfields, &
       exportState, sdat, ocn_fraction, logunit, mainproc, rc)

    ! input/output variables
    type(dfield_type)      , pointer       :: dfields
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    real(r8)               , intent(in)    :: ocn_fraction(:)
    integer                , intent(in)    :: logunit
    logical                , intent(in)    :: mainproc
    integer                , intent(out)   :: rc

    ! local variables
    integer                     :: n
    character(len=2)            :: num_str
    character(CS), allocatable  :: strm_flds_t_depth(:)
    character(CS), allocatable  :: strm_flds_s_depth(:)
    character(len=*), parameter :: subname='(docn_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Note - docn_datamode_multilev_mod the assumption is that the stream files contain single
    ! stream fields which contain the full set of levels in the stream data (i.e. 2d)
    ! Whereas here we are assuming a different stream field for each vertical level
    ! However, in both cases the export field contains an ungridded dimension for each vertical level

    ! Create stream-> export state mapping
    ! Note that strm_flds is the model name for the stream field
    ! Note that state_fld is the model name for the export field

    allocate(strm_flds_t_depth(1:nlev_export))
    allocate(strm_flds_s_depth(1:nlev_export))
    do n = 1,nlev_export
       write(num_str, '(i0)') n
       strm_flds_t_depth(n) = 'So_t_depth' // trim(num_str)
       strm_flds_s_depth(n) = 'So_s_depth' // trim(num_str)
    end do

    ! The following maps stream input multiple fields to export field that has an ungridded dimension
    call dshr_dfield_add(dfields, sdat, state_fld='So_t_depth', strm_flds=strm_flds_t_depth, state=exportState, &
         logunit=logunit, mainproc=mainproc, rc=rc)
    call dshr_dfield_add(dfields, sdat, state_fld='So_s_depth', strm_flds=strm_flds_s_depth, state=exportState, &
         logunit=logunit, mainproc=mainproc, rc=rc)

    ! Set export state ocean fraction (So_omask)
    call dshr_state_getfldptr(exportState, 'So_omask', fldptr1=So_omask   , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    So_omask(:) = ocn_fraction(:)

  end subroutine docn_datamode_multilev_cplhist_init_pointers

  !===============================================================================
  subroutine docn_datamode_multilev_cplhist_advance(exportState, rc)

    use dshr_methods_mod , only : dshr_state_getfldptr

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    integer                , intent(out)   :: rc

    ! local variables
    integer           :: idim1,idim2
    real(r8), pointer :: fldptr2(:,:)
    character(len=*), parameter :: subname='(docn_datamode_multilev): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call dshr_state_getfldptr(exportState, 'So_t_depth', fldptr2=fldptr2, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    do idim2 = 1,size(fldptr2,dim=2)
       do idim1 = 1,size(fldptr2,dim=1)
          if (fldptr2(idim1,idim2) == 0._r8) then
             fldptr2(idim1,idim2) = 1.e30_r8
          end if
       end do
    end do

    call dshr_state_getfldptr(exportState, 'So_s_depth', fldptr2=fldptr2, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    do idim2 = 1,size(fldptr2,dim=2)
       do idim1 = 1,size(fldptr2,dim=1)
          if (fldptr2(idim1,idim2) == 0._r8) then
             fldptr2(idim1,idim2) = 1.e30_r8
          end if
       end do
    end do

  end subroutine docn_datamode_multilev_cplhist_advance

end module docn_datamode_multilev_cplhist_mod
