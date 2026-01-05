module docn_datamode_multilev_mod
  use ESMF             , only : ESMF_State, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_const_mod    , only : shr_const_TkFrz, shr_const_pi, shr_const_ocn_ref_sal, shr_const_spval
  use shr_log_mod      , only : shr_log_error
  use dshr_methods_mod , only : dshr_state_getfldptr, dshr_fldbun_getfldptr, chkerr
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
  use dshr_strdata_mod , only : shr_strdata_get_stream_pointer, shr_strdata_type

  implicit none
  private

  public :: docn_datamode_multilev_advertise
  public :: docn_datamode_multilev_init_pointers
  public :: docn_datamode_multilev_advance

  ! pointers to export fields
  real(r8), pointer :: So_omask(:)     => null()    ! real ocean fraction sent to mediator
  real(r8), pointer :: So_t_depth(:,:) => null()
  real(r8), pointer :: So_s_depth(:,:) => null()

  ! pointers to stream fields
  real(r8), pointer :: strm_So_t_depth(:,:) => null()
  real(r8), pointer :: strm_So_s_depth(:,:) => null()

  integer, parameter :: nlev_export = 30
  real(r8) :: vertical_levels(nlev_export) = (/  &
       30., 90., 150., 210., 270., 330., 390., 450., 510., 570., &
       630., 690., 750., 810., 870., 930., 990., 1050., 1110., 1170., &
       1230., 1290., 1350., 1410., 1470., 1530., 1590., 1650., 1710., 1770. /)

  ! constants
  character(len=*) , parameter :: nullstr = 'null'
  character(len=*) , parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine docn_datamode_multilev_advertise(exportState, fldsexport, flds_scalar_name, rc)

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

  end subroutine docn_datamode_multilev_advertise

  !===============================================================================
  subroutine docn_datamode_multilev_init_pointers(exportState, sdat, ocn_fraction, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    real(r8)               , intent(in)    :: ocn_fraction(:)
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(docn_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Set export state pointers (has only nlev_export levels)
    call dshr_state_getfldptr(exportState, 'So_omask'   , fldptr1=So_omask   , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_t_depth' , fldptr2=So_t_depth , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_s_depth' , fldptr2=So_s_depth , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Set stream pointers (this has the full set of leveles in the stream data)
    call shr_strdata_get_stream_pointer( sdat, 'So_t_depth', strm_So_t_depth, &
         errmsg=subname//'ERROR: strm_So_t_depth must be associated for docn multilev datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'So_s_depth', strm_So_s_depth, &
         errmsg=subname//'ERROR: strm_So_s_depth must be associated for docn multilev datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Initialize export state pointers to non-zero
    So_t_depth(:,:) = shr_const_TkFrz
    So_s_depth(:,:) = shr_const_ocn_ref_sal
    So_omask(:) = ocn_fraction(:)

  end subroutine docn_datamode_multilev_init_pointers

  !===============================================================================
  subroutine docn_datamode_multilev_advance(sdat, logunit, mainproc, rc)

    ! input/output variables
    type(shr_strdata_type) , intent(in)  :: sdat
    integer                , intent(in)  :: logunit
    logical                , intent(in)  :: mainproc
    integer                , intent(out) :: rc

    ! local variables
    integer  :: i,ki,ko
    integer  :: nlev_stream
    integer  :: stream_index
    logical  :: level_found
    real(r8) :: factor
    logical  ::  first_time = .true.
    real(r8), allocatable :: stream_vlevs(:)
    character(len=*), parameter :: subname='(docn_datamode_multilev): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! For now assume that all the streams have the same vertical levels
    stream_index = 1

    nlev_stream = sdat%pstrm(stream_index)%stream_nlev
    allocate(stream_vlevs(nlev_stream))
    stream_vlevs(:) = sdat%pstrm(stream_index)%stream_vlevs(:)

    do ko = 1,nlev_export
       level_found = .false.
       do ki = 1,nlev_stream-1
          if (vertical_levels(ko) > stream_vlevs(ki) .and. vertical_levels(ko) <= stream_vlevs(ki+1)) then
             if (mainproc .and. first_time) then
                write(logunit,'(a,3(i5,2x),3(f13.5,2x))') &
                     'vertical interpolation: ki,ko,ki+1,lev(ki),lev(ko),lev(ki+1) = ',&
                     ki,ko,ki+1,stream_vlevs(ki), vertical_levels(ko), stream_vlevs(ki+1)
             end if
             level_found = .true.
             do i = 1,size(So_omask)
                if (So_omask(i) == 0.) then
                   So_t_depth(ko,i) = shr_const_spval
                   So_s_depth(ko,i) = shr_const_spval
                else
                   ! Assume input T forcing is in degrees C
                   if (strm_So_t_depth(ki+1,i) > 1.e10) then
                      if (strm_So_t_depth(ki,i) > 1.e10) then
                         So_t_depth(ko,i) = shr_const_spval
                         So_s_depth(ko,i) = shr_const_spval
                      else
                         So_t_depth(ko,i) = strm_So_t_depth(ki,i) + shr_const_tkfrz
                         So_s_depth(ko,i) = strm_So_s_depth(ki,i)
                      end if
                   else
                      factor = (strm_So_t_depth(ki+1,i)-strm_So_t_depth(ki,i))/(stream_vlevs(ki+1)-stream_vlevs(ki))
                      So_t_depth(ko,i) = strm_So_t_depth(ki,i) + (vertical_levels(ko)-stream_vlevs(ki))*factor
                      So_t_depth(ko,i) = So_t_depth(ko,i) + shr_const_tkfrz

                      factor = (strm_So_s_depth(ki+1,i)-strm_So_s_depth(ki,i))/(stream_vlevs(ki+1)-stream_vlevs(ki))
                      So_s_depth(ko,i) = strm_So_s_depth(ki,i) + (vertical_levels(ko)-stream_vlevs(ki))*factor
                   end if
                end if
             end do
          end if
       end do
       if (.not. level_found) then
          call shr_log_error("ERROR: could not find level bounds for vertical interpolation", rc=rc)
          return
       end if
    end do
    first_time = .false.

  end subroutine docn_datamode_multilev_advance

end module docn_datamode_multilev_mod
