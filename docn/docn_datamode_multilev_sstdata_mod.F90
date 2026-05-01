module docn_datamode_multilev_sstdata_mod

  use ESMF             , only : ESMF_State, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_const_mod    , only : shr_const_TkFrz, shr_const_pi, shr_const_ocn_ref_sal, shr_const_spval
  use shr_log_mod      , only : shr_log_error
  use dshr_methods_mod , only : dshr_state_getfldptr, dshr_fldbun_getfldptr, chkerr
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
  use dshr_strdata_mod , only : shr_strdata_get_stream_pointer, shr_strdata_type, shr_strdata_get_stream_count

  implicit none
  private

  public :: docn_datamode_multilev_sstdata_advertise
  public :: docn_datamode_multilev_sstdata_init_pointers
  public :: docn_datamode_multilev_sstdata_advance

  ! pointers to export fields
  real(r8), pointer :: So_omask(:)     => null()    ! real ocean fraction sent to mediator
  real(r8), pointer :: So_t_depth(:,:) => null()
  real(r8), pointer :: So_s_depth(:,:) => null()
  real(r8), pointer :: So_t(:)         => null()
  real(r8), pointer :: So_u(:)         => null()
  real(r8), pointer :: So_v(:)         => null()
  real(r8), pointer :: So_s(:)         => null()

  ! pointers to stream fields
  real(r8), pointer :: strm_So_t(:)
  real(r8), pointer :: strm_So_t_depth(:,:) => null()
  real(r8), pointer :: strm_So_s_depth(:,:) => null()

  integer, parameter :: nlev_export = 30
  real(r8) :: vertical_levels(nlev_export) = (/  &
       30., 90., 150., 210., 270., 330., 390., 450., 510., 570., &
       630., 690., 750., 810., 870., 930., 990., 1050., 1110., 1170., &
       1230., 1290., 1350., 1410., 1470., 1530., 1590., 1650., 1710., 1770. /)

  real(r8) , parameter :: tkfrz   = shr_const_tkfrz       ! freezing point, fresh water (kelvin)
  real(r8) , parameter :: ocnsalt = shr_const_ocn_ref_sal ! ocean reference salinity

  character(len=*) , parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine docn_datamode_multilev_sstdata_advertise(exportState, fldsexport, flds_scalar_name, rc)

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
    call dshr_fldList_add(fldsExport, 'So_t')
    call dshr_fldList_add(fldsExport, 'So_s')
    call dshr_fldList_add(fldsExport, 'So_u')
    call dshr_fldList_add(fldsExport, 'So_v')

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(docn_comp_advertise): Fr_ocn'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine docn_datamode_multilev_sstdata_advertise

  !===============================================================================
  subroutine docn_datamode_multilev_sstdata_init_pointers(exportState, sdat, ocn_fraction, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    real(r8)               , intent(in)    :: ocn_fraction(:)
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(docn_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! initialize pointers to export fields
    call dshr_state_getfldptr(exportState, 'So_omask', fldptr1=So_omask   , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_t', fldptr1=So_t, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_s', fldptr1=So_s, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_u', fldptr1=So_u, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_v', fldptr1=So_v, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_t_depth', fldptr2=So_t_depth , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_s_depth', fldptr2=So_s_depth , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! initialize pointers to stream fields
    ! this has the full set of leveles in the stream data
    call shr_strdata_get_stream_pointer( sdat, 'So_t', strm_So_t, &
         errmsg=subname//'ERROR: strm_So_t must be associated for docn multilev_sstdata datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'So_t_depth', strm_So_t_depth, &
         errmsg=subname//'ERROR: strm_So_t_depth must be associated for docn multilev_sstdata datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'So_s_depth', strm_So_s_depth, &
         errmsg=subname//'ERROR: strm_So_t_depth must be associated for docn multilev_sstdata datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Initialize export state pointers to non-zero
    So_t_depth(:,:) = shr_const_TkFrz
    So_s_depth(:,:) = shr_const_ocn_ref_sal

    So_u(:) = 0.0_r8
    So_v(:) = 0.0_r8
    So_s(:) = ocnsalt
    So_t(:) = TkFrz

    ! Set export state ocean fraction (So_omask)
    So_omask(:) = ocn_fraction(:)

  end subroutine docn_datamode_multilev_sstdata_init_pointers

  !===============================================================================
  subroutine docn_datamode_multilev_sstdata_advance(sdat, logunit, mainproc, rc)

    ! input/output variables
    type(shr_strdata_type) , intent(in) :: sdat
    integer                , intent(in) :: logunit
    logical                , intent(in) :: mainproc
    integer                , intent(out) :: rc

    ! local variables
    integer  :: i,ki,ko
    integer  :: nstreams
    integer  :: nlev_stream
    integer  :: stream_index
    logical  :: level_found
    real(r8) :: factor
    real(r8), allocatable :: stream_vlevs(:)
    logical :: first_time = .true.
    character(len=*), parameter :: subname='(docn_datamode_multilev_sstdata): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Set ocean sst
    So_t(:) = strm_So_t(:) + TkFrz

    ! Determine number of vertical levels for multi level stream
    nstreams = shr_strdata_get_stream_count(sdat)
    nlev_stream = 0
    do stream_index = 1,nstreams
       nlev_stream = sdat%pstrm(stream_index)%stream_nlev
       if (nlev_stream > 1) exit
    end do
    if (nlev_stream == 0) then
       call shr_log_error(subname//" could not find vertical levels greater than 0", rc=rc)
       return
    end if
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
          call shr_log_error(subname//" could not find level bounds for vertical interpolation", rc=rc)
          return
       end if
    end do

    first_time = .false.

  end subroutine docn_datamode_multilev_sstdata_advance

end module docn_datamode_multilev_sstdata_mod
