module dshr_dfield_mod

  use ESMF             , only : ESMF_State, ESMF_FieldBundle, ESMF_MAXSTR, ESMF_SUCCESS
  use ESMF             , only : ESMF_FieldBundleGet, ESMF_ITEMORDER_ADDORDER, ESMF_Field, ESMF_FieldGet
  use shr_kind_mod     , only : r8=>shr_kind_r8, cs=>shr_kind_cs, cl=>shr_kind_cl, cxx=>shr_kind_cxx
  use shr_log_mod      , only : shr_log_error
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_count, shr_strdata_get_stream_fieldbundle
  use dshr_methods_mod , only : dshr_state_getfldptr, dshr_fldbun_getfieldn, dshr_field_getfldptr, dshr_fldbun_getFldPtr
  use dshr_methods_mod , only : chkerr

  implicit none
  private

  public :: dshr_dfield_add
  public :: dshr_dfield_copy

  interface dshr_dfield_add
     module procedure dshr_dfield_add_1d
     module procedure dshr_dfield_add_1d_stateptr
     module procedure dshr_dfield_add_2d
     module procedure dshr_dfield_add_2d_stateptr
  end interface dshr_dfield_add

  ! Note that whereas the data model export state field bundle might have fields
  ! with undistributed dimensions - the stream field bundles only have fields
  ! with no undistributed dimensions

  ! Linked list node
  type, public :: dfield_type
     ! state data with no ungridded dimensions
     real(r8), pointer          :: state_data1d(:) => null()
     integer                    :: stream_index = 0
     integer                    :: fldbun_index = 0
     ! state data with ungridded dimensions
     real(r8), pointer          :: state_data2d(:,:) => null()
     integer, pointer           :: stream_indices(:) => null()
     integer, pointer           :: fldbun_indices(:) => null()
     ! linked list pointer
     type(dfield_type), pointer :: next => null()
  end type dfield_type

  integer     , parameter :: iunset = -999
  character(*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine dshr_dfield_add_1d(dfields, sdat, state_fld, strm_fld, state, logunit, mainproc, rc)

    ! Set 1d dfield values

    type(dfield_type)      , pointer       :: dfields
    type(shr_strdata_type) , intent(in)    :: sdat
    character(len=*)       , intent(in)    :: state_fld
    character(len=*)       , intent(in)    :: strm_fld
    type(ESMF_State)       , intent(inout) :: state
    integer                , intent(in)    :: logunit
    logical                , intent(in)    :: mainproc
    integer                , intent(out)   :: rc

    ! local variables
    type(dfield_type), pointer      :: dfield_new
    integer                         :: ns, nf
    integer                         :: status
    character(cl)                   :: msgstr
    integer                         :: fieldcount
    type(ESMF_FieldBundle)          :: fldbun_model
    character(ESMF_MAXSTR) ,pointer :: lfieldnamelist(:)
    logical                         :: found
    character(len=*), parameter     :: subname='(dfield_add_1d)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    allocate(dfield_new, stat=status)
    if (status /= 0) then
       write(msgstr,*)'allocation error ',__LINE__,':',__FILE__
       call shr_log_error(msgstr, rc=rc)
       return
    endif
    dfield_new%next => dfields
    dfields => dfield_new
    dfield_new%stream_index = iunset
    dfield_new%fldbun_index = iunset

    ! loop over all input streams and ! determine if the strm_fld is in the attribute vector of stream ns
    ! if strm_fld is in the field bundle of stream ns, set the field index of the field with the name strm_fld
    ! and set the index of the stream

    ! loop over all input streams and ! determine if the strm_fld is in the attribute vector of stream ns
    do ns = 1, shr_strdata_get_stream_count(sdat)
       fldbun_model = shr_strdata_get_stream_fieldbundle(sdat, ns, 'model')
       call ESMF_FieldBundleGet(fldbun_model, fieldCount=fieldCount, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       allocate(lfieldnamelist(fieldCount))
       call ESMF_FieldBundleGet(fldbun_model, fieldNameList=lfieldnamelist, itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       ! if strm_fld is in the field bundle of stream ns then set the field index of the field with
       ! the name strm_fld and set the index of the stream
       found = .false.
       do nf = 1,fieldcount
          if (trim(strm_fld) == trim(lfieldnamelist(nf))) then
             found = .true.
             dfield_new%fldbun_index = nf
             dfield_new%stream_index = ns
             if (found) exit
          end if
       end do
       deallocate(lfieldnamelist)
    end do

    ! Set export state array pointer
    call dshr_state_getfldptr(State, fldname=trim(state_fld), fldptr1=dfield_new%state_data1d, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    dfield_new%state_data1d = 0.0_r8
    if (mainproc) then
       write(logunit,110)'(dshr_addfield_add) setting pointer for export state '//trim(state_fld)
110    format(a)
    end if

  end subroutine dshr_dfield_add_1d

  !===============================================================================
  subroutine dshr_dfield_add_1d_stateptr(dfields, sdat, state_fld, strm_fld, state, state_ptr, logunit, mainproc, rc)

    ! Set 1d dfield values

    type(dfield_type)      , pointer       :: dfields
    type(shr_strdata_type) , intent(in)    :: sdat
    character(len=*)       , intent(in)    :: state_fld
    character(len=*)       , intent(in)    :: strm_fld
    type(ESMF_State)       , intent(inout) :: state
    real(r8)               , pointer       :: state_ptr(:)
    integer                , intent(in)    :: logunit
    logical                , intent(in)    :: mainproc
    integer                , intent(out)   :: rc

    ! local variables
    type(dfield_type), pointer      :: dfield_new
    integer                         :: ns, nf
    integer                         :: status
    character(cl)                   :: msgstr
    integer                         :: fieldcount
    type(ESMF_FieldBundle)          :: fldbun_model
    character(ESMF_MAXSTR) ,pointer :: lfieldnamelist(:)
    logical                         :: found
    character(len=*), parameter     :: subname='(dfield_add_1d)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS
    found = .false.
    allocate(dfield_new, stat=status)
    if (status /= 0) then
       write(msgstr,*)'allocation error ',__LINE__,':',__FILE__
       call shr_log_error(msgstr, rc=rc)
       return
    endif
    dfield_new%next => dfields
    dfields => dfield_new
    dfield_new%stream_index = iunset
    dfield_new%fldbun_index = iunset

    ! loop over all input streams and ! determine if the strm_fld is in the attribute vector of stream ns
    ! if strm_fld is in the field bundle of stream ns, set the field index of the field with the name strm_fld
    ! and set the index of the stream

    ! loop over all input streams and ! determine if the strm_fld is in the attribute vector of stream ns
    do ns = 1, shr_strdata_get_stream_count(sdat)
       fldbun_model = shr_strdata_get_stream_fieldbundle(sdat, ns, 'model')
       call ESMF_FieldBundleGet(fldbun_model, fieldCount=fieldCount, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       allocate(lfieldnamelist(fieldCount))
       call ESMF_FieldBundleGet(fldbun_model, fieldNameList=lfieldnamelist, itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       ! if strm_fld is in the field bundle of stream ns then set the field index of the field with
       ! the name strm_fld and set the index of the stream
       found = .false.
       do nf = 1,fieldcount
          if (trim(strm_fld) == trim(lfieldnamelist(nf))) then
             found = .true.
             dfield_new%fldbun_index = nf
             dfield_new%stream_index = ns
             if (found) exit
          end if
       end do
       deallocate(lfieldnamelist)
    end do

    ! Set export state array pointer
    call dshr_state_getfldptr(State, fldname=trim(state_fld), fldptr1=dfield_new%state_data1d, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    dfield_new%state_data1d = 0.0_r8
    if (mainproc) then
       write(logunit,110)'(dshr_addfield_add) setting pointer for export state '//trim(state_fld)
110    format(a)
    end if

    ! Return array pointer if argument is present
    state_ptr => dfield_new%state_data1d

    ! write output
    if (mainproc) then
       if (found) then
          write(logunit,100)'(dshr_addfield_add) set pointer to stream field strm_'//trim(strm_fld)//&
               ' stream index = ',ns,' field bundle index= ',nf
100       format(a,i6,2x,a,i6)
       end if
       write(logunit,*)
    end if

  end subroutine dshr_dfield_add_1d_stateptr

  !===============================================================================
  subroutine dshr_dfield_add_2d(dfields, sdat, state_fld, strm_flds, state, &
       logunit, mainproc, rc)

    ! input/output variables
    type(dfield_type)      , pointer       :: dfields
    type(shr_strdata_type) , intent(in)    :: sdat
    character(len=*)       , intent(in)    :: state_fld
    character(len=*)       , intent(in)    :: strm_flds(:)
    type(ESMF_State)       , intent(inout) :: state
    integer                , intent(in)    :: logunit
    logical                , intent(in)    :: mainproc
    integer                , intent(out)   :: rc

    ! local variables
    type(dfield_type), pointer      :: dfield_new
    type(ESMF_FieldBundle)          :: fldbun_model
    integer                         :: n, ns, nf
    integer                         :: nflds
    integer                         :: status
    character(cl)                   :: msgstr
    integer                         :: fieldcount
    character(ESMF_MAXSTR) ,pointer :: lfieldnamelist(:)
    logical                         :: ispresent
    character(len=*), parameter :: subname='(dfield_add_2d)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS
    if(associated(dfields)) then
       dfield_new => dfields
       do while(associated(dfield_new%next))
          dfield_new => dfield_new%next
       enddo
       allocate(dfield_new%next, stat=status)
       dfield_new => dfield_new%next
    else
       allocate(dfields, stat=status)
       dfield_new => dfields
    endif
    if (status /= 0) then
       write(msgstr,*)'allocation error ',__LINE__,':',__FILE__
       call shr_log_error(msgstr, rc=rc)
       return
    endif

    ! determine stream fldnames array
    nflds = size(strm_flds)
    allocate(dfield_new%stream_indices(nflds), stat=status)
    if (status /= 0) then
       write(msgstr,*)'allocation error ',__LINE__,':',__FILE__
       call shr_log_error(msgstr, rc=rc)
       return
    endif
    allocate(dfield_new%fldbun_indices(nflds), stat=status)
    if (status /= 0) then
       write(msgstr,*)'allocation error ',__LINE__,':',__FILE__
       call shr_log_error(msgstr, rc=rc)
       return
    endif
    dfield_new%stream_indices(:) = 0
    dfield_new%fldbun_indices(:) = 0

    ! loop through the field names in strm_flds
    do nf = 1, nflds

       ! loop through input streams
       do ns = 1, shr_strdata_get_stream_count(sdat)

          ! determine which stream the field with name dfield%stream_fldnames(nf) is in
          fldbun_model = shr_strdata_get_stream_fieldbundle(sdat, ns, 'model')

          call ESMF_FieldBundleGet(fldbun_model, fieldName=trim(strm_flds(nf)), isPresent=isPresent, rc=rc)
          if (ispresent) then
             ! if field is present in stream - determine the index in the field bundle of this field
             dfield_new%stream_indices(nf) = ns
             call ESMF_FieldBundleGet(fldbun_model, fieldCount=fieldCount, rc=rc)
             if (chkerr(rc,__LINE__,u_FILE_u)) return
             allocate(lfieldnamelist(fieldCount))
             call ESMF_FieldBundleGet(fldbun_model, fieldNameList=lfieldnamelist, itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
             if (chkerr(rc,__LINE__,u_FILE_u)) return

             do n = 1,fieldcount
                if (trim(strm_flds(nf)) == trim(lfieldnamelist(n))) then
                   dfield_new%fldbun_indices(nf) = n
                   if (mainproc) then
                      write(logunit,*)'(dshr_addfield_add) using stream field strm_'//&
                           trim(strm_flds(nf))//' for 2d '//trim(state_fld)
                   end if
                end if
             end do

             deallocate(lfieldnamelist)
             exit ! go to the next fld
          end if
       end do
    end do

    ! Set export state array pointer
    call dshr_state_getfldptr(State, fldname=trim(state_fld), fldptr2=dfield_new%state_data2d, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    dfield_new%state_data2d(:,:) = 0._r8
    if (mainproc) then
       write(logunit,*)'(dshr_addfield_add) setting pointer for export state '//trim(state_fld)
    end if

  end subroutine dshr_dfield_add_2d

  !===============================================================================
  subroutine dshr_dfield_add_2d_stateptr(dfields, sdat, state_fld, strm_flds, state, &
       state_ptr, logunit, mainproc, rc)

    ! input/output variables
    type(dfield_type)      , pointer       :: dfields
    type(shr_strdata_type) , intent(in)    :: sdat
    character(len=*)       , intent(in)    :: state_fld
    character(len=*)       , intent(in)    :: strm_flds(:)
    type(ESMF_State)       , intent(inout) :: state
    real(r8)               , pointer       :: state_ptr(:,:)
    integer                , intent(in)    :: logunit
    logical                , intent(in)    :: mainproc
    integer                , intent(out)   :: rc

    ! local variables
    type(dfield_type), pointer      :: dfield_new
    type(ESMF_FieldBundle)          :: fldbun_model
    integer                         :: n, ns, nf
    integer                         :: nflds
    integer                         :: status
    character(cl)                   :: msgstr
    integer                         :: fieldcount
    character(ESMF_MAXSTR) ,pointer :: lfieldnamelist(:)
    logical                         :: ispresent
    character(len=*), parameter :: subname='(dfield_add_2d)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS
    if(associated(dfields)) then
       dfield_new => dfields
       do while(associated(dfield_new%next))
          dfield_new => dfield_new%next
       enddo
       allocate(dfield_new%next, stat=status)
       dfield_new => dfield_new%next
    else
       allocate(dfields, stat=status)
       dfield_new => dfields
    endif
    if (status /= 0) then
       write(msgstr,*)'allocation error ',__LINE__,':',__FILE__
       call shr_log_error(msgstr, rc=rc)
       return
    endif

    ! determine stream fldnames array
    nflds = size(strm_flds)

    allocate(dfield_new%stream_indices(nflds), stat=status)
    if (status /= 0) then
       write(msgstr,*)'allocation error ',__LINE__,':',__FILE__
       call shr_log_error(msgstr, rc=rc)
       return
    endif
    allocate(dfield_new%fldbun_indices(nflds), stat=status)
    if (status /= 0) then
       write(msgstr,*)'allocation error ',__LINE__,':',__FILE__
       call shr_log_error(msgstr, rc=rc)
       return
    endif

    ! loop through the field names in strm_flds
    do nf = 1, nflds

       ! loop through input streams
       do ns = 1, shr_strdata_get_stream_count(sdat)

          ! determine which stream the field with name dfield%stream_fldnames(nf) is in
          fldbun_model = shr_strdata_get_stream_fieldbundle(sdat, ns, 'model')

          call ESMF_FieldBundleGet(fldbun_model, fieldName=trim(strm_flds(nf)), isPresent=isPresent, rc=rc)
          if (ispresent) then
             ! if field is present in stream - determine the index in the field bundle of this field
             dfield_new%stream_indices(nf) = ns
             call ESMF_FieldBundleGet(fldbun_model, fieldCount=fieldCount, rc=rc)
             if (chkerr(rc,__LINE__,u_FILE_u)) return
             allocate(lfieldnamelist(fieldCount))
             call ESMF_FieldBundleGet(fldbun_model, fieldNameList=lfieldnamelist, itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
             if (chkerr(rc,__LINE__,u_FILE_u)) return

             do n = 1,fieldcount
                if (trim(strm_flds(nf)) == trim(lfieldnamelist(n))) then
                   dfield_new%fldbun_indices(nf) = n
                   if (mainproc) then
                      write(logunit,*)'(dshr_addfield_add) using stream field strm_'//&
                           trim(strm_flds(nf))//' for 2d '//trim(state_fld)
                   end if
                end if
             end do

             deallocate(lfieldnamelist)
             exit ! go to the next fld
          end if
       end do
    end do

    ! Set export state array pointer
    call dshr_state_getfldptr(State, fldname=trim(state_fld), fldptr2=dfield_new%state_data2d, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    dfield_new%state_data2d(:,:) = 0._r8
    if (mainproc) then
       write(logunit,*)'(dshr_addfield_add) setting pointer for export state '//trim(state_fld)
    end if
    state_ptr => dfield_new%state_data2d

  end subroutine dshr_dfield_add_2d_stateptr

  !===============================================================================
  subroutine dshr_dfield_copy(dfields, sdat, rc)

    ! Copy stream data into dfield data type for each element of dfields
    ! This routine will populate the export state data
    ! (dfield%state_data1d or dfield%state_data2d) with the stream field data

    ! input/output variables
    type(dfield_type)      , pointer     :: dfields
    type(shr_strdata_type) , intent(in)  :: sdat
    integer                , intent(out) :: rc

    ! local variables
    type(ESMF_FieldBundle)     :: fldbun_model
    type(ESMF_field)           :: lfield
    type(dfield_type), pointer :: dfield
    real(r8), pointer          :: data1d(:)
    real(r8), pointer          :: data2d(:,:)
    integer                    :: nf
    integer                    :: fldbun_index
    integer                    :: stream_index
    integer                    :: ungriddedUBound_output(1)
    integer                    :: ungriddedCount
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Loop over all dfield entries and fill in stream_data and state_data1d or state_data2d arrays
    dfield => dfields ! note that dfields is the head of the linked list
    do while (associated(dfield))
       ! Map the stream data to the state data
       if (associated(dfield%state_data1d)) then
          stream_index = dfield%stream_index
          fldbun_index = dfield%fldbun_index
          if (stream_index /= iunset .and. fldbun_index /= iunset) then
             fldbun_model = shr_strdata_get_stream_fieldbundle(sdat, stream_index, 'model')
             call dshr_fldbun_getfieldn(fldbun_model, fldbun_index, lfield, rc=rc)
             if (chkerr(rc,__LINE__,u_FILE_u)) return
             call dshr_field_getfldptr(lfield, fldptr1=data1d, rc=rc)
             if (chkerr(rc,__LINE__,u_FILE_u)) return
             dfield%state_data1d(:) = data1d(:)
          end if
       else if (associated(dfield%state_data2d)) then
          do nf = 1,size(dfield%stream_indices)
             stream_index = dfield%stream_indices(nf)
             fldbun_index = dfield%fldbun_indices(nf)
             if (stream_index > 0) then
                fldbun_model = shr_strdata_get_stream_fieldbundle(sdat, stream_index, 'model')
                call dshr_fldbun_getfieldn(fldbun_model, fldbun_index, lfield, rc=rc)
                if (chkerr(rc,__LINE__,u_FILE_u)) return
                call ESMF_FieldGet(lfield, ungriddedUBound=ungriddedUBound_output, rc=rc)
                if (chkerr(rc,__LINE__,u_FILE_u)) return
                ungriddedCount = ungriddedUBound_output(1)
                if (ungriddedCount > 0) then
                   call dshr_field_getfldptr(lfield, fldptr2=data2d, rc=rc)
                   if (chkerr(rc,__LINE__,u_FILE_u)) return
                   dfield%state_data2d(:,:) = data2d(:,:)
                else
                   call dshr_field_getfldptr(lfield, fldptr1=data1d, rc=rc)
                   if (chkerr(rc,__LINE__,u_FILE_u)) return
                   dfield%state_data2d(nf,:) = data1d(:)
                end if
             endif
          end do
       end if
       dfield => dfield%next

    end do

  end subroutine dshr_dfield_copy

end module dshr_dfield_mod
