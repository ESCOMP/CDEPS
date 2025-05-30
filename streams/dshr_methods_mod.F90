module dshr_methods_mod

  ! Share methods for data model functionality

  use ESMF         , only : ESMF_State, ESMF_Field, ESMF_StateGet, ESMF_FieldBundle
  use ESMF         , only : ESMF_LogWrite, ESMF_SUCCESS, ESMF_FAILURE
  use ESMF         , only : ESMF_StateRemove, ESMF_StateGet, ESMF_RouteHandle
  use ESMF         , only : ESMF_Region_Flag, ESMF_FieldStatus_Flag, ESMF_LOGMSG_INFO
  use ESMF         , only : ESMF_MAXSTR, ESMF_LOGERR_PASSTHRU
  use ESMF         , only : ESMF_FieldBundleGet, ESMF_FieldBundleAdd, ESMF_FieldGet
  use ESMF         , only : ESMF_REGION_TOTAL, ESMF_END_ABORT, ESMF_ITEMORDER_ADDORDER
  use ESMF         , only : ESMF_LogFoundError, ESMF_FieldRegrid, ESMF_Finalize, ESMF_FIELDSTATUS_COMPLETE
  use ESMF         , only : ESMF_TERMORDER_SRCSEQ, operator(/=)
  use ESMF         , only : ESMF_TraceRegionEnter, ESMF_TraceRegionExit
  use shr_kind_mod , only : r8=>shr_kind_r8, cs=>shr_kind_cs, cl=>shr_kind_cl
  use shr_log_mod  , only : shr_log_error
  
  implicit none
  public

  public :: dshr_state_getfldptr
  public :: dshr_state_diagnose
  public :: dshr_fldbun_getFldPtr
  public :: dshr_fldbun_regrid
  public :: dshr_fldbun_getFieldN
  public :: dshr_fldbun_getNameN
  public :: dshr_fldbun_fldchk
  public :: dshr_fldbun_diagnose
  public :: dshr_field_getfldptr
  public :: chkerr
  public :: memcheck

  character(len=1024) :: msgString
  integer, parameter  :: memdebug_level=1
  character(*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine dshr_state_getfldptr(State, fldname, fldptr1, fldptr2, allowNullReturn, rc)

    ! ----------------------------------------------
    ! Get pointer to a state field
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_State) ,          intent(in)              :: State
    character(len=*) ,          intent(in)              :: fldname
    real(R8)         , pointer, intent(inout), optional :: fldptr1(:)
    real(R8)         , pointer, intent(inout), optional :: fldptr2(:,:)
    logical          ,          intent(in),optional     :: allowNullReturn
    integer          ,          intent(out)             :: rc

    ! local variables
    type(ESMF_Field)            :: lfield
    integer                     :: itemCount
    character(len=*), parameter :: subname='(dshr_state_getfldptr)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    if (present(allowNullReturn)) then
      call ESMF_StateGet(State, itemSearch=trim(fldname), itemCount=itemCount, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return

      ! check field is in the state or not?
      if (itemCount >= 1) then
        call ESMF_StateGet(State, itemName=trim(fldname), field=lfield, rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return

        call dshr_field_getfldptr(lfield, fldptr1=fldptr1, fldptr2=fldptr2, rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return
      else
        ! the call to just returns if it cannot find the field
        call ESMF_LogWrite(trim(subname)//" Could not find the field: "//trim(fldname)//" just returning", ESMF_LOGMSG_INFO)
      end if
    else
      call ESMF_StateGet(State, itemName=trim(fldname), field=lfield, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return

      call dshr_field_getfldptr(lfield, fldptr1=fldptr1, fldptr2=fldptr2, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
    end if

  end subroutine dshr_state_getfldptr

  !===============================================================================
  subroutine dshr_state_diagnose(State, flds_scalar_name, string, rc)

    ! ----------------------------------------------
    ! Diagnose status of State
    ! ----------------------------------------------

    type(ESMF_State), intent(in)  :: state
    character(len=*), intent(in)  :: flds_scalar_name
    character(len=*), intent(in)  :: string
    integer         , intent(out) :: rc

    ! local variables
    integer                         :: n
    type(ESMf_Field)                :: lfield
    integer                         :: fieldCount, lrank
    character(ESMF_MAXSTR) ,pointer :: lfieldnamelist(:)
    real(r8), pointer               :: dataPtr1d(:)
    real(r8), pointer               :: dataPtr2d(:,:)
    character(len=*),parameter      :: subname='(dshr_state_diagnose)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_StateGet(state, itemCount=fieldCount, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    allocate(lfieldnamelist(fieldCount))
    call ESMF_StateGet(state, itemNameList=lfieldnamelist, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    do n = 1, fieldCount
       call ESMF_StateGet(state, itemName=lfieldnamelist(n), field=lfield, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       if (trim(lfieldnamelist(n)) /= trim(flds_scalar_name)) then

          call dshr_field_getfldptr(lfield, fldptr1=dataPtr1d, fldptr2=dataPtr2d, rank=lrank, rc=rc)
          if (chkerr(rc,__LINE__,u_FILE_u)) return

          if (lrank == 0) then
             ! no local data
          elseif (lrank == 1) then
             if (size(dataPtr1d) > 0) then
                write(msgString,'(A,3g14.7,i8)') trim(string)//': '//trim(lfieldnamelist(n)), &
                     minval(dataPtr1d), maxval(dataPtr1d), sum(dataPtr1d), size(dataPtr1d)
             else
                write(msgString,'(A,a)') trim(string)//': '//trim(lfieldnamelist(n))," no data"
             endif
          elseif (lrank == 2) then
             if (size(dataPtr2d) > 0) then
                write(msgString,'(A,3g14.7,i8)') trim(string)//': '//trim(lfieldnamelist(n)), &
                     minval(dataPtr2d), maxval(dataPtr2d), sum(dataPtr2d), size(dataPtr2d)
             else
                write(msgString,'(A,a)') trim(string)//': '//trim(lfieldnamelist(n))," no data"
             endif
          else
             call shr_log_error(trim(subname)//": ERROR rank not supported ", rc=rc)
             return
          endif
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
       end if
    enddo

    deallocate(lfieldnamelist)

  end subroutine dshr_state_diagnose

  !===============================================================================
  subroutine dshr_fldbun_GetFldPtr(FB, fldname, fldptr1, fldptr2, rank, field, rc)

    ! ----------------------------------------------
    ! Get pointer to a field bundle field
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_FieldBundle) , intent(in)              :: FB
    character(len=*)       , intent(in)              :: fldname
    real(R8), pointer      , intent(inout), optional :: fldptr1(:)
    real(R8), pointer      , intent(inout), optional :: fldptr2(:,:)
    integer                , intent(out),   optional :: rank
    type(ESMF_Field)       , intent(out),   optional :: field
    integer                , intent(out)             :: rc

    ! local variables
    integer           :: lrank
    type(ESMF_Field)  :: lfield
    integer           :: ungriddedUBound(1)
    character(len=*), parameter :: subname='(dshr_fldbun_GetFldPtr)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    if (.not. dshr_fldbun_FldChk(FB, trim(fldname), rc=rc)) then
       call shr_log_error(trim(subname)//": ERROR field "//trim(fldname)//" not in FB ", rc=rc)
       return
    endif
    call ESMF_FieldBundleGet(FB, fieldName=trim(fldname), field=lfield, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldGet(lfield, ungriddedUBound=ungriddedUBound, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    if (ungriddedUBound(1) > 0) then
       if (.not.present(fldptr2)) then
          call shr_log_error(trim(subname)//": ERROR missing rank=2 array ", &
               line=__LINE__, file=u_FILE_u, rc=rc)
          return
       endif
       call ESMF_FieldGet(lfield, farrayptr=fldptr2, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       lrank = 2
    else
       if (.not.present(fldptr1)) then
          call shr_log_error(trim(subname)//": ERROR missing rank=1 array ", &
               line=__LINE__, file=u_FILE_u, rc=rc)
          return
       endif
       call ESMF_FieldGet(lfield, farrayptr=fldptr1, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       lrank = 1
    end if
    if (present(rank)) rank = lrank
    if (present(field)) field = lfield

  end subroutine dshr_fldbun_GetFldPtr

  !===============================================================================
  subroutine dshr_fldbun_regrid(FBsrc, FBdst, RH, zeroregion, rc)

    ! ----------------------------------------------
    ! Assumes that FBin and FBout contain fields with the same name
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_FieldBundle), intent(inout)        :: FBsrc
    type(ESMF_FieldBundle), intent(inout)        :: FBdst
    type(ESMF_RouteHandle), intent(inout)        :: RH
    type(ESMF_Region_Flag), intent(in), optional :: zeroregion
    integer               , intent(out)          :: rc

    ! local
    integer                    :: n
    type(ESMF_Region_Flag)     :: localzr
    type(ESMF_Field)           :: field_src
    type(ESMF_Field)           :: field_dst
    integer                    :: fieldcount_src
    integer                    :: fieldcount_dst
    character(ESMF_MAXSTR), allocatable :: lfieldNameList_src(:)
    character(ESMF_MAXSTR), allocatable :: lfieldNameList_dst(:)
    character(len=*),parameter :: subname='(dshr_fldbun_FieldRegrid)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_TraceRegionEnter(subname)

    localzr = ESMF_REGION_TOTAL
    if (present(zeroregion)) then
       localzr = zeroregion
    endif

    call ESMF_FieldBundleGet(FBsrc, fieldCount=fieldCount_src, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    allocate(lfieldNameList_src(fieldCount_src))
    call ESMF_FieldBundleGet(FBsrc, fieldNameList=lfieldNameList_src, itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldBundleGet(FBdst, fieldCount=fieldCount_dst, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    allocate(lfieldNameList_dst(fieldCount_dst))
    call ESMF_FieldBundleGet(FBdst, fieldNameList=lfieldNameList_dst, itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! check that input and output field bundles have identical number of fields
    if (fieldcount_src /= fieldcount_dst) then
       call ESMF_LogWrite(trim(subname)//": ERROR fieldcount_src and field_count_dst are not the same")
       rc = ESMF_FAILURE
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) then
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
       end if
    end if

    do n = 1,fieldCount_src
       call ESMF_FieldBundleGet(FBsrc, fieldName=trim(lfieldnamelist_src(n)), field=field_src, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       call ESMF_FieldBundleGet(FBdst, fieldName=trim(lfieldnamelist_dst(n)), field=field_dst, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       call ESMF_FieldRegrid(field_src, field_dst, routehandle=RH, &
            termorderflag=ESMF_TERMORDER_SRCSEQ, checkflag=.false., zeroregion=localzr, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
    end do

    deallocate(lfieldnamelist_src)
    deallocate(lfieldnamelist_dst)

    call ESMF_TraceRegionExit(subname)

  end subroutine dshr_fldbun_regrid

  !===============================================================================
  subroutine dshr_fldbun_getFieldN(FB, fieldnum, field, rc)

    ! ----------------------------------------------
    ! Get field with number fieldnum in input field bundle FB
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_FieldBundle), intent(in)    :: FB
    integer               , intent(in)    :: fieldnum
    type(ESMF_Field)      , intent(inout) :: field
    integer               , intent(out)   :: rc

    ! local variables
    character(len=ESMF_MAXSTR) :: name
    character(len=*),parameter :: subname='(dshr_fldbun_getFieldN)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    call dshr_fldbun_getNameN(FB, fieldnum, name, rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldBundleGet(FB, fieldName=name, field=field, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

  end subroutine dshr_fldbun_getFieldN

  !===============================================================================
  subroutine dshr_fldbun_getNameN(FB, fieldnum, fieldname, rc)

    ! ----------------------------------------------
    ! Get name of field number fieldnum in input field bundle FB
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_FieldBundle), intent(in)    :: FB
    integer               , intent(in)    :: fieldnum
    character(len=*)      , intent(out)   :: fieldname
    integer               , intent(out)   :: rc

    ! local variables
    integer                         :: fieldCount
    character(ESMF_MAXSTR) ,pointer :: lfieldnamelist(:)
    character(len=*),parameter      :: subname='(dshr_fldbun_getNameN)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    fieldname = ' '
    call ESMF_FieldBundleGet(FB, fieldCount=fieldCount, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    if (fieldnum > fieldCount) then
       call shr_log_error(trim(subname)//": ERROR fieldnum > fieldCount ", rc=rc)
       return
    endif

    allocate(lfieldnamelist(fieldCount))
    call ESMF_FieldBundleGet(FB, fieldNameList=lfieldnamelist, itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    fieldname = lfieldnamelist(fieldnum)
    deallocate(lfieldnamelist)

  end subroutine dshr_fldbun_getNameN

  !===============================================================================
  logical function dshr_fldbun_FldChk(FB, fldname, rc)

    ! ----------------------------------------------
    ! Determine if field with fldname is in input field bundle
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_FieldBundle), intent(in)  :: FB
    character(len=*)      , intent(in)  :: fldname
    integer               , intent(out) :: rc

    ! local variables
    logical                     :: isPresent
    character(len=*), parameter :: subname='(dshr_fldbun_FldChk)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    ! If field bundle is created determine if fldname is present in field bundle
    dshr_fldbun_FldChk = .false.

    call ESMF_FieldBundleGet(FB, fieldName=trim(fldname), isPresent=isPresent, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) then
       call shr_log_error(trim(subname)//" Error checking field: "//trim(fldname), rc=rc)
       return
    endif

    if (isPresent) then
       dshr_fldbun_FldChk = .true.
    endif

  end function dshr_fldbun_FldChk

  !===============================================================================
  subroutine dshr_fldbun_Field_diagnose(FB, fieldname, string, rc)

    ! ----------------------------------------------
    ! Diagnose status of State
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_FieldBundle), intent(inout)  :: FB
    character(len=*), intent(in)           :: fieldname
    character(len=*), intent(in), optional :: string
    integer         , intent(out)          :: rc

    ! local variables
    integer           :: lrank
    character(len=CS) :: lstring
    real(R8), pointer :: dataPtr1d(:)
    real(R8), pointer :: dataPtr2d(:,:)
    character(len=*),parameter      :: subname='(dshr_fldbun_FieldDiagnose)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    lstring = ''
    if (present(string)) lstring = trim(string)

    call dshr_fldbun_GetFldPtr(FB, fieldname, dataPtr1d, dataPtr2d, lrank, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    if (lrank == 0) then
       ! no local data
    elseif (lrank == 1) then
       if (size(dataPtr1d) > 0) then
          write(msgString,'(A,3g14.7,i8)') trim(subname)//' '//trim(lstring)//': '//trim(fieldname), &
               minval(dataPtr1d), maxval(dataPtr1d), sum(dataPtr1d), size(dataPtr1d)
       else
          write(msgString,'(A,a)') trim(subname)//' '//trim(lstring)//': '//trim(fieldname)," no data"
       endif
    elseif (lrank == 2) then
       if (size(dataPtr2d) > 0) then
          write(msgString,'(A,3g14.7,i8)') trim(subname)//' '//trim(lstring)//': '//trim(fieldname), &
               minval(dataPtr2d), maxval(dataPtr2d), sum(dataPtr2d), size(dataPtr2d)
       else
          write(msgString,'(A,a)') trim(subname)//' '//trim(lstring)//': '//trim(fieldname)," no data"
       endif
    else
       call shr_log_error(trim(subname)//": ERROR rank not supported ", rc=rc)
       return
    endif
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

  end subroutine dshr_fldbun_Field_diagnose


 !===============================================================================
  subroutine dshr_fldbun_diagnose(FB, string, rc)

    ! ----------------------------------------------
    ! Diagnose status of FB
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_FieldBundle) , intent(inout)        :: FB
    character(len=*)       , intent(in), optional :: string
    integer                , intent(out)          :: rc

    ! local variables
    integer                         :: n
    integer                         :: fieldCount, lrank
    character(ESMF_MAXSTR), pointer :: lfieldnamelist(:)
    character(len=CL)               :: lstring
    real(R8), pointer               :: dataPtr1d(:)
    real(R8), pointer               :: dataPtr2d(:,:)
    character(len=*), parameter     :: subname='(dshr_fldbun_diagnose)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    lstring = ''
    if (present(string)) lstring = trim(string) // ' '

    ! Determine number of fields in field bundle and allocate memory for lfieldnamelist
    call ESMF_FieldBundleGet(FB, fieldCount=fieldCount, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    allocate(lfieldnamelist(fieldCount))

    ! Get the fields in the field bundle
    call ESMF_FieldBundleGet(FB, fieldNameList=lfieldnamelist, itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! For each field in the bundle, get its memory location and print out the field
    do n = 1, fieldCount
       call dshr_fldbun_GetFldPtr(FB, lfieldnamelist(n), fldptr1=dataPtr1d, fldptr2=dataPtr2d, rank=lrank, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       if (lrank == 0) then
          ! no local data

       elseif (lrank == 1) then
          if (size(dataPtr1d) > 0) then
             write(msgString,'(A,3g14.7,i8)') trim(subname)//' '//trim(lstring)//': '//trim(lfieldnamelist(n))//' ', &
                  minval(dataPtr1d), maxval(dataPtr1d), sum(dataPtr1d), size(dataPtr1d)
          else
             write(msgString,'(A,a)') trim(subname)//' '//trim(lstring)//': '//trim(lfieldnamelist(n)), " no data"
          endif

       elseif (lrank == 2) then
          if (size(dataPtr2d) > 0) then
             write(msgString,'(A,3g14.7,i8)') trim(subname)//' '//trim(lstring)//': '//trim(lfieldnamelist(n))//' ', &
                  minval(dataPtr2d), maxval(dataPtr2d), sum(dataPtr2d), size(dataPtr2d)
          else
             write(msgString,'(A,a)') trim(subname)//' '//trim(lstring)//': '//trim(lfieldnamelist(n)), &
                  " no data"
          endif

       else
          call shr_log_error(trim(subname)//": ERROR rank not supported ", rc=rc)
          return
       endif
       call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

    ! Deallocate memory
    deallocate(lfieldnamelist)

    call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO)

  end subroutine dshr_fldbun_diagnose

  !===============================================================================
  subroutine dshr_field_getfldptr(field, fldptr1, fldptr2, rank, abort, rc)

    ! ----------------------------------------------
    ! for a field, determine rank and return fldptr1 or fldptr2
    ! abort is true by default and will abort if fldptr is not yet allocated in field
    ! rank returns 0, 1, or 2.  0 means fldptr not allocated and abort=false
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_Field)  , intent(in)              :: field
    real(r8), pointer , intent(inout), optional :: fldptr1(:)
    real(r8), pointer , intent(inout), optional :: fldptr2(:,:)
    integer           , intent(out)  , optional :: rank
    logical           , intent(in)   , optional :: abort
    integer           , intent(out)             :: rc

    ! local variables
    type(ESMF_FieldStatus_Flag) :: status
    integer                     :: ungriddedUBound(1)
    integer                     :: lrank
    logical                     :: labort
    character(len=CS)           :: name
    character(len=*), parameter :: subname='(field_getfldptr)'
    ! ----------------------------------------------
    rc = ESMF_SUCCESS
    lrank = 0
    labort = .true.
    if (present(abort)) then
       labort = abort
    endif

    call ESMF_FieldGet(field, status=status, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    if (status /= ESMF_FIELDSTATUS_COMPLETE) then
       if (labort) then
          call ESMF_FieldGet(field, name=name, rc=rc)
          if (chkerr(rc,__LINE__,u_FILE_u)) return
          call shr_log_error(trim(subname)//": field "//trim(name)//" has no data not allocated ", rc=rc)
          return
       else
          call ESMF_LogWrite(trim(subname)//": WARNING data not allocated ", ESMF_LOGMSG_INFO, rc=rc)
       endif
    else
        call ESMF_FieldGet(field, ungriddedUBound=ungriddedUBound, rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return
        if (ungriddedUBound(1) > 0) then
           if (.not.present(fldptr2)) then
              call shr_log_error(trim(subname)//": ERROR missing rank=2 array for "//trim(name), &
                   line=__LINE__, file=u_FILE_u, rc=rc)
              return
           endif
           call ESMF_FieldGet(field, farrayptr=fldptr2, rc=rc)
           if (chkerr(rc,__LINE__,u_FILE_u)) return
           lrank = 2
        else
           if (.not.present(fldptr1)) then
              call shr_log_error(trim(subname)//": ERROR missing rank=1 array for "//trim(name), &
                   line=__LINE__, file=u_FILE_u, rc=rc)
              return
           endif
           call ESMF_FieldGet(field, farrayptr=fldptr1, rc=rc)
           if (chkerr(rc,__LINE__,u_FILE_u)) return
           lrank = 1
        end if
    endif  ! status
    if (present(rank)) rank = lrank

  end subroutine dshr_field_getfldptr

  !===============================================================================
  subroutine memcheck(string, level, maintask)

    ! input/output variables
    character(len=*) , intent(in) :: string
    integer          , intent(in) :: level
    logical          , intent(in) :: maintask

    ! local variables
#ifdef CESMCOUPLED
    integer :: ierr
    integer, external :: GPTLprint_memusage
#endif
    !-----------------------------------------------------------------------

#ifdef CESMCOUPLED
    if ((maintask .and. memdebug_level > level) .or. memdebug_level > level+1) then
       ierr = GPTLprint_memusage(string)
    endif
#endif

  end subroutine memcheck

  !===============================================================================
  logical function chkerr(rc, line, file)
    integer, intent(in) :: rc
    integer, intent(in) :: line
    character(len=*), intent(in) :: file
    integer :: lrc
    !-----------------------------------------------------------------------
    chkerr = .false.
    lrc = rc
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=line, file=file)) then
       chkerr = .true.
    endif
  end function chkerr

end module dshr_methods_mod
