module dshr_stream_mod

  ! -------------------------------------------------------------------------------
  ! Data type and methods to manage input data streams.
  ! A "data stream" is a sequence of input files where each file contains the
  ! same set of data fields and all the data fields are on the same grid.
  ! The sequence of input data files provides an uninterupted time series of
  ! data.
  !
  ! A stream data type stores information about one data stream, including the
  ! range of data date years to use and how data dates align with model dates.
  !
  ! Given a model date, this module can return data dates that are upper and
  ! lower time bounds around the given model date and the names of the files
  ! containing those dates.
  ! -------------------------------------------------------------------------------

  use shr_kind_mod     , only : r8=>shr_kind_r8, cs=>shr_kind_cs, cl=>shr_kind_cl, cxx=>shr_kind_cxx
  use shr_sys_mod      , only : shr_sys_abort
  use shr_const_mod    , only : shr_const_cday
  use shr_string_mod   , only : shr_string_leftalign_and_convert_tabs, shr_string_parseCFtunit
  use shr_cal_mod      , only : shr_cal_noleap
  use shr_cal_mod      , only : shr_cal_date2ymd
  use shr_cal_mod      , only : shr_cal_ymd2date
  use shr_cal_mod      , only : shr_cal_calendarName
  use shr_cal_mod      , only : shr_cal_advDate
  use shr_cal_mod      , only : shr_cal_advdateint
  use shr_cal_mod      , only : shr_cal_leapyear
  use dshr_methods_mod , only : chkerr
  use pio              , only : pio_noerr, pio_seterrorhandling, pio_inq_att, pio_openfile, pio_closefile
  use pio              , only : file_desc_t, pio_inq_varid, iosystem_desc_t, pio_file_is_open
  use pio              , only : pio_nowrite, pio_inquire_dimension, pio_inquire_variable, pio_bcast_error
  use pio              , only : pio_get_att, pio_get_var
#ifdef CESMCOUPLED
  use shr_pio_mod      , only : shr_pio_getiosys, shr_pio_getiotype, shr_pio_getioformat
#endif

  implicit none
  private ! default private

  ! !PUBLIC TYPES:
  public :: shr_stream_streamType        ! stream data type with private components

  ! !PUBLIC MEMBER FUNCTIONS:
  public :: shr_stream_init_from_esmfconfig
#ifndef DISABLE_FoX
  public :: shr_stream_init_from_xml
#endif
  public :: shr_stream_init_from_inline  ! initial stream type
  public :: shr_stream_findBounds        ! return lower/upper bounding date info
  public :: shr_stream_getMeshFileName   ! return stream filename
  public :: shr_stream_getModelFieldList ! return model field name list
  public :: shr_stream_getStreamFieldList! return stream file field name list
  public :: shr_stream_getPrevFileName   ! return previous file in sequence
  public :: shr_stream_getNextFileName   ! return next file in sequence
  public :: shr_stream_getNFiles         ! get the number of files in a stream
  public :: shr_stream_getCalendar       ! get the stream calendar
  public :: shr_stream_getCurrFile       ! get the currfile, fileopen, and currpioid
  public :: shr_stream_getData           ! get stream data from target file
  public :: shr_stream_setCurrFile       ! set the currfile, fileopen, and currpioid
  public :: shr_stream_dataDump          ! internal stream data for debugging
  public :: shr_stream_restIO            ! read or write to netcdf restart file

  character(CS),parameter,public :: shr_stream_file_null    = 'not_set'

  ! valid values for time extrapoloation
  character(CS),parameter,public :: shr_stream_taxis_cycle  = 'cycle'
  character(CS),parameter,public :: shr_stream_taxis_extend = 'extend'
  character(CS),parameter,public :: shr_stream_taxis_limit  = 'limit'

  ! valid values for time interpolation
  character(CS),parameter,public :: shr_stream_tinterp_lower   = 'lower'
  character(CS),parameter,public :: shr_stream_tinterp_upper   = 'upper'
  character(CS),parameter,public :: shr_stream_tinterp_nearest = 'nearest'
  character(CS),parameter,public :: shr_stream_tinterp_linear  = 'linear'
  character(CS),parameter,public :: shr_stream_tinterp_coszen  = 'coszen'

  ! valid values for mapping interpolation
  character(CS),parameter,public :: shr_stream_mapalgo_bilinear = 'bilinear'
  character(CS),parameter,public :: shr_stream_mapalgo_redist   = 'redist'
  character(CS),parameter,public :: shr_stream_mapalgo_nn       = 'nn'
  character(CS),parameter,public :: shr_stream_mapalgo_consf    = 'consf'
  character(CS),parameter,public :: shr_stream_mapalgo_consd    = 'consd'
  character(CS),parameter,public :: shr_stream_mapalgo_none     = 'none'

  ! a useful derived type to use inside shr_streamType ---
  type shr_stream_file_type
     character(CL)         :: name = shr_stream_file_null ! the file name (full pathname)
     logical               :: haveData = .false.          ! has t-coord data been read in?
     integer               :: nt = 0                      ! size of time dimension
     integer  ,allocatable :: date(:)                     ! t-coord date: yyyymmdd
     integer  ,allocatable :: secs(:)                     ! t-coord secs: elapsed on date
     type(file_desc_t)     :: fileid
  end type shr_stream_file_type

  type shr_stream_data_variable
     character(CS) :: nameinfile
     character(CS) :: nameinmodel
  end type shr_stream_data_variable

  type shr_stream_streamType
     !private ! no public access to internal components
     type(iosystem_desc_t), pointer :: pio_subsystem
     integer           :: pio_iotype
     integer           :: pio_ioformat
     integer           :: logunit                               ! stdout log unit
     logical           :: init         = .false.                ! has stream been initialized
     integer           :: nFiles       = 0                      ! number of data files
     integer           :: yearFirst    = -1                     ! first year to use in t-axis (yyyymmdd)
     integer           :: yearLast     = -1                     ! last  year to use in t-axis (yyyymmdd)
     integer           :: yearAlign    = -1                     ! align yearFirst with this model year
     character(CS)     :: lev_dimname  = 'null'                 ! name of vertical dimension if any
     character(CS)     :: taxMode      = shr_stream_taxis_cycle ! cycling option for time axis
     character(CS)     :: tInterpAlgo  = 'linear'               ! algorithm to use for time interpolation
     character(CS)     :: mapalgo      = 'bilinear'             ! type of mapping - default is 'bilinear'
     character(CS)     :: readMode     = 'single'               ! stream read model - 'single' or 'full_file'
     real(r8)          :: dtlimit      = 1.5_r8                 ! delta time ratio limits for time interpolation
     integer           :: offset       = 0                      ! offset in seconds of stream data
     character(CS)     :: calendar     = shr_cal_noleap         ! stream calendar (obtained from first stream data file)
     character(CL)     :: meshFile     = ' '                    ! filename for mesh for all fields on stream (full pathname)
     integer           :: k_lvd        = -1                     ! file/sample of least valid date
     integer           :: n_lvd        = -1                     ! file/sample of least valid date
     logical           :: found_lvd    = .false.                ! T <=> k_lvd,n_lvd have been set
     integer           :: k_gvd        = -1                     ! file/sample of greatest valid date
     integer           :: n_gvd        = -1                     ! file/sample of greatest valid date
     logical           :: found_gvd    = .false.                ! T <=> k_gvd,n_gvd have been set
     logical           :: fileopen     = .false.                ! is current file open
     character(CL)     :: currfile     = ' '                    ! current filename
     integer           :: nvars                                 ! number of stream variables
     character(CL)     :: stream_vectors = 'null'               ! stream vectors names
     type(file_desc_t) :: currpioid                             ! current pio file desc
     type(shr_stream_file_type)    , allocatable :: file(:)     ! filenames of stream data files (full pathname)
     type(shr_stream_data_variable), allocatable :: varlist(:)  ! stream variable names (on file and in model)
  end type shr_stream_streamType

  !----- parameters -----
  integer                  :: debug = 0            ! edit/turn-on for debug write statements
  real(R8)     , parameter :: spd = shr_const_cday ! seconds per day
  character(*) , parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

#ifndef DISABLE_FoX
  subroutine shr_stream_init_from_xml(streamfilename, streamdat, isroot_task, logunit, &
                                      pio_subsystem, io_type, io_format, compname, rc)
    use FoX_DOM, only : extractDataContent, destroy, Node, NodeList, parseFile, getElementsByTagname
    use FoX_DOM, only : getLength, item
    use ESMF, only : ESMF_VM, ESMF_VMGetCurrent, ESMF_VMBroadCast, ESMF_SUCCESS

    ! ---------------------------------------------------------------------
    ! The xml format of a stream txt file will look like the following
    ! <?xml version="1.0"?>
    ! <file id="stream" version="1.0">
    !   <stream_info>
    !    <taxmode></taxmode>
    !    <tintalgo></tintalgo>
    !    <mapalgo></mapalgo>
    !    <readmode></readmode>
    !    <dtlimit></dtlimit>
    !    <year_first></year_first>
    !    <year_last></year_last>
    !    <year_align></year_align>
    !    <vectors></vectors>
    !    <meshfile></meshfile>
    !    <lev_dimname></lev_dimname>
    !    <data_files>
    !      <file></file>
    !    </data_files>
    !    <data_variables>
    !      <var></var>
    !    </data_variables>
    !    <offset></offset>
    !  </stream_info>
    ! </file>
    ! ---------------------------------------------------------------------

    ! input/output variables
    character(len=*), optional  , intent(in)             :: streamfilename
    type(shr_stream_streamType) , intent(inout), pointer :: streamdat(:)
    logical                     , intent(in)             :: isroot_task
    integer                     , intent(in)             :: logunit
    type(iosystem_desc_t)       , intent(in), pointer    :: pio_subsystem
    integer                     , intent(in)             :: io_type
    integer                     , intent(in)             :: io_format
    character(len=*)            , intent(in)             :: compname
    integer                     , intent(out)            :: rc

    ! local variables
    type(ESMF_VM)            :: vm
    type(Node)     , pointer :: Sdoc, p, streamnode
    type(NodeList) , pointer :: streamlist, filelist, varlist
    character(len=CL)        :: tmpstr
    integer                  :: i, n, nstrms
    integer                  :: status
    integer                  :: tmp(6)
    real(r8)                 :: rtmp(1)
    character(*),parameter   :: subName = '(shr_stream_init_from_xml) '
    ! --------------------------------------------------------

    rc = ESMF_SUCCESS

    nstrms = 0

    if (isroot_task) then

       Sdoc => parseFile(streamfilename, iostat=status)
       if (status /= 0) then
          call shr_sys_abort("Could not parse file "//trim(streamfilename))
       endif
       streamlist => getElementsByTagname(Sdoc, "stream_info")
       nstrms = getLength(streamlist)

       ! allocate an array of shr_streamtype objects on just isroot_task
       allocate(streamdat(nstrms))

       ! fill in non-default values for the streamdat attributes
       do i= 1, nstrms
          streamnode => item(streamlist, i-1)

          p => item(getElementsByTagname(streamnode, "taxmode"), 0)
          if (associated(p)) then
             call extractDataContent(p, streamdat(i)%taxmode)
             if (streamdat(i)%taxmode /= shr_stream_taxis_cycle   .and. &
                 streamdat(i)%taxmode /= shr_stream_taxis_extend  .and. &
                 streamdat(i)%taxmode /= shr_stream_taxis_limit) then
                call shr_sys_abort("tintalgo must have a value of either cycle, extend or limit")
             end if
          endif

          p => item(getElementsByTagname(streamnode, "mapalgo"), 0)
          if (associated(p)) then
             call extractDataContent(p, streamdat(i)%mapalgo)
             if (streamdat(i)%mapalgo /= shr_stream_mapalgo_bilinear .and. &
                 streamdat(i)%mapalgo /= shr_stream_mapalgo_redist   .and. &
                 streamdat(i)%mapalgo /= shr_stream_mapalgo_nn       .and. &
                 streamdat(i)%mapalgo /= shr_stream_mapalgo_consf    .and. &
                 streamdat(i)%mapalgo /= shr_stream_mapalgo_consd    .and. &
                 streamdat(i)%mapalgo /= shr_stream_mapalgo_none) then
                call shr_sys_abort("mapaglo must have a value of either bilinear, redist, nn, consf or consd")
             end if
          endif

          p => item(getElementsByTagname(streamnode, "tintalgo"), 0)
          if (associated(p)) then
             call extractDataContent(p, streamdat(i)%tInterpAlgo)
             if (streamdat(i)%tInterpAlgo /= shr_stream_tinterp_lower   .and. &
                 streamdat(i)%tInterpAlgo /= shr_stream_tinterp_upper   .and. &
                 streamdat(i)%tInterpAlgo /= shr_stream_tinterp_nearest .and. &
                 streamdat(i)%tInterpAlgo /= shr_stream_tinterp_linear  .and. &
                 streamdat(i)%tInterpAlgo /= shr_stream_tinterp_coszen) then
                call shr_sys_abort("tintalgo must have a value of either lower, upper, nearest, linear or coszen")
             end if
          endif

          p => item(getElementsByTagname(streamnode, "readmode"), 0)
          if (associated(p)) then
             call extractDataContent(p, streamdat(i)%readMode)
          endif

          p=> item(getElementsByTagname(streamnode, "year_first"), 0)
          if(associated(p)) then
             call extractDataContent(p, streamdat(i)%yearFirst)
          else
             call shr_sys_abort("yearFirst must be provided")
          endif

          p=> item(getElementsByTagname(streamnode, "year_last"), 0)
          if(associated(p)) then
             call extractDataContent(p, streamdat(i)%yearLast)
          else
             call shr_sys_abort("yearLast must be provided")
          endif

          p=> item(getElementsByTagname(streamnode, "year_align"), 0)
          if(associated(p)) then
             call extractDataContent(p, streamdat(i)%yearAlign)
          else
             call shr_sys_abort("yearAlign must be provided")
          endif

          p=> item(getElementsByTagname(streamnode, "dtlimit"), 0)
          if(associated(p)) then
             call extractDataContent(p, streamdat(i)%dtlimit)
          endif

          p=> item(getElementsByTagname(streamnode, "offset"), 0)
          if(associated(p)) then
             call extractDataContent(p, streamdat(i)%offset)
          endif

          p=> item(getElementsByTagname(streamnode, "meshfile"), 0)
          if (associated(p)) then
             call extractDataContent(p, streamdat(i)%meshfile)
          else
             call shr_sys_abort("mesh file name must be provided")
          endif

          p => item(getElementsByTagname(streamnode, "vectors"), 0)
          if (associated(p)) then
             call extractDataContent(p, streamdat(i)%stream_vectors)
          else
             call shr_sys_abort("stream vectors must be provided")
          endif

          ! Determine name of vertical dimension
          p => item(getElementsByTagname(streamnode, "lev_dimname"), 0)
          if (associated(p)) then
             call extractDataContent(p, streamdat(i)%lev_dimname)
          else
             call shr_sys_abort("stream vertical level dimension name must be provided")
          endif

          ! Determine input data files
          p => item(getElementsByTagname(streamnode, "datafiles"), 0)
          if (.not. associated(p)) then
             call shr_sys_abort("stream data files must be provided")
          endif
          filelist => getElementsByTagname(p,"file")
          streamdat(i)%nfiles = getLength(filelist)
          allocate(streamdat(i)%file( streamdat(i)%nfiles))
          do n=1, streamdat(i)%nfiles
             p => item(filelist, n-1)
             call extractDataContent(p, streamdat(i)%file(n)%name)
          enddo

          ! Determine name(s) of stream variable(s) in file and model
          p => item(getElementsByTagname(streamnode, "datavars"), 0)
          varlist => getElementsByTagname(p, "var")
          streamdat(i)%nvars = getLength(varlist)
          allocate(streamdat(i)%varlist(streamdat(i)%nvars))
          do n = 1, streamdat(i)%nvars
             p => item(varlist, n-1)
             call extractDataContent(p, tmpstr)
             streamdat(i)%varlist(n)%nameinfile = tmpstr(1:index(tmpstr, " "))
             streamdat(i)%varlist(n)%nameinmodel = tmpstr(index(trim(tmpstr), " ", .true.)+1:)
          enddo

       enddo
#ifndef CPRPGI
! PGI compiler has an issue with this call (empty procedure)
       call destroy(Sdoc)
#endif
    endif

    ! allocate streamdat instance on all tasks
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    tmp(1) = nstrms
    call ESMF_VMBroadCast(vm, tmp, 1, 0, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    nstrms = tmp(1)
    if (.not. isroot_task) then
       allocate(streamdat(nstrms))
    endif

    ! broadcast the contents of streamdat from the main task  to all tasks
    do i=1,nstrms
       tmp(1) = streamdat(i)%nfiles
       tmp(2) = streamdat(i)%nvars
       tmp(3) = streamdat(i)%yearFirst
       tmp(4) = streamdat(i)%yearLast
       tmp(5) = streamdat(i)%yearAlign
       tmp(6) = streamdat(i)%offset
       call ESMF_VMBroadCast(vm, tmp, 6, 0, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       streamdat(i)%nfiles    = tmp(1)
       streamdat(i)%nvars     = tmp(2)
       streamdat(i)%yearFirst = tmp(3)
       streamdat(i)%yearLast  = tmp(4)
       streamdat(i)%yearAlign = tmp(5)
       streamdat(i)%offset    = tmp(6)
       if(.not. isroot_task) then
          allocate(streamdat(i)%file(streamdat(i)%nfiles))
          allocate(streamdat(i)%varlist(streamdat(i)%nvars))
       endif
       do n=1,streamdat(i)%nfiles
          call ESMF_VMBroadCast(vm, streamdat(i)%file(n)%name, CL, 0, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       enddo
       do n=1,streamdat(i)%nvars
          call ESMF_VMBroadCast(vm, streamdat(i)%varlist(n)%nameinfile, CS, 0, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          call ESMF_VMBroadCast(vm, streamdat(i)%varlist(n)%nameinmodel, CS, 0, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       enddo
       call ESMF_VMBroadCast(vm, streamdat(i)%meshfile,     CL, 0, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_VMBroadCast(vm, streamdat(i)%lev_dimname,  CS, 0, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_VMBroadCast(vm, streamdat(i)%taxmode,      CS, 0, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_VMBroadCast(vm, streamdat(i)%readmode,     CS, 0, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_VMBroadCast(vm, streamdat(i)%tinterpAlgo,  CS, 0, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_VMBroadCast(vm, streamdat(i)%stream_vectors,  CL, 0, rc=rc)

       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_VMBroadCast(vm, streamdat(i)%mapalgo,      CS, 0, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       rtmp(1) = streamdat(i)%dtlimit
       call ESMF_VMBroadCast(vm, rtmp, 1, 0, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       streamdat(i)%dtlimit = rtmp(1)
#ifdef CESMCOUPLED
       ! Initialize stream pio
       streamdat(i)%pio_subsystem => shr_pio_getiosys(trim(compname))
       streamdat(i)%pio_iotype    =  shr_pio_getiotype(trim(compname))
       streamdat(i)%pio_ioformat  =  shr_pio_getioformat(trim(compname))
       ! This is to avoid an unused dummy argument warning
       if(.false.) then
          if(associated(pio_subsystem)) print *, io_type, io_format
       endif
#else
       streamdat(i)%pio_subsystem => pio_subsystem
       streamdat(i)%pio_iotype = io_type
       streamdat(i)%pio_ioformat = io_format
#endif
       ! Set logunit
       streamdat(i)%logunit = logunit

       call shr_stream_getCalendar(streamdat(i), 1, streamdat(i)%calendar)

       ! Error check
       if (trim(streamdat(i)%taxmode) == shr_stream_taxis_extend .and. streamdat(i)%dtlimit < 1.e10) then
          call shr_sys_abort(trim(subName)//" ERROR: if taxmode value is extend set dtlimit to 1.e30")
       end if
       ! initialize flag that stream has been set
       streamdat(i)%init = .true.
    enddo


  end subroutine shr_stream_init_from_xml

#endif

  !===============================================================================

  subroutine shr_stream_init_from_inline(streamdat, &
       pio_subsystem, io_type, io_format, &
       stream_meshfile, stream_lev_dimname, stream_mapalgo, &
       stream_yearFirst, stream_yearLast, stream_yearAlign, &
       stream_offset, stream_taxmode, stream_tintalgo, stream_dtlimit, &
       stream_fldlistFile, stream_fldListModel, stream_fileNames, &
       logunit, compname)

    ! --------------------------------------------------------
    ! set values of stream datatype independent of a reading in a stream text file
    ! this is used to initialize a stream directly from fortran interface
    ! --------------------------------------------------------

    ! input/output variables
    type(shr_stream_streamType) ,pointer, intent(inout)  :: streamdat(:)           ! data streams (assume 1 below)
    type(iosystem_desc_t)       ,pointer, intent(in)     :: pio_subsystem          ! data structure required for pio operations
    integer                     ,intent(in)              :: io_type                ! data format
    integer                     ,intent(in)              :: io_format              ! netcdf format
    character(*)                ,intent(in)              :: stream_meshFile        ! full pathname to stream mesh file
    character(*)                ,intent(in)              :: stream_lev_dimname     ! name of vertical dimension in stream
    character(*)                ,intent(in)              :: stream_mapalgo         ! stream mesh -> model mesh mapping type
    integer                     ,intent(in)              :: stream_yearFirst       ! first year to use
    integer                     ,intent(in)              :: stream_yearLast        ! last  year to use
    integer                     ,intent(in)              :: stream_yearAlign       ! align yearFirst with this model year
    character(*)                ,intent(in)              :: stream_tintalgo        ! time interpolation algorithm
    integer                     ,intent(in)              :: stream_offset          ! offset in seconds of stream data
    character(*)                ,intent(in)              :: stream_taxMode         ! time axis mode
    real(r8)                    ,intent(in)              :: stream_dtlimit         ! ratio of max/min stream delta times
    character(*)                ,intent(in)              :: stream_fldListFile(:)  ! file field names, colon delim list
    character(*)                ,intent(in)              :: stream_fldListModel(:) ! model field names, colon delim list
    character(*)                ,intent(in)              :: stream_filenames(:)    ! stream data filenames (full pathnamesa)
    integer                     ,intent(in)              :: logunit                ! stdout unit
    character(len=*)            ,intent(in)              :: compname               ! component name (e.g. ATM, OCN...)

    ! local variables
    integer                :: n
    integer                :: nfiles
    integer                :: nvars
    character(CS)          :: calendar ! stream calendar
    character(*),parameter :: subName = '(shr_stream_init_from_inline) '
    ! --------------------------------------------------------

    ! Assume only 1 stream
    allocate(streamdat(1))

    ! overwrite default values
    streamdat(1)%meshFile     = trim(stream_meshFile)
    streamdat(1)%lev_dimname  = trim(stream_lev_dimname)
    streamdat(1)%mapalgo      = trim(stream_mapalgo)

    streamdat(1)%yearFirst    = stream_yearFirst
    streamdat(1)%yearLast     = stream_yearLast
    streamdat(1)%yearAlign    = stream_yearAlign

    streamdat(1)%tinterpAlgo  = trim(stream_tintalgo)
    streamdat(1)%offset       = stream_offset
    streamdat(1)%taxMode      = trim(stream_taxMode)
    streamdat(1)%dtlimit      = stream_dtlimit
#ifdef CESMCOUPLED
    ! Initialize stream pio
    streamdat(1)%pio_subsystem => shr_pio_getiosys(trim(compname))
    streamdat(1)%pio_iotype    =  shr_pio_getiotype(trim(compname))
    streamdat(1)%pio_ioformat  =  shr_pio_getioformat(trim(compname))
    ! This is to avoid an unused dummy argument warning
    if(.false.) then
       if(associated(pio_subsystem)) print *, io_type, io_format
    endif
#else
    streamdat(1)%pio_subsystem => pio_subsystem
    streamdat(1)%pio_iotype = io_type
    streamdat(1)%pio_ioformat = io_format
#endif

    ! initialize stream filenames
    if (allocated(streamdat(1)%file)) then
       deallocate(streamdat(1)%file)
    end if
    nfiles = size(stream_filenames)
    streamdat(1)%nfiles = nfiles
    allocate(streamdat(1)%file(nfiles))
    do n = 1, nfiles
       streamdat(1)%file(n)%name = trim(stream_filenames(n))
    enddo

    ! Determine name of stream variables in file and model
    nvars = size(stream_fldlistFile)
    streamdat(1)%nvars = nvars
    allocate(streamdat(1)%varlist(nvars))
    do n = 1, nvars
       streamdat(1)%varlist(n)%nameinfile  = trim(stream_fldlistFile(n))
       streamdat(1)%varlist(n)%nameinmodel = trim(stream_fldlistModel(n))
    end do

    ! Initialize logunit
    streamdat(:)%logunit = logunit
    ! Get stream calendar
    call shr_stream_getCalendar(streamdat(1), 1, calendar)
    streamdat(1)%calendar = trim(calendar)

    ! Initialize flag that stream has been set
    streamdat(1)%init = .true.

  end subroutine shr_stream_init_from_inline

  !===============================================================================
  subroutine shr_stream_init_from_esmfconfig(streamfilename, streamdat, logunit,   &
                            pio_subsystem, io_type, io_format, rc)

    use esmf             , only : ESMF_VM, ESMF_VMGetCurrent, ESMF_VMBroadCast
    use esmf             , only : ESMF_SUCCESS, ESMF_ConfigCreate, ESMF_ConfigLoadFile
    use esmf             , only : ESMF_ConfigGetLen, ESMF_ConfigGetAttribute
    use esmf             , only : ESMF_Config, ESMF_MAXSTR

    !!---------------------------------------------------------------------
    !! The configuration file is a text file that can have following entries
    !! file_id: "stream"
    !! file_version: 1.0
    !! stream_info: 1
    !! taxmode:
    !! tInterpAlgo:
    !! readMode:
    !! mapalgo:
    !! dtlimit:
    !! yearFirst:
    !! yearLast:
    !! yearAlign:
    !! stream_vectors:
    !! stream_mesh_file:
    !! stream_lev_dimname:
    !! stream_data_files:
    !! stream_data_variables:
    !! stream_offset:
    !!---------------------------------------------------------------------

    ! input/output variables
    character(len=*), optional  , intent(in)             :: streamfilename
    type(shr_stream_streamType) , intent(inout), pointer :: streamdat(:)
    integer                     , intent(in)             :: logunit
    type(iosystem_desc_t)       , intent(in), pointer    :: pio_subsystem
    integer                     , intent(in)             :: io_type
    integer                     , intent(in)             :: io_format
    integer                     , intent(out)            :: rc

    ! local variables
    type(ESMF_VM)            :: vm
    type(ESMF_Config)        :: cf
    integer                  :: i, n, nstrms
    character(2)             :: mystrm
    character(*),parameter   :: subName = '(shr_stream_init_from_esmfconfig)'
    character(len=ESMF_MAXSTR), allocatable :: strm_tmpstrings(:)
    character(*) , parameter :: u_FILE_u = __FILE__

    ! ---------------------------------------------------------------------

    rc = ESMF_SUCCESS

    nstrms = 0

    ! allocate streamdat instance on all tasks
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! set ESMF config
    cf =  ESMF_ConfigCreate(rc=RC)
    call ESMF_ConfigLoadFile(config=CF ,filename=trim(streamfilename), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return


    ! get number of streams
    nstrms = ESMF_ConfigGetLen(config=CF, label='stream_info:', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! allocate an array of shr_stream_streamtype objects on just isroot_task
    if( nstrms > 0 ) then
      allocate(streamdat(nstrms))
    else
      call shr_sys_abort("no stream_info in config file "//trim(streamfilename))
    endif

    ! fill in non-default values for the streamdat attributes
    do i=1, nstrms
      write(mystrm,"(I2.2)") i
      call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%taxmode,label="taxmode"//mystrm//':', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%mapalgo,label="mapalgo"//mystrm//':', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%tInterpAlgo,label="tInterpAlgo"//mystrm//':', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%readMode,label="readMode"//mystrm//':', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      if( ESMF_ConfigGetLen(config=CF, label="yearFirst"//mystrm//':', rc=rc) > 0 ) then
        call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%yearFirst,label="yearFirst"//mystrm//':', rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      else
        call shr_sys_abort("yearFirst must be provided")
      endif

      if( ESMF_ConfigGetLen(config=CF, label="yearLast"//mystrm//':', rc=rc) > 0 ) then
        call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%yearLast,label="yearLast"//mystrm//':', rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      else
        call shr_sys_abort("yearLast must be provided")
      endif

      if( ESMF_ConfigGetLen(config=CF, label="yearAlign"//mystrm//':', rc=rc) > 0 ) then
        call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%yearAlign,label="yearAlign"//mystrm//':', rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      else
        call shr_sys_abort("yearAlign must be provided")
      endif

      call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%dtlimit,label="dtlimit"//mystrm//':', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%offset,label="stream_offset"//mystrm//':', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      if( ESMF_ConfigGetLen(config=CF, label="stream_mesh_file"//mystrm//':', rc=rc) > 0 ) then
        call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%meshfile,label="stream_mesh_file"//mystrm//':', rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      else
        call shr_sys_abort("stream_mesh_file must be provided")
      endif

      if( ESMF_ConfigGetLen(config=CF, label="stream_vectors"//mystrm//':', rc=rc) > 0 ) then
        call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%stream_vectors,label="stream_vectors"//mystrm//':', rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      else
        call shr_sys_abort("stream_vectors must be provided")
      endif

      if( ESMF_ConfigGetLen(config=CF, label="stream_lev_dimname"//mystrm//':', rc=rc) > 0 ) then
        call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%lev_dimname,label="stream_lev_dimname"//mystrm//':', rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      else
        call shr_sys_abort("stream_lev_dimname must be provided")
      endif

      ! Get a list of stream file names
      streamdat(i)%nfiles = ESMF_ConfigGetLen(config=CF, label="stream_data_files"//mystrm//':', rc=rc)
      if( streamdat(i)%nfiles > 0) then
        allocate(streamdat(i)%file( streamdat(i)%nfiles))
        allocate(strm_tmpstrings(streamdat(i)%nfiles))
        call ESMF_ConfigGetAttribute(CF,valueList=strm_tmpstrings, label="stream_data_files"//mystrm//':', rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        do n=1,streamdat(i)%nfiles
          streamdat(i)%file(n)%name = trim(strm_tmpstrings(n))
        enddo
        deallocate(strm_tmpstrings)
      else
        call shr_sys_abort("stream data files must be provided")
      endif

      ! Get name of stream variables in file and model
      streamdat(i)%nvars = ESMF_ConfigGetLen(config=CF, label="stream_data_variables"//mystrm//':', rc=rc)
      if( streamdat(i)%nvars > 0) then
        allocate(streamdat(i)%varlist(streamdat(i)%nvars))
        allocate(strm_tmpstrings(streamdat(i)%nvars))
        call ESMF_ConfigGetAttribute(CF,valueList=strm_tmpstrings,label="stream_data_variables"//mystrm//':', rc=rc)
        do n=1, streamdat(i)%nvars
          streamdat(i)%varlist(n)%nameinfile = strm_tmpstrings(n)(1:index(trim(strm_tmpstrings(n)), " "))
          streamdat(i)%varlist(n)%nameinmodel = strm_tmpstrings(n)(index(trim(strm_tmpstrings(n)), " ", .true.)+1:)
        enddo
        deallocate(strm_tmpstrings)
      else
        call shr_sys_abort("stream data variables must be provided")
      endif

      ! Initialize stream pio
      streamdat(i)%pio_subsystem => pio_subsystem
      streamdat(i)%pio_iotype = io_type
      streamdat(i)%pio_ioformat = io_format
      ! Set logunit
      streamdat(i)%logunit = logunit

      call shr_stream_getCalendar(streamdat(i), 1, streamdat(i)%calendar)

      ! Error check
      if (trim(streamdat(i)%taxmode) == shr_stream_taxis_extend .and.  streamdat(i)%dtlimit < 1.e10) then
        call shr_sys_abort(trim(subName)//" ERROR: if taxmode value is extend set dtlimit to 1.e30")
      end if

    enddo ! end loop nstrm

    ! initialize flag that stream has been set
    streamdat(:)%init = .true.

  end subroutine shr_stream_init_from_esmfconfig
  !===============================================================================
  subroutine shr_stream_findBounds(strm, mDateIn, secIn, isroot_task, &
       mDateLB, dDateLB, secLB, n_lb, fileLB,  mDateUB, dDateUB, secUB, n_ub, fileUB)

    !-------------------------------------------------------------------------------
    ! Given a stream and a model date, find time coordinates of the upper and
    ! lower time bounds surrounding the models date.  Returns the model date,
    ! data date, elasped seconds, time index, and file names associated with
    ! these upper and lower time bounds.
    !   1) take the model date, map it into the data date range
    !   2) find the upper and lower bounding data dates
    !   3) return the bounding data and model dates, file names, & t-coord indicies
    !-------------------------------------------------------------------------------

    ! input/output parameters:
    type(shr_stream_streamType) ,intent(inout):: strm        ! data stream to query
    integer                     ,intent(in)   :: mDateIn     ! model date (yyyymmdd)
    integer                     ,intent(in)   :: secIn       ! elapsed sec on model date
    logical                     ,intent(in)   :: isroot_task ! is mpi task root communicator task
    integer                     ,intent(out)  :: mDateLB     ! model date    of LB
    integer                     ,intent(out)  :: dDateLB     ! data  date    of LB
    integer                     ,intent(out)  :: secLB       ! elap sec      of LB
    integer                     ,intent(out)  :: n_lb        ! t-coord index of LB
    character(*)                ,intent(out)  :: fileLB      ! file containing  LB
    integer                     ,intent(out)  :: mDateUB     ! model date    of UB
    integer                     ,intent(out)  :: dDateUB     ! data  date    of UB
    integer                     ,intent(out)  :: secUB       ! elap sec      of UB
    integer                     ,intent(out)  :: n_ub        ! t-coord index of UB
    character(*)                ,intent(out)  :: fileUB      ! file containing  UB

    ! local variables
    integer  :: dDateIn       ! model date mapped onto a data date
    integer  :: dDateF        ! first date
    integer  :: dDateL        ! last date
    integer  :: n,nf          ! loop index wrt t-coord array within one file
    integer  :: k,kf          ! loop index wrt list of files
    integer  :: k_ub,k_lb     ! file index of U/L bounds
    integer  :: rCode         ! return code
    integer  :: mYear         ! year of model date
    integer  :: yrFirst       ! first year of data loop
    integer  :: yrLast        ! last year of data loop
    integer  :: yrAlign       ! model year that aligns with yearFirst
    integer  :: nYears        ! number of years in data loop
    integer  :: dYear         ! data year corresponding to model year
    integer  :: yy,mm,dd      ! year,month,day
    real(R8) :: rDateIn       ! model dDateIn + secs/(secs per day)
    real(R8) :: rDate1        ! stream dDateIn + secs/(secs per day)
    real(R8) :: rDate2        ! stream dDateIn + secs/(secs per day)
    real(R8) :: rDatelvd      ! lvd dDate + secs/(secs per day)
    real(R8) :: rDategvd      ! gvd dDate + secs/(secs per day)
    logical  :: cycle         ! is cycling on or off
    logical  :: limit         ! is limiting on or off
    character(*),parameter :: subName = '(shr_stream_findBounds) '
    character(*),parameter :: F00   = "('(shr_stream_findBounds) ',8a)"
    character(*),parameter :: F01   = "('(shr_stream_findBounds) ',a,i9.8,a)"
    character(*),parameter :: F02   = "('(shr_stream_findBounds) ',a,2i9.8,i6,i5,1x,a)"
    character(*),parameter :: F03   = "('(shr_stream_findBounds) ',a,i4)"
    character(*),parameter :: F04   = "('(shr_stream_findBounds) ',2a,i4)"
    !-------------------------------------------------------------------------------

    if (debug>0 .and. isroot_task) then
       write(strm%logunit,F02) "DEBUG: ---------- enter ------------------"
    end if

    if ( .not. strm%init ) then
       call shr_sys_abort(trim(subName)//" ERROR: trying to find bounds of uninitialized stream")
    end if

    if (trim(strm%taxMode) == trim(shr_stream_taxis_cycle)) then
       cycle = .true.
       limit = .false.
    elseif (trim(strm%taxMode) == trim(shr_stream_taxis_extend)) then
       cycle = .false.
       limit = .false.
    elseif (trim(strm%taxMode) == trim(shr_stream_taxis_limit)) then
       cycle = .false.
       limit = .true.
    else
       write(strm%logunit,*) trim(subName),' ERROR: illegal taxMode = ',trim(strm%taxMode)
       call shr_sys_abort(trim(subName)//' ERROR: illegal taxMode = '//trim(strm%taxMode))
    endif

    !----------------------------------------------------------------------------
    ! convert/map the model year/date into a data year/date
    ! note: these values will be needed later to convert data year to model year
    !----------------------------------------------------------------------------
    mYear   = mDateIn/10000             ! assumes/require F90 truncation
    yrFirst = strm%yearFirst            ! first year in data sequence
    yrLast  = strm%yearLast             ! last year in data sequence
    yrAlign = strm%yearAlign            ! model year corresponding to yearFirst
    nYears  = yrLast - yrFirst + 1      ! number of years in data sequence
    dDateF  = yrFirst * 10000 + 101     ! first date in valid range
    dDateL  = (yrLast+1)  * 10000 + 101 ! last date in valid range
    n = 0
    if (cycle) then
       dYear  = yrFirst + modulo(mYear-yrAlign+(2*nYears),nYears)   ! current data year
       if(debug>0 .and. isroot_task) then
          write(strm%logunit, *) trim(subname), ' dyear, yrfirst, myear, yralign, nyears =', dyear, yrfirst, myear, yralign, nyears
       endif
    else
       dYear  = yrFirst + mYear - yrAlign
    endif

    if (dYear < 0) then
       write(strm%logunit,*) trim(subName),' ERROR: dyear lt zero = ',dYear
       call shr_sys_abort(trim(subName)//' ERROR: dyear lt zero')
    endif

    dDateIn = dYear*10000 + modulo(mDateIn,10000) ! mDateIn mapped to range of data years
    rDateIn = dDateIn + secIn/spd                 ! dDateIn + fraction of a day
    if (debug>0 .and. isroot_task) then
       write(strm%logunit,'(a,2(i8,2x),2(f20.4,2x))') 'mYear,dYear,dDateIn,rDateIn  = ',mYear,dYear,dDateIn,rDateIn
       write(strm%logunit,'(a,2(i8,2x),2(f20.4,2x))') 'yrFirst,yrLast,yrAlign,nYears= ',yrFirst,yrLast,yrAlign,nYears
    endif

    !----------------------------------------------------------------------------
    ! find least valid date (lvd)
    !----------------------------------------------------------------------------

    if (.not. strm%found_lvd) then
       A:    do k=1,strm%nFiles
          if (.not. strm%file(k)%haveData) then
             call shr_stream_readtCoord(strm, k, isroot_task, rCode)
             if ( rCode /= 0 )then
                call shr_sys_abort(trim(subName)//" ERROR: readtCoord1")
             end if
          end if
          do n=1,strm%file(k)%nt
             if ( dDateF <= strm%file(k)%date(n) ) then
                !--- found a date in or beyond yearFirst ---
                strm%k_lvd = k
                strm%n_lvd = n
                strm%found_lvd = .true.
                exit A
             end if
          end do
       end do A
       if (.not. strm%found_lvd) then
          write(strm%logunit,F00)  "ERROR: LVD not found, all data is before yearFirst"
          call shr_sys_abort(trim(subName)//" ERROR: LVD not found, all data is before yearFirst")
       else
          !--- LVD is in or beyond yearFirst, verify it is not beyond yearLast ---
          if ( dDateL <= strm%file(strm%k_lvd)%date(strm%n_lvd) ) then
             write(strm%logunit,F00)  "ERROR: LVD not found, all data is after yearLast"
             call shr_sys_abort(trim(subName)//" ERROR: LVD not found, all data is after yearLast")
          end if
       end if
       if (debug>1 .and. isroot_task ) then
          if (strm%found_lvd) write(strm%logunit,F01) " found LVD = ",strm%file(k)%date(n)
       end if
    end if

    if (strm%found_lvd) then
       k = strm%k_lvd
       n = strm%n_lvd
       rDatelvd = strm%file(k)%date(n) + strm%file(k)%secs(n)/spd ! LVD date + frac day
    else
       write(strm%logunit,F00)  "ERROR: LVD not found yet"
       call shr_sys_abort(trim(subName)//" ERROR: LVD not found yet")
    endif

    if (strm%found_gvd) then
       k = strm%k_gvd
       n = strm%n_gvd
       rDategvd = strm%file(k)%date(n) + strm%file(k)%secs(n)/spd ! GVD date + frac day
    else
       rDategvd = 99991231.0
    endif
    if (debug>0 .and. isroot_task) then
       write(strm%logunit,'(a,3(f20.4,2x))') 'rDateIn,rDatelvd,rDategvd = ',rDateIn,rDatelvd,rDategvd
    endif

    !-----------------------------------------------------------
    ! dateIn < rDatelvd
    !   limit -> abort
    !   extend -> use lvd value, set LB to 00000101
    !   cycle -> lvd is UB, gvd is LB, shift mDateLB by -nYears
    !-----------------------------------------------------------

    if (rDateIn < rDatelvd) then
       if (limit) then
          write(strm%logunit,*)  trim(subName)," ERROR: limit on and rDateIn lt rDatelvd",rDateIn,rDatelvd
          call shr_sys_abort(trim(subName)//" ERROR: rDateIn lt rDatelvd limit true")
       endif

       if (.not.cycle) then
          k_lb = strm%k_lvd
          n_lb = strm%n_lvd
          dDateLB = 00000101
          mDateLB = 00000101
          secLB   = 0
          fileLB  = strm%file(k_lb)%name

          k_ub = strm%k_lvd
          n_ub = strm%n_lvd
          dDateUB = strm%file(k_ub)%date(n_ub)
          call shr_cal_date2ymd(dDateUB,yy,mm,dd)
          yy = yy + (mYear-dYear)
          call shr_cal_ymd2date(yy,mm,dd,mDateUB)
          secUB = strm%file(k_ub)%secs(n_ub)
          fileUB = strm%file(k_ub)%name
          return
       endif

       if (cycle) then
          !--- find greatest valid date (GVD) ---
          if (.not. strm%found_gvd) then
             !--- start search at last file & move toward first file ---
             B: do k=strm%nFiles,1,-1
                !--- read data for file number k ---
                if (.not. strm%file(k)%haveData) then
                   call shr_stream_readtCoord(strm, k, isroot_task, rCode)
                   if ( rCode /= 0 )then
                      call shr_sys_abort(trim(subName)//" ERROR: readtCoord2")
                   end if
                end if
                !--- start search at greatest date & move toward least date ---
                do n=strm%file(k)%nt,1,-1
                   if ( strm%file(k)%date(n) < dDateL ) then
                      strm%k_gvd = k
                      strm%n_gvd = n
                      strm%found_gvd = .true.
                      rDategvd = strm%file(k)%date(n) + strm%file(k)%secs(n)/spd ! GVD date + frac day
                      if (debug>1 .and. isroot_task) then
                         write(strm%logunit,F01) " found GVD ",strm%file(k)%date(n)
                      end if
                      exit B
                   end if
                end do
             end do B
          end if

          if (.not. strm%found_gvd) then
             write(strm%logunit,F00)  "ERROR: GVD not found1"
             call shr_sys_abort(trim(subName)//" ERROR: GVD not found1")
          endif

          k_lb = strm%k_gvd
          n_lb = strm%n_gvd
          dDateLB = strm%file(k_lb)%date(n_lb)
          call shr_cal_date2ymd(dDateLB,yy,mm,dd)
          yy = yy + (mYear-dYear-nYears)
          call shr_cal_ymd2date(yy,mm,dd,mDateLB)
          secLB   = strm%file(k_lb)%secs(n_lb)
          fileLB  = strm%file(k_lb)%name

          k_ub = strm%k_lvd
          n_ub = strm%n_lvd
          dDateUB = strm%file(k_ub)%date(n_ub)
          call shr_cal_date2ymd(dDateUB,yy,mm,dd)
          yy = yy + (mYear-dYear)
          call shr_cal_ymd2date(yy,mm,dd,mDateUB)
          secUB   = strm%file(k_ub)%secs(n_ub)
          fileUB  = strm%file(k_ub)%name

          return
       endif

       !-----------------------------------------------------------
       ! dateIn > rDategvd
       !   limit -> abort
       !   extend -> use gvd value, set UB to 99991231
       !   cycle -> lvd is UB, gvd is LB, shift mDateLB by +nYears
       !-----------------------------------------------------------

    else if (strm%found_gvd .and. rDateIn >= rDategvd) then
       if (limit) then
          write(strm%logunit,*) trim(subName)," ERROR: limit on and rDateIn gt rDategvd",rDateIn,rDategvd
          call shr_sys_abort(trim(subName)//" ERROR: rDateIn gt rDategvd limit true")
       endif

       if (.not.cycle) then
          k_lb = strm%k_gvd
          n_lb = strm%n_gvd
          dDateLB = strm%file(k_lb)%date(n_lb)
          call shr_cal_date2ymd(dDateLB,yy,mm,dd)
          yy = yy + (mYear-dYear)
          call shr_cal_ymd2date(yy,mm,dd,mDateLB)
          secLB   = strm%file(k_lb)%secs(n_lb)
          fileLB  = strm%file(k_lb)%name

          k_ub = strm%k_gvd
          n_ub = strm%n_gvd
          dDateUB = 99991231
          mDateUB = 99991231
          secUB   = 0
          fileUB  = strm%file(k_ub)%name
          return
       endif

       if (cycle) then
          k_lb = strm%k_gvd
          n_lb = strm%n_gvd
          dDateLB = strm%file(k_lb)%date(n_lb)
          call shr_cal_date2ymd(dDateLB,yy,mm,dd)
          yy = yy + (mYear-dYear)
          call shr_cal_ymd2date(yy,mm,dd,mDateLB)
          secLB   = strm%file(k_lb)%secs(n_lb)
          fileLB  = strm%file(k_lb)%name

          k_ub = strm%k_lvd
          n_ub = strm%n_lvd
          dDateUB = strm%file(k_ub)%date(n_ub)
          call shr_cal_date2ymd(dDateUB,yy,mm,dd)
          yy = yy + (mYear-dYear+nYears)
          call shr_cal_ymd2date(yy,mm,dd,mDateUB)
          secUB   = strm%file(k_ub)%secs(n_ub)
          fileUB  = strm%file(k_ub)%name
          return
       endif

    else

       !-----------------------------------------------------------
       ! dateIn > rDatelvd
       !-----------------------------------------------------------
       k_lb = strm%k_lvd
       n_lb = strm%n_lvd
       C:    do k=strm%k_lvd,strm%nFiles
          !--- read data for file number k ---
          if (.not. strm%file(k)%haveData) then
             call shr_stream_readtCoord(strm, k, isroot_task, rCode)
             if ( rCode /= 0 )then
                call shr_sys_abort(trim(subName)//" ERROR: readtCoord3")
             end if
          end if
          !--- examine t-coords for file k ---
          n      = strm%file(k)%nt                                 ! last t-index in file
          rDate1 = strm%file(k)%date(n) + strm%file(k)%secs(n)/spd ! last date + frac day

          if (.not. strm%found_gvd) then
             n = strm%file(k)%nt
             if (dDateL <= strm%file(k)%date(n)) then
                !--- set gvd to last timestep in previous file then advance through current file ---
                if (k > 1) then
                   strm%k_gvd = k-1
                   strm%n_gvd = strm%file(k-1)%nt
                   strm%found_gvd = .true.
                endif
                do n=1,strm%file(k)%nt
                   if ( strm%file(k)%date(n) < dDateL ) then
                      strm%k_gvd = k
                      strm%n_gvd = n
                      strm%found_gvd = .true.
                   endif
                enddo
             elseif (k == strm%nFiles) then
                strm%k_gvd = k
                strm%n_gvd = strm%file(k)%nt
                strm%found_gvd = .true.
             end if
             if (strm%found_gvd) then
                kf = strm%k_gvd
                nf = strm%n_gvd
                rDategvd = strm%file(kf)%date(nf) + strm%file(kf)%secs(nf)/spd ! GVD date + frac day
             endif
          end if

          !-----------------------------------------------------------
          ! dateIn > rDategvd
          !   limit -> abort
          !   extend -> use gvd value, set UB to 99991231
          !   cycle -> lvd is UB, gvd is LB, shift mDateLB by nYears
          !-----------------------------------------------------------

          if (strm%found_gvd .and. rDateIn >= rDategvd) then
             if (limit) then
                write(strm%logunit,*) trim(subName)," ERROR: limit on and rDateIn gt rDategvd",rDateIn,rDategvd
                call shr_sys_abort(trim(subName)//" ERROR: rDateIn gt rDategvd limit true")
             endif

             if (.not.cycle) then
                k_lb = strm%k_gvd
                n_lb = strm%n_gvd
                dDateLB = strm%file(k_lb)%date(n_lb)
                call shr_cal_date2ymd(dDateLB,yy,mm,dd)
                yy = yy + (mYear-dYear)
                call shr_cal_ymd2date(yy,mm,dd,mDateLB)
                secLB   = strm%file(k_lb)%secs(n_lb)
                fileLB  = strm%file(k_lb)%name

                k_ub = strm%k_gvd
                n_ub = strm%n_gvd
                dDateUB = 99991231
                mDateUB = 99991231
                secUB   = 0
                fileUB  = strm%file(k_ub)%name
                return
             endif

             if (cycle) then
                k_lb = strm%k_gvd
                n_lb = strm%n_gvd
                dDateLB = strm%file(k_lb)%date(n_lb)
                call shr_cal_date2ymd(dDateLB,yy,mm,dd)
                yy = yy + (mYear-dYear)
                call shr_cal_ymd2date(yy,mm,dd,mDateLB)
                secLB   = strm%file(k_lb)%secs(n_lb)
                fileLB  = strm%file(k_lb)%name

                k_ub = strm%k_lvd
                n_ub = strm%n_lvd
                dDateUB = strm%file(k_ub)%date(n_ub)
                call shr_cal_date2ymd(dDateUB,yy,mm,dd)
                yy = yy + (mYear-dYear+nYears)
                call shr_cal_ymd2date(yy,mm,dd,mDateUB)
                secUB   = strm%file(k_ub)%secs(n_ub)
                fileUB  = strm%file(k_ub)%name
                return
             endif

          endif

          if ( rDate1 < rDateIn ) then
             !--- increment lb and continue to search ---
             k_lb = k
             n_lb = strm%file(k)%nt
          else
             !--- the greatest lower-bound is in file k, find it ---
             do n=1,strm%file(k)%nt
                rDate2 = strm%file(k)%date(n) + strm%file(k)%secs(n)/spd ! date + frac day
                if ( rDate2 <= rDateIn ) then
                   !--- found another/greater lower-bound ---
                   k_lb = k
                   n_lb = n
                else
                   !--- found the least upper-bound ---
                   k_ub = k
                   n_ub = n

                   dDateLB = strm%file(k_lb)%date(n_lb)
                   call shr_cal_date2ymd(dDateLB,yy,mm,dd)
                   yy = yy + (mYear-dYear)
                   call shr_cal_ymd2date(yy,mm,dd,mDateLB)
                   secLB = strm%file(k_lb)%secs(n_lb)
                   fileLB = strm%file(k_lb)%name

                   dDateUB = strm%file(k_ub)%date(n_ub)
                   call shr_cal_date2ymd(dDateUB,yy,mm,dd)
                   yy = yy + (mYear-dYear)
                   if(mm == 2 .and. dd==29 .and. .not. shr_cal_leapyear(yy)) then
                      if(isroot_task) write(strm%logunit, *) 'Found leapyear mismatch', myear, dyear, yy
                      mm = 3
                      dd = 1
                   endif
                   call shr_cal_ymd2date(yy,mm,dd,mDateUB)
                   secUB = strm%file(k_ub)%secs(n_ub)
                   fileUB = strm%file(k_ub)%name
                   return
                endif
             enddo
          endif
       end do C
    endif

    call shr_sys_abort(trim(subName)//' ERROR: findBounds failed')

  end subroutine shr_stream_findBounds

  !===============================================================================
  subroutine shr_stream_readTCoord(strm, k, isroot_task, rc)

    ! Read in time coordinates with possible offset (require that time coordinate is 'time')

    ! input/output parameters:
    type(shr_stream_streamType) ,intent(inout) :: strm ! data stream to query
    integer                     ,intent(in)    :: k    ! stream file index
    logical                     ,intent(in)    :: isroot_task
    integer,optional            ,intent(out)   :: rc   ! return code

    ! local variables
    character(CL)          :: fileName    ! filename to read
    integer                :: nt
    integer                :: num,n
    integer                :: din,dout
    integer                :: sin,sout,offin
    integer                :: lrc
    integer                :: vid,ndims,rcode
    integer,allocatable    :: dids(:)
    character(CS)          :: units,calendar
    character(CS)          :: bunits        ! time units (days,secs,...)
    integer                :: bdate         ! base date: calendar date
    real(R8)               :: bsec          ! base date: elapsed secs
    integer                :: ndate         ! calendar date of time value
    integer                :: old_handle    ! previous setting of pio error handling
    real(R8)               :: nsec          ! elapsed secs on calendar date
    real(R8),allocatable   :: tvar(:)
    character(*),parameter :: subname = '(shr_stream_readTCoord) '
    !-------------------------------------------------------------------------------

    lrc = 0

    ! set filename to read
    fileName  = trim(strm%file(k)%name)

    ! open file if needed
    if (.not. pio_file_is_open(strm%file(k)%fileid)) then
       if (debug>1 .and. isroot_task) then
          write(strm%logunit, '(a)') trim(subname)//' opening stream filename = '//trim(filename)
       end if
       rcode = pio_openfile(strm%pio_subsystem, strm%file(k)%fileid, strm%pio_iotype, filename, pio_nowrite)
    endif

    rCode = pio_inq_varid(strm%file(k)%fileid, 'time', vid)
    rCode = pio_inquire_variable(strm%file(k)%fileid, vid, ndims=ndims)
    allocate(dids(ndims))
    rCode = pio_inquire_variable(strm%file(k)%fileid, vid, dimids=dids)

    ! determine number of times in file
    rCode = pio_inquire_dimension(strm%file(k)%fileid, dids(1), len=nt)
    deallocate(dids)

    ! allocate memory for date and secs
    if (.not. allocated(strm%file(k)%date)) then
       allocate(strm%file(k)%date(nt), strm%file(k)%secs(nt))
    else if(size(strm%file(k)%date) .ne. nt) then
       deallocate(strm%file(k)%date, strm%file(k)%secs)
       allocate(strm%file(k)%date(nt), strm%file(k)%secs(nt))
    endif

    strm%file(k)%nt = nt

    ! get time units
    units = ' '
    rCode = pio_get_att(strm%file(k)%fileid, vid, 'units', units)
    n = len_trim(units)
    if (ichar(units(n:n)) == 0 ) units(n:n) = ' '
    call shr_string_leftalign_and_convert_tabs(units)

    ! get strm%calendar
    calendar = ' '
    call pio_seterrorhandling(strm%file(k)%fileid, PIO_BCAST_ERROR, old_handle)
    rCode = pio_inq_att(strm%file(k)%fileid, vid, 'calendar')
    call pio_seterrorhandling(strm%file(k)%fileid, old_handle)
    if (rCode == pio_noerr) then
       rCode = pio_get_att(strm%file(k)%fileid, vid, 'calendar', calendar)
    else
       calendar = trim(shr_cal_noleap)
    endif
    n = len_trim(calendar)
    if (ichar(calendar(n:n)) == 0 ) calendar(n:n) = ' '
    call shr_string_leftalign_and_convert_tabs(calendar)
    call shr_string_parseCFtunit(units, bunits, bdate, bsec)
    strm%calendar = trim(shr_cal_calendarName(trim(calendar)))

    ! read in time coordinate values
    allocate(tvar(nt))
    rcode = pio_get_var(strm%file(k)%fileid,vid,tvar)

    ! determine strm%file(k)%date(n) and strm%file(k)%secs(n)
    do n = 1,nt
       call shr_cal_advDate(tvar(n), bunits, bdate, bsec, ndate, nsec, calendar)
       strm%file(k)%date(n) = ndate
       strm%file(k)%secs(n) = nint(nsec)
    enddo
    deallocate(tvar)

    ! close file
    if (debug>1 .and. isroot_task) then
       write(strm%logunit, '(a)') trim(subname)//' closing stream filename = '//trim(filename)
    end if
    call pio_closefile(strm%file(k)%fileid)

    ! if offset is not zero, adjust strm%file(k)%date(n) and strm%file(k)%secs(n)
    if (strm%offset /= 0) then
       if (size(strm%file(k)%date) /= size(strm%file(k)%secs)) then
          write(strm%logunit,'(a,2i7)') trim(subname)//" Incompatable date and secs sizes",&
               size(strm%file(k)%date), size(strm%file(k)%secs)
          call shr_sys_abort()
       endif
       num = size(strm%file(k)%date)
       offin = strm%offset
       do n = 1,num
          din = strm%file(k)%date(n)
          sin = strm%file(k)%secs(n)
          call shr_cal_advDateInt(offin,'seconds',din,sin,dout,sout,calendar)
          strm%file(k)%date(n) = dout
          strm%file(k)%secs(n) = sout
          ! write(strm%logunit,*) 'debug ',n,strm%offset,din,sin,dout,sout
       enddo
    endif

    ! Verify that time coordinate is valid
    strm%file(k)%haveData = .true.
    call verifyTCoord(strm, k, lrc) ! check new t-coord data

    if (present(rc)) then
       rc = lrc
    endif

  contains

    subroutine verifyTCoord(strm,k,rc)
      ! verify time coordinate data is OK

      ! !input/output parameters:
      type(shr_stream_streamType),intent(in) :: strm  ! data stream
      integer                   :: k     ! index of file to check
      integer                   :: rc    ! return code

      !----- local -----
      integer :: n           ! generic loop index
      integer :: nt          ! size of t-dimension
      integer :: date1,secs1 ! date and seconds for a    time coord
      integer :: date2,secs2 ! date and seconds for next time coord
      logical :: checkIt     ! have data / do comparison
      character(*),parameter :: subName = '(shr_stream_verifyTCoord) '
      character(*),parameter :: F00   = "('(shr_stream_verifyTCoord) ',8a)"
      character(*),parameter :: F01   = "('(shr_stream_verifyTCoord) ',a,2i7)"
      character(*),parameter :: F02   = "('(shr_stream_verifyTCoord) ',a,2i9.8)"
      !-------------------------------------------------------------------------------
      ! Notes:
      !   o checks that dates are increasing (must not decrease)
      !   o does not check for valid dates (eg. day=0 & month = 13 are "OK")
      !   o checks that secs are strictly increasing within any one day
      !   o checks that 0 <= secs <= spd (seconds per day)
      !   o checks all dates from one file plus last date of previous file and
      !     first date of next file
      !-------------------------------------------------------------------------------

      rc = 0
      if (debug>1 .and. isroot_task) then
         write(strm%logunit,F01) "checking t-coordinate data   for file k =",k
      end if

      if ( .not. strm%file(k)%haveData) then
         rc = 1
         write(strm%logunit,F01) "Don't have data for file ",k
         call shr_sys_abort(subName//"ERROR: can't check -- file not read.")
      end if

      do n=1,strm%file(k)%nt+1
         checkIt = .false.

         !--- do we have data for two consecutive dates? ---
         if (n==1) then
            !--- compare with previous file? ---
            if (k>1) then
               if ( strm%file(k-1)%haveData ) then
                  nt    = strm%file(k-1)%nt
                  date1 = strm%file(k-1)%date(nt)
                  secs1 = strm%file(k-1)%secs(nt)
                  date2 = strm%file(k  )%date(n)
                  secs2 = strm%file(k  )%secs(n)
                  checkIt = .true.
                  if (debug>1 .and. isroot_task) write(strm%logunit,F01) "comparing with previous file for file k =",k
               end if
            end if
         else if (n==strm%file(k)%nt+1) then
            !--- compare with next file? ---
            if (k<strm%nFiles) then
               if ( strm%file(k+1)%haveData ) then
                  nt    = strm%file(k  )%nt
                  date1 = strm%file(k  )%date(nt)
                  secs1 = strm%file(k  )%secs(nt)
                  date2 = strm%file(k+1)%date(1)
                  secs2 = strm%file(k+1)%secs(1)
                  checkIt = .true.
                  if (debug>1 .and. isroot_task) write(strm%logunit,F01) "comparing with next     file for file k =",k
               end if
            end if
         else
            !--- compare within this file ---
            date1 = strm%file(k)%date(n-1)
            secs1 = strm%file(k)%secs(n-1)
            date2 = strm%file(k)%date(n  )
            secs2 = strm%file(k)%secs(n  )
            checkIt = .true.
         end if

         !--- compare two consecutive dates ---
         if (checkIt) then
            if ( date1 > date2 ) then
               rc = 1
               write(strm%logunit,F01) "ERROR: calendar dates must be increasing"
               write(strm%logunit,F02) "date(n), date(n+1) = ",date1,date2
               call shr_sys_abort(subName//"ERROR: calendar dates must be increasing")
            else if ( date1 == date2 ) then
               if ( secs1 >= secs2 ) then
                  rc = 1
                  write(strm%logunit,F01) "ERROR: elapsed seconds on a date must be strickly increasing"
                  write(strm%logunit,F02) "secs(n), secs(n+1) = ",secs1,secs2
                  call shr_sys_abort(subName//"ERROR: elapsed seconds must be increasing")
               end if
            end if
            if ( secs1 < 0 .or. spd < secs1 ) then
               rc = 1
               write(strm%logunit,F01) "ERROR: elapsed seconds out of valid range [0,spd]"
               write(strm%logunit,F02) "secs(n) = ",secs1
               call shr_sys_abort(subName//"ERROR: elapsed seconds out of range")
            end if
         end if
      end do

      if (debug>0 .and. isroot_task) write(strm%logunit,F01) "data is OK (non-decreasing)  for file k =",k

    end subroutine verifyTCoord

  end subroutine shr_stream_readTCoord

  !===============================================================================
  subroutine shr_stream_getMeshFileName(stream, filename)

    ! Return stream mesh filename

    ! input/output parameters:
    type(shr_stream_streamType) ,intent(in)  :: stream  ! stream in question
    character(len=*)            ,intent(out) :: filename
    !-------------------------------------------------------------------------------

    filename = stream%meshfile

  end subroutine shr_stream_getMeshFileName

  !===============================================================================
  subroutine shr_stream_getModelFieldList(stream, list)

    ! Get list of file fields

    !input/output parameters:
    type(shr_stream_streamType) ,intent(in)  :: stream  ! stream in question
    character(*)                ,intent(out) :: list(:)    ! field list

    ! local variables
    integer :: i
    !-------------------------------------------------------------------------------

    do i=1,stream%nvars
       list(i) = stream%varlist(i)%nameinmodel
    enddo

  end subroutine shr_stream_getModelFieldList

  !===============================================================================
  subroutine shr_stream_getStreamFieldList(stream, list)

    ! Get list of file fields

    !input/output parameters:
    type(shr_stream_streamType) ,intent(in)  :: stream  ! stream in question
    character(*)                ,intent(out) :: list(:)    ! field list
    !-------------------------------------------------------------------------------
    integer :: i

    do i=1,stream%nvars
       list(i) = stream%varlist(i)%nameinfile
    enddo

  end subroutine shr_stream_getStreamFieldList

  !===============================================================================
  subroutine shr_stream_getCalendar(strm, k, calendar)
    use pio, only : PIO_set_log_level, PIO_OFFSET_KIND
    use ESMF, only: ESMF_VM, ESMF_VMGet, ESMF_VMGetCurrent
    ! Returns calendar name

    ! input/output parameters:
    type(shr_stream_streamType) ,intent(inout) :: strm     ! data stream
    integer                     ,intent(in)    :: k        ! file to query
    character(*)                ,intent(out)   :: calendar ! calendar name

    ! local
    type(ESMF_VM)          :: vm
    integer                :: myid
    integer                :: vid, n
    character(CL)          :: fileName
    character(CL)          :: lcal
    integer(PIO_OFFSET_KIND) :: attlen
    integer                :: old_handle
    integer                :: rCode
    integer :: rc
    character(*),parameter :: subName = '(shr_stream_getCalendar) '
    !-------------------------------------------------------------------------------

    lcal = ' '
    calendar = ' '
    if (k > strm%nfiles) call shr_sys_abort(subname//' ERROR: k gt nfiles')

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMGet(vm, localPet=myid, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    fileName  = strm%file(k)%name

    if (.not. pio_file_is_open(strm%file(k)%fileid)) then
       if(myid == 0) write(strm%logunit, '(a)') trim(subname)//' opening stream filename = '//trim(filename)
       rcode = pio_openfile(strm%pio_subsystem, strm%file(k)%fileid, strm%pio_iotype, trim(filename))
    else if(myid == 0) then
       write(strm%logunit, '(a)') trim(subname)//' reading stream filename = '//trim(filename)
    endif

    rCode = pio_inq_varid(strm%file(k)%fileid, 'time', vid)
    if(vid .lt. 0) then
       call shr_sys_abort(subName//"ERROR: time variable id incorrect")
    endif
    call pio_seterrorhandling(strm%file(k)%fileid, PIO_BCAST_ERROR, old_handle)
    rCode = pio_inq_att(strm%file(k)%fileid, vid, 'calendar', len=attlen)
    call pio_seterrorhandling(strm%file(k)%fileid, old_handle)
    if(rcode == PIO_NOERR) then
       rCode = pio_get_att(strm%file(k)%fileid, vid, 'calendar', lcal)
    else
       lcal = trim(shr_cal_noleap)
    endif

    n = len_trim(lcal)
    if(n>0) then
       if (ichar(lcal(n:n)) == 0 ) lcal(n:n) = ' '
    else
       write(strm%logunit,*) 'calendar attribute to time variable not found in file, using default noleap'
       call shr_sys_abort(subName//"ERROR: calendar attribute not found in file "//trim(filename))
       lcal = trim(shr_cal_noleap)
    endif
    call shr_string_leftalign_and_convert_tabs(lcal)
    calendar = trim(shr_cal_calendarName(trim(lcal)))


    if(myid == 0) write(strm%logunit, '(a)') trim(subname)//' closing stream filename = '//trim(filename)
    call pio_closefile(strm%file(k)%fileid)

  end subroutine shr_stream_getCalendar

  !===============================================================================
  subroutine shr_stream_getCurrFile(strm, fileopen, currfile, currpioid)

    ! returns current file information

    ! input/output parameters:
    type(shr_stream_streamType),intent(inout)  :: strm      ! data stream
    logical           ,optional,intent(out) :: fileopen  ! file open flag
    character(*)      ,optional,intent(out) :: currfile  ! current filename
    type(file_desc_t) ,optional,intent(out) :: currpioid ! current pioid
    !-------------------------------------------------------------------------------

    if (present(fileopen  )) fileopen = strm%fileopen
    if (present(currfile  )) currfile = strm%currfile
    if (present(currpioid )) currpioid = strm%currpioid

  end subroutine shr_stream_getCurrFile

  !===============================================================================
  subroutine shr_stream_setCurrFile(strm, fileopen, currfile, currpioid)

    ! set current file information

    ! input/output parameters:
    type(shr_stream_streamType),intent(inout) :: strm      ! data stream
    logical           ,optional,intent(in)    :: fileopen  ! file open flag
    character(*)      ,optional,intent(in)    :: currfile  ! current filename
    type(file_desc_t) ,optional,intent(in)    :: currpioid ! current pioid
    !-------------------------------------------------------------------------------

    if (present(fileopen  )) strm%fileopen = fileopen
    if (present(currfile  )) strm%currfile = currfile
    if (present(currpioid )) strm%currpioid = currpioid

  end subroutine shr_stream_setCurrFile

  !===============================================================================
  subroutine shr_stream_getNextFileName(strm, fn, fnNext,rc)

    ! Returns next file name in sequence
    ! Note: will wrap-around data loop if lvd & gvd are known
    ! otherwise may return file name = "unknown"

    ! !input/output parameters:
    type(shr_stream_streamType) ,intent(in)  :: strm   ! data stream
    character(*)                ,intent(in)  :: fn     ! file name
    character(*)                ,intent(out) :: fnNext ! next file name
    integer      ,optional      ,intent(out) :: rc     ! return code

    ! local variables
    integer                :: rCode   ! return code
    integer                :: n      ! loop index
    logical                :: found  ! file name found?
    character(*),parameter :: subName = '(shr_stream_getNextFileName) '
    character(*),parameter :: F00   = "('(shr_stream_getNextFileName) ',8a)"
    !-------------------------------------------------------------------------------

    rCode = 0

    !--- locate input file in the stream's list of files ---
    found = .false.
    do n = 1,strm%nFiles
       if ( trim(fn) == trim(strm%file(n)%name)) then
          found = .true.
          exit
       end if
    end do
    if (.not. found) then
       rCode = 1
       write(strm%logunit,F00) "ERROR: input file name is not in stream: ",trim(fn)
       call shr_sys_abort(subName//"ERROR: file name not in stream: "//trim(fn))
    end if

    !--- get next file name ---
    n = n+1  ! next in list
    if (strm%found_lvd .and. strm%found_gvd) then
       if (n > strm%k_gvd)  n = strm%k_lvd ! wrap-around to lvd
    else if (strm%found_lvd ) then
       if (n > strm%nFiles) n = strm%k_lvd ! wrap-around to lvd
    else if (n > strm%nFiles ) then
       n = 1                               ! wrap-around to 1st file
    end if

    fnNext = trim(strm%file(n)%name)
    if ( present(rc) ) rc = rCode

  end subroutine shr_stream_getNextFileName

  !===============================================================================
  subroutine shr_stream_getPrevFileName(strm, fn, fnPrev,rc)

    ! Returns previous file name in sequence

    ! !input/output parameters:
    type(shr_stream_streamType) ,intent(in)  :: strm   ! data stream
    character(*)                ,intent(in)  :: fn     ! file name
    character(*)                ,intent(out) :: fnPrev ! preciding file name
    integer      ,optional      ,intent(out) :: rc     ! return code

    !--- local ---
    integer                :: rCode ! return code
    integer                :: n     ! loop index
    logical                :: found ! file name found?
    character(*),parameter :: subName = '(shr_stream_getPrevFileName) '
    character(*),parameter :: F00   = "('(shr_stream_getPrevFileName) ',8a)"

    !-------------------------------------------------------------------------------
    ! Note: will wrap-around data loop if lvd & gvd are known
    ! otherwise may return file name = "unknown"
    !-------------------------------------------------------------------------------

    rCode = 0

    !--- locate input file in the stream's list of files ---
    found = .false.
    do n = 1,strm%nFiles
       if ( trim(fn) == trim(strm%file(n)%name)) then
          found = .true.
          exit
       end if
    end do
    if (.not. found) then
       rCode = 1
       write(strm%logunit,F00) "ERROR: input file name is not in stream: ",trim(fn)
       call shr_sys_abort(subName//"ERROR: file name not in stream: "//trim(fn))
    end if

    !--- get previous file name ---
    n = n-1  ! previous in list
    if (strm%found_lvd .and. strm%found_gvd) then
       if ( n < strm%k_lvd) n = strm%k_gvd ! do wrap-around ---
    end if
    if (n>0) then
       fnPrev = trim(strm%file(n)%name)
    else
       fnPrev = "unknown "
    end if
    if ( present(rc) ) rc = rCode

  end subroutine shr_stream_getPrevFileName

  !===============================================================================
  subroutine shr_stream_getNFiles(strm,nfiles)

    ! Returns number of input files in stream

    ! input/output parameters:
    type(shr_stream_streamType),intent(in)  :: strm      ! data stream
    integer                    ,intent(out) :: nfiles    ! number of input files in stream
    !-------------------------------------------------------------------------------

    nfiles = strm%nfiles

  end subroutine shr_stream_getNFiles

  !===============================================================================
  subroutine shr_stream_restIO(pioid, streams, mode)

    use pio, only : pio_def_dim, pio_def_var, pio_put_var, pio_get_var, file_desc_t, var_desc_t
    use pio, only : pio_int, pio_char

    ! input/out variables
    type(file_desc_t)           , intent(inout) :: pioid
    type(shr_stream_streamType) , intent(inout) :: streams(:)
    character(len=*)            , intent(in)    :: mode  ! read, write, define

    ! local variables
    type(var_desc_t)     :: varid, tvarid, dvarid, ntvarid, hdvarid
    integer              :: rcode
    integer              :: dimid_stream, dimid_files,dimid_nt, dimid_str
    integer              :: n, k, maxnfiles=0
    integer              :: maxnt = 0
    integer, allocatable :: tmp(:)
    character(len=CL)    :: fname
    !-------------------------------------------------------------------------------

    if (mode .eq. 'define') then

       rcode = pio_def_dim(pioid, 'strlen',   CL, dimid_str)
       do k=1,size(streams)
          ! maxnfiles is the maximum number of files across all streams
          if (streams(k)%nfiles > maxnfiles) then
             maxnfiles = streams(k)%nfiles
          endif
          ! maxnt is the maximum number of time samples across all possible stream files
          do n=1,streams(k)%nFiles
             if( streams(k)%file(n)%nt > maxnt) then
                maxnt = streams(k)%file(n)%nt
             endif
          enddo
       enddo
       rcode = pio_def_dim(pioid, 'nt'      , maxnt, dimid_nt)
       rcode = pio_def_dim(pioid, 'nfiles'  , maxnfiles, dimid_files)
       rcode = pio_def_dim(pioid, 'nstreams', size(streams), dimid_stream)

       rcode = pio_def_var(pioid, 'ymdLB' ,   PIO_INT , (/dimid_stream/), varid)
       rcode = pio_def_var(pioid, 'ymdUB' ,   PIO_INT , (/dimid_stream/), varid)
       rcode = pio_def_var(pioid, 'todLB' ,   PIO_INT , (/dimid_stream/), varid)
       rcode = pio_def_var(pioid, 'todUB' ,   PIO_INT , (/dimid_stream/), varid)
       rcode = pio_def_var(pioid, 'nfiles',   PIO_INT , (/dimid_stream/), varid)
       rcode = pio_def_var(pioid, 'offset',   PIO_INT , (/dimid_stream/), varid)
       rcode = pio_def_var(pioid, 'k_lvd',    PIO_INT , (/dimid_stream/), varid)
       rcode = pio_def_var(pioid, 'n_lvd',    PIO_INT , (/dimid_stream/), varid)
       rcode = pio_def_var(pioid, 'k_gvd',    PIO_INT , (/dimid_stream/), varid)
       rcode = pio_def_var(pioid, 'n_gvd',    PIO_INT , (/dimid_stream/), varid)
       rcode = pio_def_var(pioid, 'nt'    ,   PIO_INT , (/dimid_files, dimid_stream/), varid)
       rcode = pio_def_var(pioid, 'haveData', PIO_INT , (/dimid_files, dimid_stream/), varid)
       rcode = pio_def_var(pioid, 'filename', PIO_CHAR, (/dimid_str, dimid_files, dimid_stream/), varid)
       rcode = pio_def_var(pioid, 'date',     PIO_INT , (/dimid_nt , dimid_files, dimid_stream/), varid)
       rcode = pio_def_var(pioid, 'timeofday',PIO_INT , (/dimid_nt , dimid_files, dimid_stream/), varid)

    else if (mode .eq. 'write') then

       ! write out nfiles
       rcode = pio_inq_varid(pioid, 'nfiles', varid)
       allocate(tmp(size(streams)))
       do k=1,size(streams)
          tmp(k) = streams(k)%nFiles
       enddo
       rcode = pio_put_var(pioid, varid, tmp)

       ! write out offset
       rcode = pio_inq_varid(pioid, 'offset', varid)
       do k=1,size(streams)
          tmp(k) = streams(k)%offset
       enddo
       rcode = pio_put_var(pioid, varid, tmp)

       ! write out k_lvd
       rcode = pio_inq_varid(pioid, 'k_lvd', varid)
       do k=1,size(streams)
          tmp(k) = streams(k)%k_lvd
       enddo
       rcode = pio_put_var(pioid, varid, tmp)

       ! write out n_lvd
       rcode = pio_inq_varid(pioid, 'n_lvd', varid)
       do k=1,size(streams)
          tmp(k) = streams(k)%n_lvd
       enddo
       rcode = pio_put_var(pioid, varid, tmp)

       ! write out k_gvd
       rcode = pio_inq_varid(pioid, 'k_gvd', varid)
       do k=1,size(streams)
          tmp(k) = streams(k)%k_gvd
       enddo
       rcode = pio_put_var(pioid, varid, tmp)

       ! write out n_gvd
       rcode = pio_inq_varid(pioid, 'n_gvd', varid)
       do k=1,size(streams)
          tmp(k) = streams(k)%n_gvd
       enddo
       rcode = pio_put_var(pioid, varid, tmp)
       deallocate(tmp)

       ! write out nt
       rcode = pio_inq_varid(pioid, 'nt', varid)
       do k=1,size(streams)
          do n=1,streams(k)%nfiles
             rcode = pio_put_var(pioid, varid, (/n,k/), streams(k)%file(n)%nt)
          enddo
       enddo

       ! write out haveData
       rcode = pio_inq_varid(pioid, 'haveData', dvarid)
       do k=1,size(streams)
          do n=1,streams(k)%nfiles
             if(streams(k)%file(n)%haveData) then
                rcode = pio_put_var(pioid, dvarid, (/n,k/), 1)
             else
                rcode = pio_put_var(pioid, dvarid, (/n,k/), 0)
             endif
          enddo
       enddo

       ! write out date
       rcode = pio_inq_varid(pioid, 'date', varid)
       do k=1,size(streams)
          do n=1,streams(k)%nfiles
             if (allocated(streams(k)%file(n)%date)) then
                rcode = pio_put_var(pioid, varid, (/1,n,k/), (/streams(k)%file(n)%nt,1,1/), streams(k)%file(n)%date)
             endif
          enddo
       enddo

       ! write out timeofday
       rcode = pio_inq_varid(pioid, 'timeofday', varid)
       do k=1,size(streams)
          do n=1,streams(k)%nfiles
             if (allocated(streams(k)%file(n)%secs)) then
                rcode = pio_put_var(pioid, varid, (/1,n,k/), (/streams(k)%file(n)%nt,1,1/), streams(k)%file(n)%secs)
             endif
          enddo
       enddo

       ! write out filename
       rcode = pio_inq_varid(pioid, 'filename', varid)
       do k=1,size(streams)
          do n=1,streams(k)%nfiles
             rcode = pio_put_var(pioid, varid, (/1,n,k/), streams(k)%file(n)%name)
          enddo
       enddo

    else if (mode .eq. 'read') then

       ! Read in nfiles
       rcode = pio_inq_varid(pioid, 'nfiles', varid)
       allocate(tmp(size(streams)))
       rcode = pio_get_var(pioid, varid, tmp)
       do k=1,size(streams)
          if (streams(k)%nFiles /= tmp(k)) then
             call shr_sys_abort('ERROR reading in nfiles')
          endif
       enddo

       ! read in offset
       rcode = pio_inq_varid(pioid, 'offset', varid)
       rcode = pio_get_var(pioid, varid, tmp)
       do k=1,size(streams)
          if (streams(k)%offset /= tmp(k)) then
             call shr_sys_abort('ERROR reading in offset')
          endif
       enddo

       rcode = pio_inq_varid(pioid, 'k_lvd', varid)
       rcode = pio_get_var(pioid, varid, tmp)
       do k=1,size(streams)
          streams(k)%k_lvd = tmp(k)
          if (streams(k)%k_lvd /= tmp(k)) then
             call shr_sys_abort('ERROR reading in k_lvd')
          endif
       enddo

       rcode = pio_inq_varid(pioid, 'n_lvd', varid)
       rcode = pio_get_var(pioid, varid, tmp)
       do k=1,size(streams)
          streams(k)%n_lvd = tmp(k)
          if (streams(k)%n_lvd /= tmp(k)) then
             call shr_sys_abort('ERROR reading in n_lvd')
          endif
       enddo

       rcode = pio_inq_varid(pioid, 'k_gvd', varid)
       rcode = pio_get_var(pioid, varid, tmp)
       do k=1,size(streams)
          streams(k)%k_gvd = tmp(k)
          if (streams(k)%k_gvd /= tmp(k)) then
             call shr_sys_abort('ERROR reading in k_gvd')
          endif
       enddo

       rcode = pio_inq_varid(pioid, 'n_gvd', varid)
       rcode = pio_get_var(pioid, varid, tmp)
       do k=1,size(streams)
          streams(k)%n_gvd = tmp(k)
          if (streams(k)%n_gvd /= tmp(k)) then
             call shr_sys_abort('ERROR reading in n_gvd')
          endif
       enddo
       deallocate(tmp)

       rcode = pio_inq_varid(pioid, 'filename' , varid)
       rcode = pio_inq_varid(pioid, 'nt'       , ntvarid)
       rcode = pio_inq_varid(pioid, 'date'     , dvarid)
       rcode = pio_inq_varid(pioid, 'timeofday', tvarid)
       rcode = pio_inq_varid(pioid, 'haveData' , hdvarid)
       do k=1,size(streams)
          do n=1,streams(k)%nfiles

             ! read in filename
             rcode = pio_get_var(pioid, varid, (/1,n,k/), fname)
             if (trim(fname) /= trim(streams(k)%file(n)%name)) then
                write(6,'(a)')' fname = '//trim(fname)
                write(6,'(a,i8,2x,i8,2x,a)')' k,n,streams(k)%file(n)%name = ',k,n,trim(streams(k)%file(n)%name)
                call shr_sys_abort('ERROR reading in filename')
             endif

             ! read in nt
             allocate(tmp(1))
             rcode = pio_get_var(pioid, ntvarid, (/n,k/), tmp(1))
             streams(k)%file(n)%nt = tmp(1)
             if(tmp(1) /= streams(k)%file(n)%nt) then
                call shr_sys_abort('ERROR read in nt')
             endif
             deallocate(tmp)

             if (streams(k)%file(n)%nt > 0) then

                ! Allocate memory
                allocate(tmp(streams(k)%file(n)%nt))

                ! Read in date
                rcode = pio_get_var(pioid, dvarid, (/1,n,k/), (/streams(k)%file(n)%nt,1,1/),tmp)
                streams(k)%file(n)%date = tmp
                if (.not. allocated(streams(k)%file(n)%date) .or. any(tmp .ne. streams(k)%file(n)%date) ) then
                   call shr_sys_abort('ERROR reading in date')
                endif

                ! Read in timeofday
                rcode = pio_get_var(pioid, tvarid, (/1,n,k/), (/streams(k)%file(n)%nt,1,1/),tmp)
                streams(k)%file(n)%secs = tmp
                if (.not. allocated(streams(k)%file(n)%secs) .or. any(tmp .ne. streams(k)%file(n)%secs) ) then
                   call shr_sys_abort('ERROR reaing in timeofday')
                endif

                ! Read in havedata
                rcode = pio_get_var(pioid, hdvarid, (/n,k/), tmp(1))
                if(tmp(1)==1) then
                   streams(k)%file(n)%havedata = .true.
                else
                   streams(k)%file(n)%havedata = .false.
                endif

                ! Free memory
                deallocate(tmp)

             endif
          enddo
       enddo
    endif

  end subroutine shr_stream_restIO

  !===============================================================================
  subroutine shr_stream_dataDump(strm)

    ! Dump all data to stdout for debugging

    ! input/output parameters:
    type(shr_stream_streamType),intent(in) :: strm      ! data stream

    !----- local -----
    integer   :: k ! generic loop index
    integer   :: logunit
    character(*),parameter :: F00   = "('(shr_stream_dataDump) ',8a)"
    character(*),parameter :: F01   = "('(shr_stream_dataDump) ',a,3i5)"
    character(*),parameter :: F02   = "('(shr_stream_dataDump) ',a,365i9.8)"
    character(*),parameter :: F03   = "('(shr_stream_dataDump) ',a,365i6)"
    !-------------------------------------------------------------------------------

    logunit = strm%logunit

    if (debug > 0) then
       write(logunit,F00) "dump internal data for debugging..."
       write(logunit,F01) "nFiles        = ", strm%nFiles
       do k=1,strm%nFiles
          write(logunit,F01) "data for file k = ",k
          write(logunit,F00)    "* file(k)%name    = ", trim(strm%file(k)%name)
          if ( strm%file(k)%haveData ) then
             write(logunit,F01) "* file(k)%nt      = ", strm%file(k)%nt
             write(logunit,F02) "* file(k)%date(:) = ", strm%file(k)%date(:)
             write(logunit,F03) "* file(k)%Secs(:) = ", strm%file(k)%secs(:)
          else
             write(logunit,F00) "* time coord data not read in yet for this file"
          end if
       end do
       write(logunit,F01) "yearF/L/A    = ", strm%yearFirst,strm%yearLast,strm%yearAlign
       write(logunit,F01) "offset       = ", strm%offset
       write(logunit,F00) "taxMode      = ", trim(strm%taxMode)
       write(logunit,F00) "meshfile     = ", trim(strm%meshfile)
    end if

  end subroutine shr_stream_dataDump

  !===============================================================================
  subroutine shr_stream_getData(stream, index, filename)

    ! Returns full pathname of stream data file (nt)

    type(shr_stream_streamType) , intent(in)  :: stream
    integer                     , intent(in)  :: index
    character(len=*)            , intent(out) :: filename
    !-------------------------------------------------------------------------------

    filename = trim(stream%file(index)%name)

  end subroutine shr_stream_getData

end module dshr_stream_mod
