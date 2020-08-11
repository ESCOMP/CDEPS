.. _design-details:

================
 Design Details
================

----------------------
Data Model Performance
----------------------

There are two primary costs associated with CDEPS share code: reading data and spatially mapping data.
Time interpolation is relatively cheap in the current implementation.
As much as possible, redundant operations are minimized.
The upper and lower bound mapped input data is saved between time steps to reduce mapping costs in cases where data is time interpolated more often than new data is read.
If the input data timestep is relatively small (for example, hourly data as opposed to daily or monthly data) the cost of reading input data can be quite large.
Also, there can be significant variation in cost of the data model over the coarse of the run, for instance, when new inputdata must be read and interpolated, although it's relatively predictable.
The present implementation doesn't support changing the order of operations, for instance, time interpolating the data before spatial mapping.
Because the present computations are always linear, changing the order of operations will not fundamentally change the results.
The present order of operations generally minimizes the mapping cost for typical data model use cases.

----------------------
IO Through Data Models
----------------------

At the present time, data models can only read netcdf data, and IO is handled through the PIO library using either netcdf or pnetcdf.
PIO can read the data either serially or in parallel in chunks that are approximately the global field size divided by the number of IO tasks.
If pnetcdf is used through PIO, then the pnetcdf library must be included during the build of the model.

----------------------------------
IO Through Data Models In CIME-CCS
----------------------------------

If CDEPS is used in CIME, the pnetcdf path and option is hardwired
into the ``Macros.make`` file for the specific machine.  To turn on
``pnetcdf`` in the build, make sure the ``Macros.make`` variables
``PNETCDF_PATH``, ``INC_PNETCDF``, and ``LIB_PNETCDF`` are set and
that the PIO ``CONFIG_ARGS`` sets the ``PNETCDF_PATH`` argument.
Beyond just the option of selecting IO with PIO, several namelist variables are available to help optimize PIO IO performance.
Those are **TODO** - list these.
The total mpi tasks that can be used for IO is limited to the total number of tasks used by the data model.
Often though, using fewer IO tasks results in improved performance.
In general, [io_root + (num_iotasks-1)*io_stride + 1] has to be less than the total number of data model tasks.
In practice, PIO seems to perform optimally somewhere between the extremes of 1 task and all tasks, and is highly machine and problem dependent.

-------------
Restart Files
-------------
Restart files are generated automatically by the data models based on an attribute flag received in the NUOPC cap.
The restart files must meet the CIME-CCS naming convention and an ``rpointer`` file is generated at the same time.
An ``rpointer`` file is a *restart pointer* file which contains the name of the most recently created restart file.
Normally, if restart files are read, the restart filenames are specified in the ``rpointer`` file.
Optionally though, there are namelist variables such as ``restfilm`` to specify the restart filenames via namelist. If those namelist variables are set, the ``rpointer`` file will be ignored.

In most cases, no restart file is required for the data models to restart exactly.
This is because there is no memory between timesteps in many of the data model science modes.
If a restart file is required, it will be written automatically and then must be used to continue the previous run.

There are separate stream restart files that only exist for
performance reasons.  A stream restart file contains information about
the time axis of the input streams.  This information helps reduce the
startup costs associated with reading the input dataset time axis
information.  If a stream restart file is missing, the code will
restart without it but may need to reread data from the input data
files that would have been stored in the stream restart file.  This
will take extra time but will not impact the results.

.. _data-structures:

---------------
Stream Modules
---------------

The CDEPS stream code contains four modules:

**dshr_strdata_mod.F90**
  Carries out stream IO along with the spatial and
  temporal interpolation of the stream data to the model mesh and
  model time. Initializes the module data type ``shr_strdata_type``.

**dshr_stream_mod.F90**
  Reads in the stream xml file and returns the upper and
  lower bounds of the stream data. Initializes the module data type
  ``shr_stream_streamType``.

**dshr_tinterp_mod.F90**
  Determines the time interpolation factors.

**dshr_methods_mod.F90**
  Wrappers to ESMF such as getting a pointer to a field in a field bundle, etc.

----------------
Stream Datatypes
----------------

The most basic type, ``shr_stream_fileType`` is contained in
``shr_stream_mod.F90`` and specifies basic information related to a
given stream file.

.. code-block:: Fortran

   type shr_stream_fileType
      character(SHR_KIND_CL) :: name = shr_stream_file_null	! the file name
      logical                :: haveData = .false.		! has t-coord data been read in?
      integer  (SHR_KIND_IN) :: nt = 0				! size of time dimension
      integer  (SHR_KIND_IN),allocatable :: date(:)		! t-coord date: yyyymmdd
      integer  (SHR_KIND_IN),allocatable :: secs(:)		! t-coord secs: elapsed on date
   end type shr_stream_fileType

The following type, ``shr_stream_streamType`` contains information
that encapsulates the information related to all files specific to a
target stream. (see the overview of the :ref:`stream_description_file`).

.. code-block:: Fortran

  type shr_stream_streamType
     !private ! no public access to internal components
     integer           :: logunit                               ! stdout log unit
     type(iosystem_desc_t), pointer :: pio_subsystem
     integer           :: pio_iotype
     integer           :: pio_ioformat
     logical           :: init         = .false.                ! has stream been initialized
     integer           :: nFiles       = 0                      ! number of data files
     integer           :: yearFirst    = -1                     ! first year to use in t-axis (yyyymmdd)
     integer           :: yearLast     = -1                     ! last  year to use in t-axis (yyyymmdd)
     integer           :: yearAlign    = -1                     ! align yearFirst with this model year
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
     character(CL)     :: stream_vectors                        ! stream vectors names
     type(file_desc_t) :: currpioid                             ! current pio file desc
     type(shr_stream_file_type)    , allocatable :: file(:)     ! filenames of stream data files (full pathname)
     type(shr_stream_data_variable), allocatable :: varlist(:)  ! stream variable names (on file and in model)
  end type shr_stream_streamType

Finally, the datatypes ``shr_strdata_per_stream`` and
``shr_strdata_type`` in ``dshr_strdata_mod.F90`` are at the heart
of the CDEPS stream code and contains information for
all the streams that are active for the target data model.

.. code-block:: Fortran

  type shr_strdata_perstream
     character(CL)                       :: stream_meshfile                 ! stream mesh file from stream txt file
     type(ESMF_Mesh)                     :: stream_mesh                     ! stream mesh created from stream mesh file
     type(io_desc_t)                     :: stream_pio_iodesc               ! stream pio descriptor
     logical                             :: stream_pio_iodesc_set =.false.  ! true=>pio iodesc has been set
     type(ESMF_RouteHandle)              :: routehandle                     ! stream n -> model mesh mapping
     character(CL), allocatable          :: fldlist_stream(:)               ! names of stream file fields
     character(CL), allocatable          :: fldlist_model(:)                ! names of stream model fields
     integer                             :: stream_lb                       ! index of the Lowerbound (LB) in fldlist_stream
     integer                             :: stream_ub                       ! index of the Upperbound (UB) in fldlist_stream
     type(ESMF_Field)                    :: field_stream                    ! a field on the stream data domain
     type(ESMF_Field)                    :: stream_vector                   ! a vector field on the stream data domain
     type(ESMF_FieldBundle), allocatable :: fldbun_data(:)                  ! stream field bundle interpolated to model grid
     type(ESMF_FieldBundle)              :: fldbun_model                    ! stream n field bundle interpolated to model grid and time
     integer                             :: ucomp = -1                      ! index of vector u in stream
     integer                             :: vcomp = -1                      ! index of vector v in stream
     integer                             :: ymdLB = -1                      ! stream ymd lower bound
     integer                             :: todLB = -1                      ! stream tod lower bound
     integer                             :: ymdUB = -1                      ! stream ymd upper bound
     integer                             :: todUB = -1                      ! stream tod upper bound
     real(r8)                            :: dtmin = 1.0e30_r8
     real(r8)                            :: dtmax = 0.0_r8
     type(ESMF_Field)                    :: field_coszen                    ! needed for coszen time interp
  end type shr_strdata_perstream

.. code-block:: Fortran

  type shr_strdata_type
     type(shr_strdata_perstream), allocatable :: pstrm(:)              ! stream info
     type(shr_stream_streamType), pointer :: stream(:)=> null()        ! stream datatype
     integer                        :: nvectors                        ! number of vectors
     logical                        :: masterproc
     integer                        :: logunit                         ! stdout unit
     integer                        :: io_type                         ! pio info
     integer                        :: io_format                       ! pio info
     integer                        :: modeldt = 0                     ! model dt in seconds
     type(ESMF_Mesh)                :: model_mesh                      ! model mesh
     real(r8), pointer              :: model_lon(:) => null()          ! model longitudes
     real(r8), pointer              :: model_lat(:) => null()          ! model latitudes
     integer                        :: model_nxg                       ! model global domain lon size
     integer                        :: model_nyg                       ! model global domain lat size
     integer                        :: model_nzg                       ! model global domain vertical size
     integer                        :: model_lsize                     ! model local domain size
     integer, pointer               :: model_gindex(:)                 ! model global index spzce
     integer                        :: model_gsize                     ! model global domain size
     type(ESMF_CLock)               :: model_clock                     ! model clock
     character(CL)                  :: model_calendar = shr_cal_noleap ! model calendar for ymd,tod
     integer                        :: ymd, tod                        ! model time
     type(iosystem_desc_t), pointer :: pio_subsystem => null()         ! pio info
     real(r8)                       :: eccen  = SHR_ORB_UNDEF_REAL     ! cosz t-interp info
     real(r8)                       :: mvelpp = SHR_ORB_UNDEF_REAL     ! cosz t-interp info
     real(r8)                       :: lambm0 = SHR_ORB_UNDEF_REAL     ! cosz t-interp info
     real(r8)                       :: obliqr = SHR_ORB_UNDEF_REAL     ! cosz t-interp info
     real(r8), allocatable          :: tavCoszen(:)                    ! cosz t-interp data
  end type shr_strdata_type
