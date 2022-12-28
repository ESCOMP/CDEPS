.. _extending-cdeps:

===============
Extending CDEPS
===============

.. _adding-datamodes:

--------------------
Adding New Data Mode 
--------------------

While, the existing ``copyall`` data modes can be used to bring 
new data streams easily to the CDEPS, the more complex data
streams might need to create a new data mode. For example, bringing 
new data mode to the DATM might involve calculating added value 
fields such as humidity from temperature, pressure and dew point 
temperature or wind speed from wind components.

Adding new data mode to the existing data components 
involve multiple steps:

 #. Creating ESMF mesh file for data stream
 #. Creating new Fortran module file for new data mode
 #. Adding New Data Mode to Data Component 

.. _create-scrip:

Creating SCRIP File 
-------------------

The easiest way to create ESMF Mesh file is to use the 
``ESMF_Scrip2Unstruct`` application. The application is a parallel 
program that converts a `SCRIP format grid file <http://earthsystemmodeling.org/
docs/nightly/develop/ESMF_refdoc/node3.html#sec:fileformat:scrip>`_ 
into an unstructured grid file in the `ESMF unstructured file format 
<http://earthsystemmodeling.org/docs/nightly/develop/ESMF_refdoc/node3.html#sec:
fileformat:esmf>`_ or in the `UGRID file format <http://earthsystemmodeling.org/
docs/nightly/develop/ESMF_refdoc/node3.html#sec:fileformat:ugrid>`_. 

To use ``ESMF_Scrip2Unstruct`` application, SCRIP or UGRID format grid files need
to be created first by using coordinate information found in the stream data
files. The SCRIP format grid file can be created using existing tools such
as `netCDF Operator (NCO) <https://nco.sourceforge.net>`_ and `NCAR Command 
Language (NCL) <https://www.ncl.ucar.edu>`_ while UGRID format requires to
develop a custom tool to generate. 

For more sophisticated mesh definitions such as unstructured grids and 
the polygons with more sides, SCRIP format grid file or ESMF Mesh file 
needs to be directly created by developing custom tool by following the 
exiting convention that is used to define SCRIP and ESMF Mesh file formats.

.. _create-scrip-nco:

``netCDF Operator (NCO)``

The `ncks <https://nco.sourceforge.net/nco.html#ncks-netCDF-Kitchen-Sink>`_ 
command provided by ``NCO`` generates accurate and complete SCRIP-format 
gridfiles for select grid types, including uniform, capped and Gaussian 
rectangular, latitude/longitude grids, global or regional. In this case, the grids 
are stored in an external grid-file. The more information and examples can be
found in `here <https://nco.sourceforge.net/nco.html#Grid-Generation>`_.

As an example, a SCRIP grid definition file for `Weather Research & Forecasting 
Model (WRF) <https://www.mmm.ucar.edu/models/wrf>`_ can be created using 
following command:  

.. code-block:: console

  ncks --rgr infer --rgr scrip=wrfinput_d01_scrip.nc wrfinput_d01_2d_lat_lon.nc wrfinput_d01_foo.nc

where ``wrfinput_d01_scrip.nc`` is the output file (in SCRIP format), 
``wrfinput_d01_2d_lat_lon.nc`` is the input file with the 2d grid information, 
and ``wrfinput_d01_foo.nc`` is an output file containing metadata. In this case,
``wrfinput_d01_2d_lat_lon.nc`` needs to be created from origial WRF input file
by following netCDF `CF conventions <https://cfconventions.org>`_.  

The following example also creates SCRIP grid file using `ncremap 
<https://nco.sourceforge.net/nco.html#ncremap-netCDF-Remapper>`_. 
command without providing any input file:

.. code-block:: console

  ncremap -g glo_30m.SCRIP.nc -G latlon=321,720#snwe=-80.25,80.25,-0.25,359.75#lat_typ=uni#lat_drc=s2n

where ``glo_30m.SCRIP.nc`` is the output file, ``-G`` options indicates that
the command will create the gridfile in SCRIP format, ``latlon`` is used
to indicate the size of latitude and longitude coordinates, ``snwe`` option specifies the
the outer edges of a regional rectangular grid, ``lat_typ`` option is used to
define grid type (``uni`` is used for global and uniform grids) and 
``lat_drc`` option specifies whether latitudes monotonically increase 
or decrease in rectangular grids (``s2n`` for grids that begin with the 
most southerly latitude).

.. _create-scrip-ncl:

``NCAR Command Language (NCL)``

The NCL way of creating SCRIP grid definition file requires additional
development to represent stream data grid information. In this case, NCL
provides set of functions to get grid coordinates as input to create
SCRIP grid definition file. These functions are listed as follows:

.. _latlon_to_SCRIP: https://www.ncl.ucar.edu/Document/Functions/ESMF/latlon_to_SCRIP.shtml
.. |latlon_to_SCRIP| replace:: **latlon_to_SCRIP** 

|latlon_to_SCRIP|_
  This procedure writes the description of the requested lat/lon 
  grid to a netCDF SCRIP output file. It does not get any input arguments
  related to the coordinates but generates them internally based on given
  `grid_type` option ("1deg", "0.25deg", etc).

.. _rectilinear_to_SCRIP: https://www.ncl.ucar.edu/Document/Functions/ESMF/rectilinear_to_SCRIP.shtml
.. |rectilinear_to_SCRIP| replace:: **rectilinear_to_SCRIP**

|rectilinear_to_SCRIP|_
  This procedure writes the description of a rectilinear grid, 
  given the 1D coordinate lat/lon arrays, to a netCDF SCRIP file.

.. _curvilinear_to_SCRIP: https://www.ncl.ucar.edu/Document/Functions/ESMF/curvilinear_to_SCRIP.shtml
.. |curvilinear_to_SCRIP| replace:: **curvilinear_to_SCRIP**

|curvilinear_to_SCRIP|_
  This procedure writes the description of a curvilinear grid 
  to a NetCDF SCRIP file, given the 2D lat/lon arrays. 

A simple code snippets that demonstrates the usage of NCL provided 
routines to create ESMF mesh file can be seen in the following example:

.. code-block:: console

  ;--- open file and read variables ---
  ncGrdFilePath = "domain.nc"
  grd_file = addfile(ncGrdFilePath,"r")
  lon = grd_file->lon
  lat = grd_file->lat

  ;--- set options for SCRIP generation ---
  opt = True
  opt@ForceOverwrite = True
  opt@NetCDFType = "netcdf4"
  opt@Title = "Global Grid"

  ;--- generate SCRIP file ---
  dstGridPath = "SCRIP.nc"
  rectilinear_to_SCRIP(dstGridPath, lat, lon, opt)

To add area field to the SCRIP file:

.. code-block:: console

  ;--- add area to SCRIP file ---
  scripFile = addfile("scrip.nc", "w")

  grid_size = dimsizes(scripFile->grid_center_lat)
  grid_area = new(grid_size,double)
  grid_area!0 = "grid_size"

  do i = 0,grid_size-1
    temp_tlat = (/ scripFile->grid_corner_lat(i,3), \
              scripFile->grid_corner_lat(i,1), \
              scripFile->grid_corner_lat(i,0), \
              scripFile->grid_corner_lat(i,2)    /)
    temp_tlon = (/ scripFile->grid_corner_lon(i,3), \
              scripFile->grid_corner_lon(i,1), \
              scripFile->grid_corner_lon(i,0), \
              scripFile->grid_corner_lon(i,2)    /)
    grid_area(i) = area_poly_sphere(temp_tlat, temp_tlon, 1)
  end do

  scripFile->grid_area = (/ grid_area /)

.. note::
  The NCL project is feature frozen. The next generation
  Python tool is now underway, and more information about Geoscience 
  Community Analysis Toolkit (GeoCAT) project can be found in the 
  following `site <https://geocat.ucar.edu>`_.

.. _create-mesh:

Creating ESMF Mesh File 
-----------------------

Once SCRIP grid definition file is created, the ESMF mesh file can
be created using following command:

.. code-block:: console

  ESMF_Scrip2Unstruct input_SCRIP.nc output_ESMFmesh.nc 0

where ``input_SCRIP.nc`` is the input SCRIP grid file and 
``output_ESMFmesh.nc`` is the ESMF mesh file.

.. note::
  Creating SCRIP grid definition file and ESMF mesh file could
  be very memory intensive in case of creating file for very
  high-resolution global grids like `GHRSST <https://www.ghrsst.org>`_
  dataset (0.01 deg.). 

  In this case, NCL way could fail due to the memory usage since
  the process is not parallel and can not be distributed to multiple
  nodes. The workaround could be generating SCRIP and ESMF mesh file 
  for smaller domains or just for the region of interest. In some 
  cases taking advantage of parallelization in ``ESMF_Scrip2Unstruct``
  might help but the current implementation of ``ESMF_Scrip2Unstruct``
  requires reading whole coordinate information in each MPI task and
  This could prevent scaling of the job in terms of its memory usage.

.. _create-fortran-module:

Creating New Fortran Module 
---------------------------

The existing date mode specific Fortran module files can be used 
as a reference to create a new data mode. As an example, 
existing `clmncep <https://github.com/ESCOMP/CDEPS/blob/master/
datm/datm_datamode_clmncep_mod.F90>`_ data mode under DATM can 
be used for this purpose.

In ``datm_datamode_clmncep_mod.F90``, there are five main routines:

  **datm_datamode_clmncep_advertise()**

  This routine advertises a field in a state. In this case, an empty 
  field is created and added to the state through use of ESMF/NUOPC
  provided ``NUOPC_Advertise()`` call. The ``dshr_fldList_add()`` is a 
  generic routine defined under ``dshr/dshr_fldlist_mod.F90`` and 
  populates the internal data structure.

  **datm_datamode_clmncep_init_pointers()**
  
  This routine initializes pointers for module level stream arrays.
  It provides flexibility to access data pointer in actual stream 
  data file (`shr_strdata_get_stream_pointer() <https://github.com/
  ESCOMP/CDEPS/blob/master/streams/dshr_strdata_mod.F90#L75>`_) as 
  well as ESMF Fields (`dshr_state_getfldptr() <https://github.com/
  ESCOMP/CDEPS/blob/master/streams/dshr_methods_mod.F90>`_). 
  The flexibility of checking the fields in the stream data file 
  allows control the behaviour of the data mode based on different
  conditions. In this routine, it is also possible to access
  data provided by other model components to support interaction
  with other components like prognostic mode defined in this mode.

  **datm_datamode_clmncep_advance()**

  This routine is called every time when the data component needs
  to provide the data to other components. It also includes custom
  calculations like limiting temperature field, calculating
  specific humidity or downward longwave and applying unit 
  conversions.

  **datm_datamode_clmncep_restart_write()**

  This routine is used to write restart information to data model
  specific restart file through the use of `dshr_restart_write() 
  <https://github.com/ESCOMP/CDEPS/blob/master/dshr/dshr_mod.F90>`_
  call.

  **datm_datamode_clmncep_restart_read()**

  This routine is used to read restart information from data model
  specific restart file through the use of `dshr_restart_read() 
  <https://github.com/ESCOMP/CDEPS/blob/master/dshr/dshr_mod.F90>`_
  call.

.. _adding-new-datamode:

Adding New Data Mode to Data Component 
--------------------------------------

The data modes are defined in data component specific Fortran modules 
named as ``CDEPS/d[model_name]/[model_name]_comp_nuopc.F90`` where 
``model_name`` can be ``atm``, ``ice``, ``lnd``, ``ocn``, ``rof`` 
or ``wav``. In the ``clmncep`` example, the data mode is defined in
``CDEPS/datm/atm_comp_nuopc.F90`` and DATM component `calls different
routine <https://github.com/ESCOMP/CDEPS/blob/master/datm/
atm_comp_nuopc.F90#L315>`_ based on selected ``datamode`` argument 
in the ``[model_name]_in`` namelist file.
