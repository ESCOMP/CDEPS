.. _dglc:

Data Land-Ice (DGLC)
=================

DGLC is normally used as a substitute for CESM/CISM running in NOEVOLVE mode.
The various ways of running DGLC is referred to as its mode.

NOEVOLVE mode is used in CESM as follows.
In typical runs, CISM is not evolving; CLM computes the surface mass
balance (SMB) and sends it to CISM, but CISM’s ice sheet geometry
remains fixed over the course of the run. In these runs, CISM serves
two roles in the system:

  - Over the CISM domain (typically Greenland in
    CESM2), CISM dictates glacier areas and topographic elevations,
    overriding the values on CLM’s surface dataset. CISM also dictates the
    elevation of non-glacier land units in its domain, and only in this
    domain are atmospheric fields downscaled to non-glacier land
    units. (So if you run with a stub glacier model - SGLC - then glacier
    areas and elevations will be taken entirely from CLM’s surface
    dataset, and no downscaling will be done over non-glacier land units.)

  - CISM provides the grid onto which SMB is downscaled. (If you run with
    SGLC then SMB will still be computed in CLM, but it won’t be
    downscaled to a high-resolution ice sheet grid.)

--------------------
Supported Data Modes
--------------------

DGLC has the capability of supporting multiple ice sheets (as is the
case with CISM/CMEPS coupling). This is configured via the following
``dglc_in`` namelist settings:

  - ``model_meshfiles_list`` is a colon separated string containing  model
    meshfiles describing the different ice sheets.

  - ``model_datafiles_list`` is colon separated string containing
    input datafiles that specify are used to obtain data for bedrock
    topography and the ice thickness.

  - ``model_internal_gridsize`` is an array that is the size of the number of ice
    sheets and that specifies the internal gridcell size that corresponds
    what internal gridcell areas the prognostic land-ice component
    uses internally (in this case CISM). From this value the internal grid areas in
    radians squared are given by (model_internal_gridsize/radius_earth)**2.
    Both model_internal_gridsize and radius_earth have units of meters.


  - ``nx_global`` is an array that is the size of the number of ice
    sheets and that specifies the global longitude dimension of the
    each ice sheet.

  - ``ny_global`` is an array that is the size of the number of ice
    sheets and that specifies the global latitude dimension of the
    each ice sheet.

.. note::
   Each element of ``model_data_filelist``, ``model_areas``,
   ``nx_global`` and ``ny_global`` corresponds to a different ice
   sheet mesh and should have the **same order** as those in the
   ``model_meshfiles_list``.

DGLC has its own set of supported ``datamode`` values that appears in
the ``dglc_in`` namelist input. The datamode specifies what additional
operations need to be done by DGLC on *ALL* of the streams in the
``dglc.streams.xml`` file. Each datamode value is also associated with
a DGLC source file that carries out these operations and these are
listed in parentheses next to the mode name. Currently, the only
supported ``datamode`` is ``noevolve``.

noevolve (``dglc_datamode_noevolve_mod.F90``)
  - This mode is an analytic mode that has no associated stream files.
    This mode uses ``dglc_in`` namelist variables as follows:

.. _dglc-cime-vars:

---------------------------------------
Configuring DGLC using CIME-CCS
---------------------------------------

If CDEPS is coupled to the CIME-CCS then the CIME ``$CASEROOT`` xml
variable ``DGLC_MODE`` will be generated based on the compset
specification ``DGLC%{DGLC_MODE}``.  ``DGLC_MODE`` will in term be
used in the ``namelist_definition_dglc.xml`` file to determine the
collection of streams that are associated with DGLC and also sets the
dglc namelist variable ``datamode`` in the file ``dglc_in``.

The following list describes the valid values of ``DGLC_MODE``
(defined in the ``config_component.xml`` file for DGLC), and how they
relate to the associated input streams and the ``datamode`` namelist
variable.

DGLC%NOEVOLVE
   - fields sent to mediator are created analytically without stream
     input
   - dglc_mode: NOEVOLVE
   - streams: none
   - datamode: noevolve

In addition, the following DGLC specific CIME-CCS xml variables will appear in ``$CASEROOT/env_run.xml``:

DGLC_USE_GREENLAND
   - Whether to include the Greenland Ice Sheet in this DGLC simulation
     This should generally be set at create_newcase time (via the compset). In principle it
     can be changed later, but great care is needed to change a number of other variables
     to be consistent (GLC_GRID, GLC_DOMAIN_MESH and possibly others).

DGLC_USE_ANTARCTICA
   - Whether to include the Antarctic Ice Sheet in this DGLC simulation
     This should generally be set at create_newcase time (via the compset). In principle it
     can be changed later, but great care is needed to change a number of other variables
     to be consistent (GLC_GRID, GLC_DOMAIN_MESH and possibly others).

DGLC_SKIP_RESTART_READ
   - If set to true, than dglc restarts will not be read on a continuation run.
