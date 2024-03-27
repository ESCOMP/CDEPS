.. _dglc:

Data Ocean (DGLC)
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

DGLC has the capability of supporting multiple ice sheets (as is the
case with CISM/CMEPS coupling). This is configured via the following
namelist settings:

  - ``model_meshfiles_list`` is a colon separated string containing  model
    meshfiles describing the different ice sheets.

  - ``model_datafiles_list`` is colon separated string containing
    input datafiles that are used by CISM. Each datafile corresponds to a different
    ice sheet mesh and should have the same order as those in the ``model_meshfiles_list``.

.. _dglc-datamodes:

--------------------
Supported Data Modes
--------------------

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

DGLC%DOM
   - SST data provided by a file
   - dglc_mode: prescribed
   - streams: prescribed
   - datamode: sstdata

DGLC%IAF
   - SST data provided by a file
   - dglc_mode: interannual
   - streams: interannual
   - datamode: iaf

DGLC%SOM
   - Slab Ocean Model is used to calculate sea surface temperature.
   - dglc_mode: som
   - streams: som
   - datamode: som

DGLC%SOMAQP
   - Slab Ocean Model is used to calculate sea surface temperature.
   - dglc_mode: som_aquap
   - streams: N/A
   - datamode: som_aquap

DGLC%AQP[1-10]
   - Lattitude varying analytical sea surface data
   - dglc_mode: sst_aquap[1-10]
   - streams: N/A
   - datamode: sst_aquap[1-10]

DGLC%AQPFILE
   - SST data provided by a file
   - dglc_mode: sst_aquap_file
   - streams: aquapfile
   - datamode: sst_aquapfile

DGLC%AQPCONST
   - Constant sea surface data from aquaplanet
   - dglc_mode: sst_aquap_constant
   - streams: N/A
   - datamode: sst_aquap_constant
