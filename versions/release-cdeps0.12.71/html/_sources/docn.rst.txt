.. _docn:

Data Ocean (DOCN)
=================

DOCN is normally used to provide observational forcing data (or
forcing data produced by a previous run using active components) to
drive prognostic components. The various ways of running DOCN is 
referred to as its mode.

.. _docn-datamodes:

--------------------
Supported Data Modes
--------------------

DOCN its own set of supported ``datamode`` values that appears in the
``docn_in`` namelist input. The datamode specifies what additional
operations need to be done by DOCN on *ALL* of the streams in the
``docn.streams.xml`` file. Each datamode value is also associated
with a DOCN source file that carries out these operations and these are
listed in parentheses next to the mode name.

sstdata and sst_aquap_file (``docn_datamode_copyall_mod.F90``)
  - `sstdata` and `sst_aquap_file` modes assume that the only field in the 
    input stream is SST. It also assumes the SST is in Celsius and must be 
    converted to Kelvin. All other fields are set to zero.

.. note::
  Normally the ice fraction data is found in the same data files that
  provide SST data to the data ocean model. They are normally found in
  the same file because the SST and ice fraction data are derived from
  the same observational data sets and are consistent with each other.
  to the data ocean model. They are normally found in the same file
  because the SST and ice fraction data are derived from the same
  observational data sets and are consistent with each other.

iaf (``docn_datamode_iaf_mod.F90``)
  - iaf is the interannually varying version of `sstdata`.
    The ocean salinity is set to a constant reference salinity value.
    All other fields other than SST and ocean salinity are set to zero.

sst_aquap_analytic, sst_aquap_constant and sst_aquap[1-10] (``docn_datamode_aquaplanet_mod.F90``)
  - This mode creates analytic sea surface temperature. In case of using
    `sst_aquap[1-10]` data mode, an additional information (`sst_option`)
    is extracted from the data mode to change the behaviour of the 
    data mode such as the way of calculation of sea surface temperature.

som and som_aquap (``docn_datamode_som_mod.F90``)
  - som ("slab ocean model") mode is a prognostic mode. This mode
    computes a prognostic sea surface temperature and a freeze/melt
    potential (surface Q-flux) used by the sea ice model.  This
    calculation requires an external SOM forcing data file that includes
    ocean mixed layer depths and bottom-of-the-slab Q-fluxes.
    Scientifically appropriate bottom-of-the-slab Q-fluxes are normally
    ocean resolution dependent and are derived from the ocean model output
    of a fully coupled CCSM run.  Note that while this mode runs out of
    the box, the default SOM forcing file is not scientifically
    appropriate and is provided for testing and development purposes only.
    Users must create scientifically appropriate data for their particular
    application. A tool is available to derive valid SOM forcing.
    
    The only difference between `som` and `som_aquap` is that `som_aquap`
    limits sea surface temperature based on calculated value of freezing
    temperature.

.. _docn-cime-vars:

---------------------------------------
Configuring DOCN from CIME
---------------------------------------

If CDEPS is coupled to the CIME-CCS then the CIME ``$CASEMROOT`` xml
variable ``DOCN_MODE`` sets the collection of streams the streams that
are associated with DOCN and also sets the docn namelist variable
``datamode`` in the file ``docn_in``. The following are the supported
DOCN ``datamode`` values, as defined in the file
``namelist_definition_docn.xml``.

The following table describes the valid values of ``DOCN_MODE``
(defined in the ``config_component.xml`` file for DOCN), and how they
relate to the associated input streams and the ``datamode`` namelist
variable. CIME will generate a value of ``DOCN_MODE`` based on the
compset.

prescribed
   - SST data provided by a file
   - streams: prescribed 
   - datamode: sstdata

interannual
   - SST data provided by a file
   - streams: interannual
   - datamode: iaf

som
   - Slab Ocean Model is used to calculate sea surface temperature.
   - streams: som
   - datamode: som

som_aquap
   - Slab Ocean Model is used to calculate sea surface temperature.
   - streams: som_aquap
   - datamode: som_aquap

sst_aquap[1-10]
   - Lattitude varying analytical sea surface data
   - streams: N/A
   - datamode: sst_aquap[1-10]

sst_aquap_file
   - SST data provided by a file
   - streams: aquapfile
   - datamode: sst_aquapfile

sst_aquap_constant
   - Constant sea surface data from aquaplanet
   - streams: N/A
   - datamode: sst_aquap_constant
