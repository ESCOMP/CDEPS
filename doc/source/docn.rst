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

DOCN has its own set of supported ``datamode`` values that appears in the
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

iaf (``docn_datamode_iaf_mod.F90``)
  - iaf is the interannually varying version of `sstdata`.
    The ocean salinity is set to a constant reference salinity value.
    All other fields other than SST and ocean salinity are set to zero.

sst_aquap_analytic, sst_aquap_constant and sst_aquap[1-10] (``docn_datamode_aquaplanet_mod.F90``)
  - This mode creates analytic sea surface temperature. In case of using
    `sst_aquap[1-10]` data mode, an additional information (`sst_option`)
    is extracted from the data mode to change the behaviour of the
    data mode such as the method of calculation of sea surface temperature.

som and som_aquap (``docn_datamode_som_mod.F90``)
  - som ("slab ocean model") mode is a prognostic mode. This mode
    computes a prognostic sea surface temperature and a freeze/melt
    potential (surface Q-flux) used by the sea ice model.  This
    calculation requires an external SOM forcing data file that includes
    ocean mixed layer depths and bottom-of-the-slab Q-fluxes.
    Scientifically appropriate bottom-of-the-slab Q-fluxes are normally
    ocean resolution dependent and are derived from the ocean model output
    of a fully coupled CESM run.  Note that while this mode runs out of
    the box, the default SOM forcing file is not scientifically
    appropriate and is provided for testing and development purposes only.
    Users must create scientifically appropriate data for their particular
    application. A tool is available to derive valid SOM forcing.

    The only difference between `som` and `som_aquap` is that `som_aquap`
    limits sea surface temperature based on calculated value of freezing
    temperature.

cplhist (``docn_datamode_cplhist_mod.F90``)

.. _docn-cime-vars:

---------------------------------------
Configuring DOCN using CIME-CCS
---------------------------------------

If CDEPS is coupled to the CIME-CCS then the CIME ``$CASEROOT`` xml
variable ``DOCN_MODE`` will be generated based on the compset
specification ``DOCN%{DOCN_MODE}``.  ``DOCN_MODE`` will in term be
used in the ``namelist_definition_docn.xml`` file to determine the
collection of streams that are associated with DOCN and also sets the
docn namelist variable ``datamode`` in the file ``docn_in``.

The following list describes the valid values of ``DOCN_MODE``
(defined in the ``config_component.xml`` file for DOCN), and how they
relate to the associated input streams and the ``datamode`` namelist
variable.

DOCN%DOM
   - SST data provided by a file
   - docn_mode: prescribed
   - streams: prescribed
   - datamode: sstdata

DOCN%IAF
   - SST data provided by a file
   - docn_mode: interannual
   - streams: interannual
   - datamode: iaf

DOCN%SOM
   - Slab Ocean Model is used to calculate sea surface temperature.
   - docn_mode: som
   - streams: som
   - datamode: som

DOCN%SOMAQP
   - Slab Ocean Model is used to calculate sea surface temperature.
   - docn_mode: som_aquap
   - streams: N/A
   - datamode: som_aquap

DOCN%AQP[1-10]
   - Lattitude varying analytical sea surface data
   - docn_mode: sst_aquap[1-10]
   - streams: N/A
   - datamode: sst_aquap[1-10]

DOCN%AQPFILE
   - SST data provided by a file
   - docn_mode: sst_aquap_file
   - streams: aquapfile
   - datamode: sst_aquapfile

DOCN%AQPCONST
   - Constant sea surface data from aquaplanet
   - docn_mode: sst_aquap_constant
   - streams: N/A
   - datamode: sst_aquap_constant


In addition, the following list outlines the DOCN specific CIME-CCS xml variables that  appear in ``$CASEROOT/env_run.xml``:

SSTICE_DATA_FILENAME
   - Prescribed SST and ice coverage data file name.
     Sets SST and ice coverage data file name.
     This is only used when DOCN%DOM or DOCN%IAF is present in the compset.
     (used by both DOCN and CICE running in prescribed mode)

SSTICE_MESH_FILENAME
   - Prescribed SST and ice coverage mesh file name.
     Sets SST and ice coverage grid file name for prescribed runs.
     This is only used when DOCN%DOM or DOCN%IAF is present in the compset.
     (used by both DOCN and CICE running in prescribed mode)

SSTICE_YR_START
   - Starting year to loop data over
     (only used by both DOCN runing in prescribed mode and CICE running in prescribed mode)

SSTICE_YR_END
   - Ending year to loop data over
     (only used by both DOCN runing in prescribed mode and CICE running in prescribed mode)

SSTICE_YR_ALIGN
   - The model year that corresponds to SSTICE_YEAR_START on the data file.
     Prescribed SST and ice coverage data will be aligned so that the first year of
     data corresponds to SSTICE_YEAR_ALIGN in the model. For instance, if the first
     year of prescribed data is the same as the first year of the model run, this
     should be set to the year given in RUN_STARTDATE.
     If SSTICE_YEAR_ALIGN is later than the model's starting year, or if the model is
     run after the prescribed data ends (as determined by SSTICE_YEAR_END), the
     default behavior is to assume that the data from SSTICE_YEAR_START to
     SSTICE_YEAR_END cyclically repeats. This behavior is controlled by the
     &quot;taxmode&quot; stream option; see the data model documentation for more details.
     (only used by both DOCN runing in prescribed mode and CICE running in prescribed mode)

DOCN_AQPCONST_VALUE
   - Sets globally constant SST value and is only used when DOCN%AQPCONST is present in the compset.

DOCN_SOMAQP_DATAFILE
   - Sets the SOM aquaplanet file and is only used when DOCN%AQPFILE is present in the compset.

DOCN_CPLHIST_YR_START
   - Starting year to loop data over
     Only used if DOCN_MODE=cplhist.

DOCN_CPLHIST_YR_END
   - Ending year to loop data over
     Only used if DOCN_MODE=cplhist.

DOCN_CPLHIST_YR_ALIGN
   - Simulation year corresponding to DOCN_CPLHIST_YR_START. A common usage
     is to set this to RUN_STARTDATE. With this setting, the forcing
     in the first year of the run will be the forcing of year
     DOCN_CPLHIST_YR_START. Another use case is to align the calendar
     of transient forcing with the model calendar. For example,
     setting DOCN_CPLHIST_YR_ALIGN=DOCN_CPLHIST_YR_START will lead to
     the forcing calendar being the same as the model calendar. The
     forcing for a given model year would be the forcing of the same
     year. This would be appropriate in transient runs where the
     model calendar is setup to span the same year range as the forcing data.
     Only used if DOCN_MODE=cplhist.

DOCN_IMPORT_FIELDS
   - A column delimited set of import fields that are advertised by DOCN but never used.
     In some cases it is needed for DOCN to advertise import fields even though it never will actually use them.
     This is needed in order for the mediator to add fields that would
     be sent to the ocean from the atmosphere, ice and runoff if the
     ocean were prognostic.
     If the value of DOCN_IMPORT_FIELDS is 'none' then no import fields are advertised
     by DOCN. If the DOCN_IMPORT_FIELDS is set to 'Faxa_bcph:Faxa_dstdry:Faxa_dstwet:So_duu10n:Si_ifrac' then
     these fields will be written to the mediator history file(s).
