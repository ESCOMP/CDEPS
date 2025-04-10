.. _datm:

Data Atmosphere (DATM)
======================

DATM is normally used to provide observational forcing data (or
forcing data produced by a previous run using active components) to
drive prognostic components. The various ways of running DATM is
referred to as its mode.

In the case of CESM, the active model components would be: CTSM,
POP2, MOM6, POP2, CICE5-6, WW3 and MOSART. As examples, CORE2_NYF
(CORE2 normal year forcing) is the DATM mode used in driving
POP2 and MOM6. On the other hand CLM_QIAN, CLMCRUJRA2024, CLMCRUNCEP, CLMGSWP3
and CLM1PT are DATM modes using observational data for forcing CTSM.

.. _datm-datamodes:

--------------------
Supported Data Modes
--------------------

DATM has its own set of supported ``datamode`` values that appear in the
``datm_in`` namelist input. The datamode specifies what additional
operations need to be done by DATM on *ALL* of the streams in the
``datm.streams.xml`` file. Each datamode value is also associated
with a DATM source file that carries out these operations and these are
listed in parentheses next to the mode name.

CLMNCEP (``datm_datamode_clmncep_mod.F90``)
  - In conjunction with NCEP climatological atmosphere data, provides
    the atmosphere forcing favored by the CESM Land Model Working Group
    when coupling an active land model with observed atmospheric
    forcing. This mode replicates code previously found in CLM (circa
    2005), before the LMWG started using the CIME coupling
    infrastructure and data models to do active-land-only simulations.

CORE2_NYF (``datm_datamode_core2_mod.F90``)
  - Coordinated Ocean-ice Reference Experiments (CORE) Version 2
    Normalst_aquap[1-10]Year Forcing.

CORE2_IAF (``datm_datamode_core2_mod.F90``)
  - In conjunction with CORE Version 2 atmospheric forcing data,
    provides the atmosphere forcing when coupling an active ocean model
    with observed atmospheric forcing. This mode and associated data
    sets implement the `CORE-IAF` Version 2 forcing data, as developed by
    Large and Yeager (2008) at NCAR. Note that `CORE2_NYF` and `CORE2_IAF`
    work exactly the same way.

CORE_IAF_JRA (``datm_datamode_jra_mod.F90``)
  - In conjunction with JRA-55 Project, provides the atmosphere forcing
    when coupling an active ocean model with observed atmospheric
    forcing. This mode and associated data sets implement the JRA-55
    v1.3 forcing data.

ERA5 (``datm_datamode_era5_mod.F90``)
  - Fifth generation ECMWF atmospheric reanalysis of the global climate.
    This mode is mainly used by NOAA's UFS Weather model to support
    different applications such as HYCOM ocean and NOAHMP land components.

.. note::
  Due to the high temporal and spatial resoultion of ERA5 dataset, only 2019
  data is staged on NCAR's Derecho platform under
  `$CESMDATAROOT/inputdata/atm/datm7/ERA5`

.. note::
  Using the GEFS data mode, the `CDEPS fork <https://github.com/NOAA-EMC/CDEPS>`_
  used by `NOAA's UFS Weather Model <https://github.com/ufs-community/ufs-weather-model>`_
  supports CFSR, GEFS and GFS data inputs.

.. _datm-cime-vars:

---------------------------------------
Configuring DATM from CIME
---------------------------------------

If CDEPS is coupled to the CIME-CCS then the CIME ``$CASEROOT`` xml
variable ``DATM_MODE`` will be generated based on the compset
specification ``DATM%{DATM_MODE}``.  ``DATM_MODE`` will in term be
used in the ``namelist_definition_datm.xml`` file to determine the
collection of streams that are associated with DATM and also sets the
datm namelist variable ``datamode`` in the file ``datm_in``.

The following list describes the valid values of ``DATM_MODE``
(defined in the ``config_component.xml`` file for DATM), and how they
relate to the associated input streams and the ``datamode`` namelist
variable.

DATM%CORE2_NYF,
   - CORE2 normal year forcing (CESM C ang G compsets)
   - datm_mode: CORE2_NYF
   - streams: CORE2_NYF.GISS,CORE2_NYF.GXGXS,CORE2_NYF.NCEP
   - datamode: CORE2_NYF

DATM%CORE2_IAF,
   - CORE2 interannual year forcing (CESM C ang G compsets)
   - datm_mode: CORE2_IAF
   - streams: CORE2_IAF.GCGCS.PREC,CORE2_IAF.GISS.LWDN,
     CORE2_IAF.GISS.SWDN,CORE2_IAF.GISS.SWUP,
     CORE2_IAF.NCEP.DN10,CORE2_IAF.NCEP.Q_10,
     CORE2_IAF.NCEP.SLP\_,CORE2_IAF.NCEP.T_10,CORE2_IAF.NCEP.U_10,
     CORE2_IAF.NCEP.V_10,CORE2_IAF.CORE2.ArcFactor
   - datamode: CORE2_IAF

DATM%CORE_IAF_JRA
   - JRA-55 intra-annual year forcing (CESM C ang G compsets)
   - streams: CORE_IAF_JRA.PREC,CORE_IAF_JRA.LWDN,CORE_IAF_JRA.SWDN,
     CORE_IAF_JRA.Q_10,CORE_IAF_JRA.SLP\_,CORE_IAF_JRA.T_10,CORE_IAF_JRA.U_10,
     CORE_IAF_JRA.V_10,CORE_IAF_JRA.CORE2.ArcFactor
   - datamode: CORE_IAF_JRA

DATM%CLM_QIAN_WISO
   - QIAN atm input data with water isotopes (CESM I compsets)
   - datm_mode: CLMNCEP
   - streams: CLM_QIAN_WISO.Solar,CLM_QIAN_WISO.Precip,CLM_QIAN_WISO.TPQW
   - datamode: CLMNCEP

DATM%CLM_QIAN
   - QIAN atm input data (CESM I compsets)
   - datm_mode: CLMNCEP
   - streams: CLM_QIAN.Solar,CLM_QIAN.Precip,CLM_QIAN.TPQW
   - datamode: CLMNCEP

DATM%CLMCRUJRA2024
   - CRUJRA atm input data (CESM I compsets)
   - datm_mode: CLMNCEP
   - streams: CLMCRUJRA2024.Solar,CLMCRUJRA2024.Precip,CLMCRUJRA2024.TPQW
   - datamode: CLMNCEP

DATM%CLMCRUNCEPv7
   - CRUNCEP atm input data (CESM I compsets)
   - datm_mode: CLMNCEP
   - streams: CLMCRUNCEP.Solar,CLMCRUNCEP.Precip,CLMCRUNCEP.TPQW
   - datamode: CLMNCEP

DATM%CLMGSWP3
   - GSWP3 atm input data (I compsets)
   - datm_mode: CLMNCEP
   - streams: CLMGSWP3.Solar,CLMGSWP3.Precip,CLMGSWP3.TPQW
   - datamode: CLMNCEP

DATM%CLM1PT
   - single point tower site atm input data
   - datm_mode: CLMNCEP
   - streams: CLM1PT.$ATM_GRID
   - datamode: CLMNCEP

DATM%ERA5
   - ERA5 atm input data (not used any compset)
   - datm_mode: ERA5
   - streams: ERA5_HOURLY
   - datamode: ERA5

DATM%CPLHIST
   - user generated forcing data from using coupler history files
     used to spinup relevant prognostic components (for CESM this is CLM, POP and CISM)
   - datm_mode:CPLHIST
   - streams: CPLHISTForcing.Solar,CPLHISTForcing.nonSolarFlux,
   - datamode: CPLHIST

In addition, the following DATM specific CIME-CCS xml variables will appear in ``$CASEROOT/env_run.xml``:

DATM_PRESAERO
   - DATM prescribed aerosol forcing mode

DATM_TOPO
   - DATM surface topography forcing (only needed for compsets with active land)

DATM_CO2_TSERIES
   - Full pathname for domain file for datm when DATM_MODE is
     CPLHIST, NOTE: if this is set to 'null' (the default), then
     domain information is read in from the first coupler history
     file in the target stream and it is assumed that the first
     coupler stream file that is pointed to contains the domain
     information for that stream.

DATM_CPLHIST_CASE
   - Case name used to determine stream filenames when DATM_MODE is CPLHIST

DATM_YR_START
   -  Starting year to loop data over

DATM_YR_START_FILENAME
   -  Start year listed in PLUMBER2 filename

DATM_YR_END
   -  Ending year to loop data over

DATM_YR_ALIGN
   - Simulation year corresponding to DATM_YR_START. A common usage
     is to set this to RUN_STARTDATE. With this setting, the forcing
     in the first year of the run will be the forcing of year
     DATM_YR_START. Another use case is to align the calendar
     of transient forcing with the model calendar. For example,
     setting DATM_YR_ALIGN=DATM_YR_START will lead to
     the forcing calendar being the same as the model calendar. The
     forcing for a given model year would be the forcing of the same
     year. This would be appropriate in transient runs where the
     model calendar is setup to span the same year range as the
     forcing data.

DATM_SKIP_RESTART_READ
   -  If set to true, than datm restarts will not be read on a continuation run.
      This capability is used, for example, in CTSM spinup runs.


     
