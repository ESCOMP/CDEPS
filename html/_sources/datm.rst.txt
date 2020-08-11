.. _datm-datamodes:

Data Atmosphere (DATM)
======================

DATM is normally used to provide observational forcing data (or
forcing data produced by a previous run using active components) to
drive prognostic components. The various ways of running DATM is referred to as its mode.

In the case of CESM, these would be: CTSM, POP2, MOM6, POP2/CICE5-6
and MOM6/CICE5-6.  As examples, CORE2_NYF (CORE2 normal year forcing)
is the DATM mode used in driving POP2 and MOM6. On the other hand
CLM_QIAN, CLMCRUNCEP, CLMGSWP3 and CLM1PT are DATM modes using
observational data for forcing CTSM.

.. _datm-datamodes:

--------------------
datamode values
--------------------

DATM its own set of supported ``datamode`` values that appears in the
``datm_in`` namelist input.  The datamode specifies what additional
operations need to be done by DATM on *ALL* of the streams in the
``datm.streams.xml`` file.  Each datamode value is also associated
with a DATM source file that carries out these operations and these are
listed in parentheses next to the mode name.

CLMNCEP (``datm_datamode_clmncep_mod.F90``)
  - In conjunction with NCEP climatological atmosphere data, provides
    the atmosphere forcing favored by the Land Model Working Group when
    coupling an active land model with observed atmospheric
    forcing. This mode replicates code previously found in CLM (circa
    2005), before the LMWG started using the CIME coupling
    infrastructure and data models to do active-land-only simulations."

CORE2_NYF (``datm_datamode_core2_mod.F90``)
  - Coordinated Ocean-ice Reference Experiments (CORE) Version 2 Normal Year Forcing."

CORE2_IAF (``datm_datamode_core2_mod.F90``)
  - In conjunction with CORE Version 2 atmospheric forcing data,
    provides the atmosphere forcing when coupling an active ocean model
    with observed atmospheric forcing. This mode and associated data
    sets implement the CORE-IAF Version 2 forcing data, as developed by
    Large and Yeager (2008) at NCAR.  Note that CORE2_NYF and CORE2_IAF
    work exactly the same way.

CORE_IAF_JRA (``datm_datamode_jra_mod.F90``)
  - In conjunction with JRA-55 Project, provides the atmosphere forcing
    when coupling an active ocean model with observed atmospheric
    forcing. This mode and associated data sets implement the JRA-55
    v1.3 forcing data."

ERA5 (``datm_datamode_era5_mod.F90``)
  - Fifth generation ECMWF atmospheric reanalysis of the global climate

.. _datm-cime-vars:

---------------------------------------
Configuring DATM from CIME
---------------------------------------

If CDEPS is coupled to the CIME-CCS then the CIME ``$CASEMROOT`` xml
variable ``DATM_MODE`` sets the collection of streams the streams that
are associated with DATM and also sets the datm namelist variable
``datamode`` in the file ``datm_in``.  The following are the supported
DATM ``datamode`` values, as defined in the file
``namelist_definition_datm.xml``.

The following table describes the valid values of ``DATM_MODE``
(defined in the ``config_component.xml`` file for DATM), and how they
relate to the associated input streams and the ``datamode`` namelist
variable.  CIME will generate a value of ``DATM_MODE`` based on the
compset.

CORE2_NYF,
   - CORE2 normal year forcing (CESM C ang G compsets)
   - streams: CORE2_NYF.GISS,CORE2_NYF.GXGXS,CORE2_NYF.NCEP
   - datamode: CORE2_NYF

CORE2_IAF
   - CORE2 interannual year forcing (CESM C ang G compsets)
   - streams: CORE2_IAF.GCGCS.PREC,CORE2_IAF.GISS.LWDN,
     CORE2_IAF.GISS.SWDN,CORE2_IAF.GISS.SWUP,
     CORE2_IAF.NCEP.DN10,CORE2_IAF.NCEP.Q_10,
     CORE2_IAF.NCEP.SLP_,CORE2_IAF.NCEP.T_10,CORE2_IAF.NCEP.U_10,
     CORE2_IAF.NCEP.V_10,CORE2_IAF.CORE2.ArcFactor
   - datamode: CORE2_IAF

CORE_IAF_JRA
   - JRA-55 intra-annual year forcing (CESM C ang G compsets)
   - streams: CORE_IAF_JRA.PREC,CORE_IAF_JRA.LWDN,CORE_IAF_JRA.SWDN,
     CORE_IAF_JRA.Q_10,CORE_IAF_JRA.SLP_,CORE_IAF_JRA.T_10,CORE_IAF_JRA.U_10,
     CORE_IAF_JRA.V_10,CORE_IAF_JRA.CORE2.ArcFactor
   - datamode: CORE_IAF_JRA

CLM_QIAN_WISO
   - QIAN atm input data with water isotopes (CESM I compsets)
   - streams: CLM_QIAN_WISO.Solar,CLM_QIAN_WISO.Precip,CLM_QIAN_WISO.TPQW
   - datamode: CLMNCEP

CLM_QIAN
   - QIAN atm input data (CESM I compsets)
   - streams: CLM_QIAN.Solar,CLM_QIAN.Precip,CLM_QIAN.TPQW
   - datamode: CLMNCEP

CLMCRUNCEPv7
   - CRUNCEP atm input data (CESM I compsets)
   - streams: CLMCRUNCEP.Solar,CLMCRUNCEP.Precip,CLMCRUNCEP.TPQW
   - datamode: CLMNCEP

CLMGSWP3
   - GSWP3 atm input data (I compsets)
   - streams: CLMGSWP3.Solar,CLMGSWP3.Precip,CLMGSWP3.TPQW
   - datamode: CLMNCEP

CLM1PT
   - single point tower site atm input data
   - streams: CLM1PT.$ATM_GRID
   - datamode: CLMNCEP

CPLHIST
   - user generated forcing data from using coupler history files
     used to spinup relevant prognostic components (for CESM this is CLM, POP and CISM)
   - streams: CPLHISTForcing.Solar,CPLHISTForcing.nonSolarFlux,
   - datamode: CPLHIST
