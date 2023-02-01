.. _drof:

Data Runoff (DROF)
==================

DROF is normally used to provide observational forcing data to
drive prognostic components. The various ways of running DROF is 
referred to as its mode.

.. _drof-datamodes:

--------------------
Supported Data Modes
--------------------

DROF has its own set of supported ``datamode`` values that appears in the
``drof_in`` namelist input. The datamode specifies what additional
operations need to be done by DROF on *ALL* of the streams in the
``drof.streams.xml`` file. Each datamode value is also associated
with a DROF source file that carries out these operations and these are
listed in parentheses next to the mode name.

copyall (``rof_comp_nuopc.F90``)
  - The runoff data is associated with the river model. Copies all 
    fields directly from the input data streams Any required
    fields not found on an input stream will be set to zero.

.. _drof-cime-vars:

---------------------------------------
Configuring DROF from CIME
---------------------------------------

If CDEPS is coupled to the CIME-CCS then the CIME ``$CASEROOT`` xml
variable ``DROF_MODE`` will be generated based on the compset
specification ``DROF%{DROF_MODE}``.  ``DROF_MODE`` will in term be
used in the ``namelist_definition_drof.xml`` file to determine the
collection of streams that are associated with DROF and also sets the
drof namelist variable ``datamode`` in the file ``drof_in``.

The following list describes the valid values of ``DROF_MODE``
(defined in the ``config_component.xml`` file for DROF), and how they
relate to the associated input streams and the ``datamode`` namelist
variable.

DROF%CPLHIST
   - Reads data from coupler history file
   - drof_mode: CPLHIST
   - streams: rof.cplhist
   - datamode: copyall

DROF%DIATREN_ANN_RX1
   - Annual Average data from Dai_Trenberth Continental Freshwater Discharge
   - drof_mode: DIATREN_ANN_RX1
   - streams: rof.diatren_ann_rx1
   - datamode: copyall

DROF%DIATREN_ANN_AIS00_RX1
   - Annual Average data from Dai_Trenberth Continental Freshwater Discharge
   - drof_mode: DIATREN_ANN_AIS00_RX1
   - streams: rof.diatren_ann_ais00_rx1
   - datamode: copyall

DROF%DIATREN_ANN_AIS45_RX1
   - Annual Average data from Dai_Trenberth Continental Freshwater Discharge
   - drof_mode: DIATREN_ANN_AIS45_RX1
   - streams: rof.diatren_ann_ais45_rx1
   - datamode: copyall

DROF%DIATREN_ANN_AIS55_RX1
   - Annual Average data from Dai_Trenberth Continental Freshwater Discharge
   - drof_mode: DIATREN_ANN_AIS55_RX1
   - streams: rof.diatren_ann_ais55_rx1
   - datamode: copyall

DROF%DIATREN_IAF_RX1
   - Monthly data from Dai_Trenberth Continental Freshwater Discharge
   - drof_mode: DIATREN_IAF_RX1
   - streams: rof.diatren_iaf_rx1
   - datamode: copyall

DROF%DIATREN_IAF_AIS00_RX1
   - Monthly data from Dai_Trenberth Continental Freshwater Discharge
   - drof_mode: DIATREN_IAF_AIS00_RX1
   - streams: rof.diatren_iaf_ais00_rx1
   - datamode: copyall

DROF%DIATREN_IAF_AIS45_RX1
   - Monthly data from Dai_Trenberth Continental Freshwater Discharge
   - drof_mode: DIATREN_IAF_AIS45_RX1
   - streams: rof.diatren_iaf_ais45_rx1
   - datamode: copyall

DROF%DIATREN_IAF_AIS55_RX1
   - Monthly data from Dai_Trenberth Continental Freshwater Discharge
   - drof_mode: DIATREN_IAF_AIS55_RX1
   - streams: rof.diatren_iaf_ais55_rx1
   - datamode: copyall

DROF%IAF_JRA
   - JRA-55 based river runoff data
   - drof_mode: IAF_JRA
   - streams: rof.iaf_jra
   - datamode: copyall

DROF%IAF_JRA_1p4_2018
   - JRA-55 based river runoff data (2018)
   - drof_mode: IAF_JRA_1p4_2018
   - streams: rof.iaf_jra_1p4_2018
   - datamode: copyall

DROF%RYF8485_JRA
   - JRA55 v1.3 data from 1984 May - 1985 Apr RAF
   - drof_mode: RYF8485_JRA
   - streams: rof.ryf8485_jra
   - datamode: copyall

DROF%RYF9091_JRA
   - JRA55 v1.3 data from 1990 May - 1991 Apr RAF
   - drof_mode: RYF9091_JRA
   - streams: rof.ryf9091_jra
   - datamode: copyall

DROF%RYF0304_JRA
   - JRA55 v1.3 data form 2003 May - 2004 Apr RAF 
   - drof_mode: RYF0304_JRA
   - streams: rof.ryf0304_jra
   - datamode: copyall

In addition, the following DATM specific CIME-CCS xml variables will appear in ``$CASEROOT/env_run.xml``:

DROF_CPLHIST_CASE
   - Case name used to determine stream filenames when DROF_MODE is CPLHIST

DROF_CPLHIST_YR_START
   - starting year to loop data over (only used when DROF_MODE is CPLHIST)

DROF_CPLHIST_YR_END
   - ending year to loop data over (only used when DROF_MODE is CPLHIST)

DROF_CPLHIST_YR_ALIGN
   - Simulation year corresponding to DROF_CPLHIST_YR_START (only used
     when DROF_MODE is CPLHIST). A common usage is to set this to
     RUN_STARTDATE. With this setting, the forcing in the first year of
     the run will be the forcing of year DROF_CPLHIST_YR_START. Another
     use case is to align the calendar of transient forcing with the
     model calendar. For example, setting
     DROF_CPLHIST_YR_ALIGN=DROF_CPLHIST_YR_START will lead to the
     forcing calendar being the same as the model calendar. The forcing
     for a given model year would be the forcing of the same year. This
     would be appropriate in transient runs where the model calendar is
     setup to span the same year range as the forcing data.

DROF_SKIP_RESTART_READ     
   - If set to true, than drof restarts will not be read on a continuation run
