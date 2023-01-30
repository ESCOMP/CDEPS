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
variable ``DROF_MODE`` sets the collection of streams that
are associated with DROF and also sets the DROF namelist variable
``datamode`` in the file ``drof_in``. The following are the supported
DROF ``datamode`` values, as defined in the file
``namelist_definition_drof.xml``.

The following table describes the valid values of ``DROF_MODE``
(defined in the ``config_component.xml`` file for DROF), and how they
relate to the associated input streams and the ``datamode`` namelist
variable. CIME will generate a value of ``DROF_MODE`` based on the
compset.

CPLHIST
   - Reads data from coupler history file
   - streams: rof.cplhist
   - datamode: copyall

DIATREN_ANN_RX1
   - Annual Average data from Dai_Trenberth Continental Freshwater Discharge
   - streams: rof.diatren_ann_rx1
   - datamode: copyall

DIATREN_ANN_AIS00_RX1
   - Annual Average data from Dai_Trenberth Continental Freshwater Discharge
   - streams: rof.diatren_ann_ais00_rx1
   - datamode: copyall

DIATREN_ANN_AIS45_RX1
   - Annual Average data from Dai_Trenberth Continental Freshwater Discharge
   - streams: rof.diatren_ann_ais45_rx1
   - datamode: copyall

DIATREN_ANN_AIS55_RX1
   - Annual Average data from Dai_Trenberth Continental Freshwater Discharge
   - streams: rof.diatren_ann_ais55_rx1
   - datamode: copyall

DIATREN_IAF_AIS00_RX1
   - Monthly data from Dai_Trenberth Continental Freshwater Discharge
   - streams: rof.diatren_iaf_ais00_rx1
   - datamode: copyall

DIATREN_IAF_RX1
   - Monthly data from Dai_Trenberth Continental Freshwater Discharge
   - streams: rof.diatren_iaf_rx1
   - datamode: copyall

DIATREN_IAF_AIS45_RX1
   - Monthly data from Dai_Trenberth Continental Freshwater Discharge
   - streams: rof.diatren_iaf_ais45_rx1
   - datamode: copyall

DIATREN_IAF_AIS55_RX1
   - Monthly data from Dai_Trenberth Continental Freshwater Discharge
   - streams: rof.diatren_iaf_ais55_rx1
   - datamode: copyall

IAF_JRA_1p4_2018
   - JRA-55 based river runoff data (2018)
   - streams: rof.iaf_jra_1p4_2018
   - datamode: copyall

IAF_JRA
   - JRA-55 based river runoff data
   - streams: rof.iaf_jra
   - datamode: copyall

RYF8485_JRA
   - JRA55 v1.3 1984 May - 1985 Apr RAF
   - streams: rof.ryf8485_jra
   - datamode: copyall

RYF9091_JRA
   - JRA55 v1.3 1990 May - 1991 Apr RAF
   - streams: rof.ryf9091_jra
   - datamode: copyall

RYF0304_JRA
   - JRA55 v1.3 2003 May - 2004 Apr RAF 
   - streams: rof.ryf0304_jra
   - datamode: copyall
