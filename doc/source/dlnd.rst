.. _dlnd:

Data Land (DLND)
================

DLND is normally used to provide forcing data produced by a previous run
using active components to drive prognostic components. Currently, there
is one way of running DLND is referred to as its mode.

.. _dlnd-datamodes:

--------------------
Supported Data Modes
--------------------

DLND has its own set of supported ``datamode`` values that appears in the
``dlnd_in`` namelist input. The datamode specifies what additional
operations need to be done by DLND on *ALL* of the streams in the
``dlnd.streams.xml`` file. Each datamode value is also associated
with a DLND source file that carries out these operations and these are
listed in parentheses next to the mode name.

copyall (``lnd_comp_nuopc.F90``)
  - This mode assumes that the data file has following variables:
    1. Surface temperature (`Sl_tsrf_elev`) in each elevation class
    2. Surface topography (`Sl_topo_elev`) in each elevation class
    3. SMB flux (`Flgl_qice_elev`) in each elevation class

.. _dlnd-cime-vars:

---------------------------------------
Configuring DLND using the CIME-CCS
---------------------------------------

If CDEPS is coupled to the CIME-CCS then the CIME ``$CASEROOT`` xml
variable ``DLND_MODE`` will be generated based on the compset
specification ``DLND%{DLND_MODE}``.  ``DLND_MODE`` will in term be
used in the ``namelist_definition_dlnd.xml`` file to determine the
collection of streams that are associated with DLND and also sets the
dlnd namelist variable ``datamode`` in the file ``dlnd_in``.

The following list describes the valid values of ``DLND_MODE``
(defined in the ``config_component.xml`` file for DLND), and how they
relate to the associated input streams and the ``datamode`` namelist
variable.

DLND%LCPL
   - Non-snow coupling mode. Land forcing data (produced by CLM) from a previous
     model run is read in from a coupler history file.
   - dlnd_mode: SCPL
   - streams: lnd.cplhist
   - datamode: copyall

DLND%SCPL
   - Snow coupling mode. Glacier coupling data (produced by CISM) from a previous
     model run is read in from a coupler history file.
   - dlnd_mode: LCPL
   - streams: sno.cplhist
   - datamode: copyall

In addition, the following DLND specific CIME-CCS xml variables will appear in ``$CASEROOT/env_run.xml``:

DLND_CPLHIST_DIR
   - directory for coupler history data mode

DLND_CPLHIST_CASE
   - case name for coupler history data mode

DLND_CPLHIST_YR_START
    - starting year to loop data over

DLND_CPLHIST_YR_ALIGN
   - Simulation year corresponding to DLND_CPLHIST_YR_START (only used
     when DLND_MODE is CPLHIST or GLC_CPLHIST). A common usage is to
     set this to RUN_STARTDATE. With this setting, the forcing in the
     first year of the run will be the forcing of year
     DLND_CPLHIST_YR_START. Another use case is to align the calendar
     of transient forcing with the model calendar. For example, setting
     DLND_CPLHIST_YR_ALIGN=DLND_CPLHIST_YR_START will lead to the
     forcing calendar being the same as the model calendar. The forcing
     for a given model year would be the forcing of the same year. This
     would be appropriate in transient runs where the model calendar is
     setup to span the same year range as the forcing data.

DLND_SKIP_RESTART_READ
   - If set to true, than dlnd restarts will not be read on a continuation run.
