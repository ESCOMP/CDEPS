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
Configuring DLND from CIME
---------------------------------------

If CDEPS is coupled to the CIME-CCS then the CIME ``$CASEROOT`` xml
variable ``DLND_MODE`` sets the collection of streams that
are associated with DLND and also sets the dlnd namelist variable
``datamode`` in the file ``dlnd_in``. The following are the supported
DLND ``datamode`` values, as defined in the file
``namelist_definition_dlnd.xml``.

The following table describes the valid values of ``DLND_MODE``
(defined in the ``config_component.xml`` file for DLND), and how they
relate to the associated input streams and the ``datamode`` namelist
variable. CIME will generate a value of ``DLND_MODE`` based on the
compset.

LCPL
   - Non-snow coupling mode. Land forcing data (produced by CLM) from a previous
      model run is read in from a coupler history file.
   - streams: lnd.cplhist
   - datamode: copyall

SCPL
   - Snow coupling mode. Glacier coupling data (produced by CISM) from a previous
     model run is read in from a coupler history file.
   - streams: sno.cplhist
   - datamode: copyall
