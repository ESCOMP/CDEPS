.. _dwav:

Data Wave (DWAV)
==================

DWAV is normally used to provide observational forcing data (or
forcing data produced by a previous run using active components) to
drive prognostic components. Currently, there is only one way of 
running DWAV is referred to as its mode.

.. _dwav-datamodes:

--------------------
Supported Data Modes
--------------------

DWAV has its own set of supported ``datamode`` values that appears in the
``dwav_in`` namelist input. The datamode specifies what additional
operations need to be done by DWAV on *ALL* of the streams in the
``dwav.streams.xml`` file. Each datamode value is also associated
with a DWAV source file that carries out these operations and these are
listed in parentheses next to the mode name.

copyall (``wav_comp_nuopc.F90``)
  - The wave data is associated with the wave model and is normally on 
    a different grid than the ocean data. This mode copies all fields 
    directly from the input data streams Any required fields not found 
    on an input stream will be set to zero.

.. note::
  The following fields are supported through the data wave component:

  *Sw_lamult*  = Langmuir multiplier

  *Sw_ustokes* = Stokes drift u component

  *Sw_vstokes* = Stokes drift v component

.. _dwav-cime-vars:

---------------------------------------
Configuring DWAV using the CIME-CCS
---------------------------------------

If CDEPS is coupled to the CIME-CCS then the CIME ``$CASEROOT`` xml
variable ``DWAV_MODE`` will be generated based on the compset
specification ``DWAV%{DWAV_MODE}``.  ``DWAV_MODE`` will in term be
used in the ``namelist_definition_dwav.xml`` file to determine the
collection of streams that are associated with DWAV and also sets the
dwav namelist variable ``datamode`` in the file ``dwav_in``.

The following list describes the valid values of ``DWAV_MODE``
(defined in the ``config_component.xml`` file for DWAV), and how they
relate to the associated input streams and the ``datamode`` namelist
variable.

DWAV%CLIMO
   - Reads data from wave climatology file
   - dwav_mode: CLIMO
   - streams: climo
   - datamode: copyall

In addition, the following DICE specific CIME-CCS xml variables will appear in ``$CASEROOT/env_run.xml``:

DWAV_SKIP_RESTART_READ
   - If set to true, than dice restarts will not be read on a continuation run.
     
