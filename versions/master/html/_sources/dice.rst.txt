.. _dice:

Data Ice (DICE)
===============

DICE is normally used to provide observational forcing data to
drive prognostic components. The various ways of running DICE is 
referred to as its mode.

.. _dice-datamodes:

--------------------
Supported Data Modes
--------------------

DICE has its own set of supported ``datamode`` values that appears in the
``dice_in`` namelist input. The datamode specifies what additional
operations need to be done by DICE on *ALL* of the streams in the
``dice.streams.xml`` file. Each datamode value is also associated
with a DICE source file that carries out these operations and these are
listed in parentheses next to the mode name.

ssmi (``dice_datamode_ssmi_mod.F90``)
  - It provides sea-ice concantration data, which is
    derived from the passive microwave sensor SSM/I.

ssmi_iaf (``dice_datamode_ssmi_mod.F90``)
  - `ssmi_iaf` is the interannually varying version of `ssmi`.

.. _dice-cime-vars:

---------------------------------------
Configuring DICE from CIME
---------------------------------------

If CDEPS is coupled to the CIME-CCS then the CIME ``$CASEROOT`` xml
variable ``DICE_MODE`` sets the collection of streams that
are associated with DICE and also sets the dice namelist variable
``datamode`` in the file ``dice_in``. The following are the supported
DICE ``datamode`` values, as defined in the file
``namelist_definition_dice.xml``.

The following table describes the valid values of ``DICE_MODE``
(defined in the ``config_component.xml`` file for DICE), and how they
relate to the associated input streams and the ``datamode`` namelist
variable. CIME will generate a value of ``DICE_MODE`` based on the
compset.

ssmi
   - Reads data from file
   - streams: ssmi_nyf
   - datamode: ssmi 

ssmi_iaf
   - Reads data from file 
   - streams: ssmi_iaf
   - datamode: ssmi_iaf 
