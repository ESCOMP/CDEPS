.. _data-model-introduction:

Introduction
============

--------
Overview
--------

The Community Data Models for Earth Predictive Systems (CDEPS)
contains a set of NUOPC-compliant data components along with
ESMF-based "stream" code that enables new capabilities in selectively
removing feedbacks in coupled model systems.  The CDEPS data
models perform the basic function of reading external data files,
modifying those data, and then sending the data back to the CMEPS
mediator.  The fields sent to the mediator are the same as those that
would be sent by an active component.  This takes advantage of the
fact that the mediator and other CMEPS-compliant model components have
no fundamental knowledge of whether another component is fully active
or just a data component.

CDEPS is completely ESMF based. As a result, regridding between the
stream resolution and the model resolution can occur at run time for
any regridding option that ESMF supports. In addition, the PIO library
is used so that all of the stream data is read in parallel by the
CDEPS stream code.

The CDEPS data models typically read gridded data from a variety of sources
including observations, reanalysis products or output data from a
previous model simulation.  Out of the box, they often provide a few
possible data sources and/or time periods that you can choose from
when setting up a case.

In some cases, data models have prognostic functionality, that is,
they also receive and use data sent by the mediator.  However, in most
cases, the data models are not running prognostically and have no need
to receive any data from the mediator.

The CIME data models have parallel capability and share significant
amounts of source code.  Methods for reading and interpolating data
have been established and can easily be reused: The data model calls
strdata ("stream data") methods which then call stream methods.  The
stream methods are responsible for managing lists of input data files
and their time axes.  The information is then passed up to the strdata
methods where the data is read and interpolated in space and time.
The interpolated data is passed up to the data model where final
fields are derived, packed, and returned to the mediator.

---------
Code Tree
---------

CDEPS contains the following code tree:

===============  =========================================
Directory        Function
===============  =========================================
cime_config      CIME Case Control System
cmake            Build (can be used with or without CIME)
datm             data atmosphere component
dice	         data sea-ice component
dlnd	         data land component
docn	         data ocean component
drof	         data river component
dwav	         data wave component
dshr             shared NUOPC cap code
share            shared utility code
streams          code to handle streams
===============  =========================================

------
Design
------

Data models function by reading in different ``streams`` of input
data. A ``stream`` is defined as a set of data files containing a set
of fields, where all the fields are on the same stream mesh and have
the same time coordinates. Data models input falls into two
categories: stream-independent and stream-dependent data.

**stream-dependent-data**
  Stream-dependent input is contained in the input xml file
  ``d{model_name}.streams.xml``, where ``model_name`` can be ``atm``,
  ``ice``, ``lnd``, ``ocn``, ``rof`` or ``wav``.  Multiple streams can
  be specified in the this xml file (see
  :ref:`streams<input-streams>`). In turn, each stream in the xml file
  can be associated with multiple stream input files.  The data across
  all the stream input files must all be on the same stream mesh and
  share the same time coordinates.

**stream-independent-data**
  Stream-independent input is contained in the input namelist file
  file ``d{model_name}_in``. This file specifies a data model mesh
  file, a data model mask file along with other stream-independent
  data model specific configuration variables. In addition, each
  ``d{model_name}_in`` namelist file contains a namelist variable
  ``datamode`` which specifies the additional operations that need to
  be performed on the input streams to create the data model export
  state.

Data models leverage the CDEPS stream code to spatially interpolate the
stream data to the model resolution and temporarlly interpolate the
data to the model time.  The CDEPS stream code carries this out as
follows:

* The two timestamps of input data that bracket the present model time are read first.
  These are called the lower and upper bounds of data and will change as the model advances.
* The lower and upper bound data are then spatially mapped to the
  model grid based upon the in the ``d{model_name}.streams.xml`` node
  ``mapalgo``.  Spatial interpolation only occurs if the input data
  grid and model grid are not identical, and this is determined in the
  strdata module automatically.
* Time interpolation is the final step and is done using a time
  interpolation method specified in the ``d{model_name}.streams.xml``
  node ``tintalgo``.
* A final set of fields is then available to the data model on the
  model grid and for the current model time.
* Each data model component communicates with the NUOPC mediator and
  exchanges fields on only the data model mesh.

-----------------------------------
CDEPS and CIME Control System (CCS)
-----------------------------------

If the CDEPS data models are used in conjunction with the CIME Case Control System (CCS) then the following will also hold:
Each data model has an xml variable in ``env_run.xml`` that specifies the data model mode.
These are: ``DATM_MODE``, ``DICE_MODE``, ``DLND_MODE``, ``DOCN_MODE``, ``DROF_MODE``, ``DWAV_MODE``.
Each data model mode specifies the streams that are associated with that data model.

More details of the data model design are covered in :ref:`design details<design-details>`.
