!===============================================================================
!
!  Module: datm_datamode_ufs_mod
!
!  Description:
!    Configurable datamode for UFS. Replaces hardcoded datamodes with a dynamic, 
!    configuration-driven approach. 
!    
!    - datm_datamode_ufs_advertise: Parses config and advertises stream variables.
!    - datm_datamode_ufs_init_pointers: Caches pass-through pointers in state object.
!    - datm_datamode_ufs_advance: Executes raw copy and chained calculations in-place.
!
!===============================================================================
module datm_datamode_ufs_mod
  
  use ESMF
  use NUOPC
  
  ! CDEPS-native imports
  use shr_kind_mod,     only: r8 => shr_kind_r8
  use dshr_strdata_mod, only: shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_methods_mod, only: dshr_state_getfldptr, chkerr
  use shr_log_mod,      only: shr_log_error
  use dshr_stream_mod,  only: shr_stream_streamType, shr_stream_init_from_esmfconfig
  use dshr_fldList_mod, only: fldList_type, dshr_fldList_add

  implicit none
  private
  
  public :: datm_datamode_ufs_advertise
  public :: datm_datamode_ufs_init_pointers
  public :: datm_datamode_ufs_advance
  public :: ufs_datamode_state

  character(len=*), parameter :: u_FILE_u = __FILE__

  !> \brief Dynamic mapping for pass-through variables
  type :: ufs_var_map
     character(len=64) :: var_name
     real(r8), pointer :: ptr_strm(:) => null()
     real(r8), pointer :: ptr_exp(:)  => null()
  end type ufs_var_map

  !> \brief Instance-specific state object for thread-safe operations
  type :: ufs_datamode_state
     type(ufs_var_map), allocatable :: var_maps(:)
  end type ufs_datamode_state

contains

  subroutine add_stream_variables_to_export(streamfilename, fldsExport, ufs_state, rc)
    character(len=*),         intent(in)    :: streamfilename
    type(dshr_fldList_type),  intent(inout) :: fldsExport
    type(ufs_datamode_state), intent(inout) :: ufs_state
    integer,                  intent(out)   :: rc

    rc = ESMF_SUCCESS

    ! allocate streamdat instance on all tasks
    nstrms = 0

    ! set ESMF config
    cf =  ESMF_ConfigCreate(rc=RC)
    call ESMF_ConfigLoadFile(config=CF ,filename=trim(streamfilename), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! get number of streams
    nstrms = ESMF_ConfigGetLen(config=CF, label='stream_info:', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! allocate an array of shr_stream_streamtype objects
    if (nstrms > 0) then
      allocate(streamdat(nstrms), stat=istat)
      if ( istat /= 0 ) then
         call shr_log_error(subName//': allocation error for streamdat with size '//toString(nstrms),rc=rc)
         return
      end if
    else
      call shr_log_error("no stream_info in config file "//trim(streamfilename), rc=rc)
      return
    endif

    do i=1, nstrms
      ! Get name of stream variables in file and model
      streamdat(i)%nvars = ESMF_ConfigGetLen(config=CF, label="stream_data_variables"//mystrm//':', rc=rc)
      if( streamdat(i)%nvars > 0) then
        allocate(streamdat(i)%varlist(streamdat(i)%nvars), stat=istat)
        if ( istat /= 0 ) then
           call shr_log_error(subName//&
                ': allocation error for streamdat('//toString(i)//')%varlist'//&
                ' with size '//toString(streamdat(i)%nvars), rc=rc)
           return
        end if
        allocate(strm_tmpstrings(streamdat(i)%nvars), stat=istat)
        if ( istat /= 0 ) then
           call shr_log_error(subName//&
                ': allocation error for strm_tmpstrings('//toString(i)//')%varlist'//&
                ' with size '//toString(streamdat(i)%nvars), rc=rc)
           return
        end if
        call ESMF_ConfigGetAttribute(CF,valueList=strm_tmpstrings,label="stream_data_variables"//mystrm//':', rc=rc)
        do n=1, streamdat(i)%nvars
          streamdat(i)%varlist(n)%nameinfile = strm_tmpstrings(n)(1:index(trim(strm_tmpstrings(n)), " "))
          streamdat(i)%varlist(n)%nameinmodel = strm_tmpstrings(n)(index(trim(strm_tmpstrings(n)), " ", .true.)+1:)

          call append_var_map(ufs_state%var_maps, trim(streamdat(i)%varlist(n)%nameinmodel))
          call dshr_fldList_add(fldsExport, trim(streamdat(i)%varlist(n)%nameinmodel))
        enddo
        deallocate(strm_tmpstrings)
      else
         call shr_log_error("stream data variables must be provided", rc=rc)
         return
      endif
    end do ! i nstrms

  end subroutine add_stream_variables_to_export

  !=============================================================================
  ! \brief Reads config streams and advertises to CDEPS field list
  !=============================================================================
  subroutine datm_datamode_ufs_advertise(exportState, fldsExport, ufs_state, flds_scalar_name, rc)

    type(esmf_State)        , intent(inout) :: exportState
    type(fldList_type),       pointer       :: fldsExport
    type(ufs_datamode_state), intent(inout) :: ufs_state
    character(len=*)        , intent(in)    :: flds_scalar_name
    integer,                  intent(out)   :: rc
    
    type(shr_stream_streamType), pointer    :: streamdat(:)
    integer :: istrm, ivar
    
    character(len=18) :: streamfilename = 'datm.streams'
    type(fldlist_type), pointer :: fldList
    
    rc = ESMF_SUCCESS

    ! parse datm.streams stream_variables to fldsExport and ufs_state
    call add_stream_variables_to_export(streamfilename, fldsExport, ufs_state, rc)
    ! deallocate(streamdat)

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(datm_comp_advertise): Fr_atm '//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine datm_datamode_ufs_advertise


  !=============================================================================
  ! \brief Caches pass-through pointers natively into the instance state object
  !=============================================================================
  subroutine datm_datamode_ufs_init_pointers(exportState, sdat, ufs_state, rc)
    type(ESMF_State),         intent(inout) :: exportState
    type(shr_strdata_type),   intent(inout) :: sdat
    type(ufs_datamode_state), intent(inout) :: ufs_state
    integer,                  intent(out)   :: rc
    
    character(len=*), parameter :: subName = 'datm_datamode_ufs_init_pointers: '
    integer :: i

    rc = ESMF_SUCCESS

    ! Cache standard pass-through pointers to eliminate overhead in the run loop
    if (allocated(ufs_state%var_maps)) then
      do i = 1, size(ufs_state%var_maps)
        call shr_strdata_get_stream_pointer(sdat, trim(ufs_state%var_maps(i)%var_name), &
             ufs_state%var_maps(i)%ptr_strm, requirePointer=.true., &
             errmsg=trim(subName)//'ERROR: stream pointer missing for '//trim(ufs_state%var_maps(i)%var_name), rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        
        call dshr_state_getfldptr(exportState, trim(ufs_state%var_maps(i)%var_name), &
             fldptr1=ufs_state%var_maps(i)%ptr_exp, allowNullReturn=.true., rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      end do
    end if

  end subroutine datm_datamode_ufs_init_pointers


  !=============================================================================
  ! \brief Core run loop. Performs array copies and triggers calculation driver.
  !=============================================================================
  subroutine datm_datamode_ufs_advance(exportState, ufs_state, calc_opts, rc)
    type(ESMF_State),         intent(inout) :: exportState
    type(ufs_datamode_state), intent(inout) :: ufs_state
    character(len=*),         intent(in)    :: calc_opts
    integer,                  intent(out)   :: rc
    
    integer :: i
    rc = ESMF_SUCCESS

    ! -------------------------------------------------------------------------
    ! Phase 1: Ingestion (Stream -> Export State)
    ! 100% Native, 100% Thread-Safe Array Math
    ! -------------------------------------------------------------------------
    if (allocated(ufs_state%var_maps)) then
      do i = 1, size(ufs_state%var_maps)
        if (associated(ufs_state%var_maps(i)%ptr_exp) .and. associated(ufs_state%var_maps(i)%ptr_strm)) then
          ufs_state%var_maps(i)%ptr_exp(:) = ufs_state%var_maps(i)%ptr_strm(:)
        end if
      end do
    end if

    ! -------------------------------------------------------------------------
    ! Phase 2: Chained Calculations Driver
    ! -------------------------------------------------------------------------
    if (len_trim(calc_opts) > 0) then
      call datm_datamode_ufs_calc_driver(exportState, calc_opts, rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if
    
  end subroutine datm_datamode_ufs_advance


  !=============================================================================
  ! \brief Driver subroutine to route execution based on calc_opts string
  !=============================================================================
  subroutine datm_datamode_ufs_calc_driver(exportState, calc_opts, rc)
    type(ESMF_State), intent(inout) :: exportState
    character(len=*), intent(in)    :: calc_opts
    integer,          intent(out)   :: rc
    
    rc = ESMF_SUCCESS

    if (index(calc_opts, 'convert_precip_accum_to_rate') > 0) then
      call calc_convert_precip_accum_to_rate(exportState, rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    if (index(calc_opts, 'convert_rad_accum_to_flux') > 0) then
      call calc_convert_rad_accum_to_flux(exportState, rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    if (index(calc_opts, 'partition_sw_4band') > 0) then
      call calc_partition_sw_4band(exportState, rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    if (index(calc_opts, 'partition_precip_freezing') > 0) then
      call calc_partition_precip_freezing(exportState, rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

  end subroutine datm_datamode_ufs_calc_driver


  !=============================================================================
  ! \brief Subroutine to dynamically append variables via Fortran 2003 move_alloc
  !=============================================================================
  subroutine append_var_map(maps_array, new_var_name)
    type(ufs_var_map), allocatable, intent(inout) :: maps_array(:)
    character(len=*),               intent(in)    :: new_var_name
    
    type(ufs_var_map), allocatable :: temp_maps(:)
    integer :: current_size

    if (allocated(maps_array)) then
      current_size = size(maps_array)
      allocate(temp_maps(current_size + 1))
      temp_maps(1:current_size) = maps_array
      temp_maps(current_size + 1)%var_name = trim(new_var_name)
      call move_alloc(from=temp_maps, to=maps_array)
    else
      allocate(maps_array(1))
      maps_array(1)%var_name = trim(new_var_name)
    end if
  end subroutine append_var_map


  !=============================================================================
  ! Modular Calculation Subroutines
  !=============================================================================

  subroutine calc_convert_precip_accum_to_rate(exportState, rc)
    type(ESMF_State), intent(inout) :: exportState
    integer,          intent(out)   :: rc
    
    character(len=*), parameter :: subName = 'calc_convert_precip_accum_to_rate: '
    real(r8), pointer :: Faxa_prec(:) => null()

    rc = ESMF_SUCCESS
    call dshr_state_getfldptr(exportState, 'Faxa_prec', fldptr1=Faxa_prec, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    
    if (.not. associated(Faxa_prec)) then
      call shr_log_error(trim(subName)//'ERROR: Faxa_prec required for precip calculation.', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    if (associated(Faxa_prec)) then
      Faxa_prec(:) = Faxa_prec(:) * (1000.0_r8 / 3600.0_r8)
    end if
  end subroutine calc_convert_precip_accum_to_rate


  subroutine calc_convert_rad_accum_to_flux(exportState, rc)
    type(ESMF_State), intent(inout) :: exportState
    integer,          intent(out)   :: rc
    
    character(len=*), parameter :: subName = 'calc_convert_rad_accum_to_flux: '
    real(r8), pointer :: Faxa_swdn(:) => null()
    real(r8), pointer :: Faxa_lwdn(:) => null()

    rc = ESMF_SUCCESS
    call dshr_state_getfldptr(exportState, 'Faxa_swdn', fldptr1=Faxa_swdn, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_lwdn', fldptr1=Faxa_lwdn, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    
    ! Apply conversion safely to whatever radiation variables the user provided
    if (associated(Faxa_swdn)) Faxa_swdn(:) = Faxa_swdn(:) / 3600.0_r8
    if (associated(Faxa_lwdn)) Faxa_lwdn(:) = Faxa_lwdn(:) / 3600.0_r8
  end subroutine calc_convert_rad_accum_to_flux


  subroutine calc_partition_sw_4band(exportState, rc)
    type(ESMF_State), intent(inout) :: exportState
    integer,          intent(out)   :: rc
    
    character(len=*), parameter :: subName = 'calc_partition_sw_4band: '
    real(r8), pointer :: Faxa_swdn(:)  => null()
    real(r8), pointer :: Faxa_swndr(:) => null()
    real(r8), pointer :: Faxa_swvdr(:) => null()
    real(r8), pointer :: Faxa_swndf(:) => null()
    real(r8), pointer :: Faxa_swvdf(:) => null()
    
    rc = ESMF_SUCCESS
    
    call dshr_state_getfldptr(exportState, 'Faxa_swdn',  fldptr1=Faxa_swdn,  rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swndr', fldptr1=Faxa_swndr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swvdr', fldptr1=Faxa_swvdr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swndf', fldptr1=Faxa_swndf, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swvdf', fldptr1=Faxa_swvdf, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    
    if (.not. associated(Faxa_swdn)) then
      call shr_log_error(trim(subName)//'ERROR: Faxa_swdn required as input for sw partitioning.', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    ! Only write to the output fields that the user explicitly allocated/advertised
    if (associated(Faxa_swdn)) then
      if (associated(Faxa_swndr)) Faxa_swndr(:) = Faxa_swdn(:) * 0.25_r8
      if (associated(Faxa_swvdr)) Faxa_swvdr(:) = Faxa_swdn(:) * 0.25_r8
      if (associated(Faxa_swndf)) Faxa_swndf(:) = Faxa_swdn(:) * 0.25_r8
      if (associated(Faxa_swvdf)) Faxa_swvdf(:) = Faxa_swdn(:) * 0.25_r8
    end if
  end subroutine calc_partition_sw_4band


  subroutine calc_partition_precip_freezing(exportState, rc)
    type(ESMF_State), intent(inout) :: exportState
    integer,          intent(out)   :: rc
    
    character(len=*), parameter :: subName = 'calc_partition_precip_freezing: '
    real(r8), pointer :: Faxa_prec(:) => null()
    real(r8), pointer :: Faxa_prrn(:) => null()
    real(r8), pointer :: Faxa_prsn(:) => null()

    rc = ESMF_SUCCESS
    
    call dshr_state_getfldptr(exportState, 'Faxa_prec', fldptr1=Faxa_prec, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_prrn', fldptr1=Faxa_prrn, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_prsn', fldptr1=Faxa_prsn, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (.not. associated(Faxa_prec)) then
      call shr_log_error(trim(subName)//'ERROR: Faxa_prec required as input for precip partitioning.', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    if (associated(Faxa_prec)) then
      if (associated(Faxa_prrn)) Faxa_prrn(:) = Faxa_prec(:) * 0.5_r8 
      if (associated(Faxa_prsn)) Faxa_prsn(:) = Faxa_prec(:) * 0.5_r8
    end if
  end subroutine calc_partition_precip_freezing

end module datm_datamode_ufs_mod
