module datm_datamode_cplhist_mod

  use ESMF             , only : ESMF_SUCCESS, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_State
  use ESMF             , only : ESMF_StateItem_Flag, ESMF_STATEITEM_NOTFOUND, operator(/=)
  use ESMF             , only : ESMF_StateGet
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_log_mod      , only : shr_log_error
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_strdata_mod , only : shr_strdata_type
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add

  implicit none
  private ! except

  public  :: datm_datamode_cplhist_advertise
  public  :: datm_datamode_cplhist_init_pointers
  public  :: datm_datamode_cplhist_advance


  ! export state data pointers

  real(r8), pointer :: Sa_z(:)            => null()
  real(r8), pointer :: Sa_tbot(:)         => null()
  real(r8), pointer :: Sa_ptem(:)         => null()
  real(r8), pointer :: Sa_shum(:)         => null()
  real(r8), pointer :: Sa_dens(:)         => null()
  real(r8), pointer :: Sa_pbot(:)         => null()
  real(r8), pointer :: Sa_pslv(:)         => null()
  real(r8), pointer :: Sa_u(:)            => null()
  real(r8), pointer :: Sa_v(:)            => null()
  real(r8), pointer :: Faxa_rainc(:)      => null()
  real(r8), pointer :: Faxa_rainl(:)      => null()
  real(r8), pointer :: Faxa_snowc(:)      => null()
  real(r8), pointer :: Faxa_snowl(:)      => null()
  real(r8), pointer :: Faxa_lwdn(:)       => null()
  real(r8), pointer :: Faxa_swndr(:)      => null()
  real(r8), pointer :: Faxa_swndf(:)      => null()
  real(r8), pointer :: Faxa_swvdr(:)      => null()
  real(r8), pointer :: Faxa_swvdf(:)      => null()

  ! stream data pointers

  real(r8), pointer :: strm_Sa_z   (:)    => null()
  real(r8), pointer :: strm_Sa_tbot(:)    => null()
  real(r8), pointer :: strm_Sa_ptem(:)    => null()
  real(r8), pointer :: strm_Sa_shum(:)    => null()
  real(r8), pointer :: strm_Sa_pbot(:)    => null()
  real(r8), pointer :: strm_Sa_dens(:)    => null()
  real(r8), pointer :: strm_Sa_pslv(:)    => null()
  real(r8), pointer :: strm_Sa_u(:)       => null()
  real(r8), pointer :: strm_Sa_v(:)       => null()
  real(r8), pointer :: strm_Faxa_swndr(:) => null()
  real(r8), pointer :: strm_Faxa_swvdr(:) => null()
  real(r8), pointer :: strm_Faxa_swndf(:) => null()
  real(r8), pointer :: strm_Faxa_swvdf(:) => null()
  real(r8), pointer :: strm_Faxa_rainc(:) => null()
  real(r8), pointer :: strm_Faxa_rainl(:) => null()
  real(r8), pointer :: strm_Faxa_snowc(:) => null()
  real(r8), pointer :: strm_Faxa_snowl(:) => null()
  real(r8), pointer :: strm_Faxa_lwdn (:) => null()

  character(*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine datm_datamode_cplhist_advertise(exportState, fldsexport, flds_scalar_name, rc)

    ! input/output variables
    type(esmf_State)   , intent(inout) :: exportState
    type(fldlist_type) , pointer       :: fldsexport
    character(len=*)   , intent(in)    :: flds_scalar_name
    integer            , intent(out)   :: rc

    ! local variables
    type(fldlist_type), pointer :: fldList
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
    call dshr_fldList_add(fldsExport, 'Sa_topo'    )
    call dshr_fldList_add(fldsExport, 'Sa_z'       )
    call dshr_fldList_add(fldsExport, 'Sa_ptem'    )
    call dshr_fldList_add(fldsExport, 'Sa_dens'    )
    call dshr_fldList_add(fldsExport, 'Sa_pslv'    )
    call dshr_fldList_add(fldsExport, 'Sa_tbot'    )
    call dshr_fldList_add(fldsExport, 'Sa_pbot'    )
    call dshr_fldList_add(fldsExport, 'Sa_shum'    )

    call dshr_fldList_add(fldsExport, 'Sa_u'       )
    call dshr_fldList_add(fldsExport, 'Sa_v'       )

    call dshr_fldList_add(fldsExport, 'Faxa_swndr' )
    call dshr_fldList_add(fldsExport, 'Faxa_swvdr' )
    call dshr_fldList_add(fldsExport, 'Faxa_swndf' )
    call dshr_fldList_add(fldsExport, 'Faxa_swvdf' )

    call dshr_fldList_add(fldsExport, 'Faxa_rainc' )
    call dshr_fldList_add(fldsExport, 'Faxa_rainl' )
    call dshr_fldList_add(fldsExport, 'Faxa_snowc' )
    call dshr_fldList_add(fldsExport, 'Faxa_snowl' )
    call dshr_fldList_add(fldsExport, 'Faxa_lwdn'  )

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(datm_comp_advertise): Fr_atm'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine datm_datamode_cplhist_advertise

  !===============================================================================
  subroutine datm_datamode_cplhist_init_pointers(importState, exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: importState
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(datm_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! get export state pointers
    call dshr_state_getfldptr(exportState, 'Sa_z'       , fldptr1=Sa_z       , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_tbot'    , fldptr1=Sa_tbot    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_pbot'    , fldptr1=Sa_pbot    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_pslv'    , fldptr1=Sa_pslv    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_ptem'    , fldptr1=Sa_ptem    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_shum'    , fldptr1=Sa_shum    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_dens'    , fldptr1=Sa_dens    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_u'       , fldptr1=Sa_u       , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_v'       , fldptr1=Sa_v       , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_rainc' , fldptr1=Faxa_rainc , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_rainl' , fldptr1=Faxa_rainl , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_snowc' , fldptr1=Faxa_snowc , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_snowl' , fldptr1=Faxa_snowl , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_lwdn'  , fldptr1=Faxa_lwdn  , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swvdr' , fldptr1=Faxa_swvdr , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swvdf' , fldptr1=Faxa_swvdf , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swndr' , fldptr1=Faxa_swndr , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swndf' , fldptr1=Faxa_swndf , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Set pointers into stream data

    call shr_strdata_get_stream_pointer(sdat, 'Sa_z', strm_Sa_z, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Sa_z must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_tbot', strm_Sa_tbot, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Sa_tbot must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_ptem', strm_Sa_ptem, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Sa_ptem must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_shum', strm_Sa_shum, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Sa_shum must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_pbot', strm_Sa_pbot, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Sa_pbot must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_dens', strm_Sa_dens, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Sa_wind must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_pslv', strm_Sa_pslv, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Sa_pslv must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_dens', strm_Sa_dens, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Sa_dens must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_u', strm_Sa_u, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Sa_u must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_v', strm_Sa_v, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Sa_v must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_rainc', strm_Faxa_rainc, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Faxa_rainc must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_rainl', strm_Faxa_rainl, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Faxa_rainl must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_snowc', strm_Faxa_snowc, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Faxa_snowc must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_snowl', strm_Faxa_snowl, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Faxa_snowl must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_lwdn', strm_Faxa_lwdn, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Faxa_lwdn must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_swndr', strm_Faxa_swndr, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Faxa_swndr must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_swvdr', strm_Faxa_swvdr, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Faxa_swvdr must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_swndf', strm_Faxa_swndf, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Faxa_swndf must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_swvdf', strm_Faxa_swvdf, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Faxa_swvdf must be associated for cplhist datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_lwdn', strm_Faxa_lwdn, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Faxa_lwdn must be associated for clmncep datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

 end subroutine datm_datamode_cplhist_init_pointers

  !===============================================================================
  subroutine datm_datamode_cplhist_advance(rc)

    ! input/output variables
    integer, intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(datm_datamode_cplhist_advance): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    Sa_z(:)    = strm_Sa_z(:)
    Sa_tbot(:) = strm_Sa_tbot(:)
    Sa_ptem(:) = strm_Sa_ptem(:)
    Sa_shum(:) = strm_Sa_shum(:)
    Sa_dens(:) = strm_Sa_dens(:)
    Sa_pbot(:) = strm_Sa_pbot(:)
    Sa_pslv(:) = strm_Sa_pslv(:)
    Sa_u(:)    = strm_Sa_u(:)
    Sa_v(:)    = strm_Sa_v(:)

    Faxa_rainc(:) = strm_Faxa_rainc(:)
    Faxa_rainl(:) = strm_Faxa_rainl(:)
    Faxa_snowc(:) = strm_Faxa_snowc(:)
    Faxa_snowl(:) = strm_Faxa_snowl(:)
    Faxa_lwdn(:)  = strm_Faxa_lwdn (:)
    Faxa_swndr(:) = strm_Faxa_swndr(:)
    Faxa_swndf(:) = strm_Faxa_swvdr(:)
    Faxa_swvdr(:) = strm_Faxa_swndf(:)
    Faxa_swvdf(:) = strm_Faxa_swvdf(:)

  end subroutine datm_datamode_cplhist_advance

end module datm_datamode_cplhist_mod
