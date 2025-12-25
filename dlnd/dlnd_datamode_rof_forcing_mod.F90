module dlnd_datamode_rof_forcing_mod

   use ESMF                    , only : ESMF_SUCCESS, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_State
   use ESMF                    , only : ESMF_TraceRegionExit, ESMF_TraceRegionEnter
   use NUOPC                   , only : NUOPC_Advertise
   use shr_kind_mod            , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
   use shr_string_mod          , only : shr_string_listGetNum, shr_string_listGetName
   use dshr_methods_mod        , only : dshr_state_getfldptr, chkerr
   use dshr_strdata_mod        , only : shr_strdata_type, shr_strdata_get_stream_pointer
   use dshr_fldlist_mod        , only : fldlist_type, dshr_fldlist_add
   use shr_lnd2rof_tracers_mod , only : shr_lnd2rof_tracers_readnl

   implicit none
   private ! except

   public :: dlnd_datamode_rof_forcing_advertise
   public :: dlnd_datamode_rof_forcing_init_pointers
   public :: dlnd_datamode_rof_forcing_advance

   ! export state pointers
   real(r8), pointer :: lfrac(:)
   real(r8), pointer :: Flrl_rofsur_nonh2o_2d(:,:)
   real(r8), pointer :: Flrl_rofsur_nonh2o_1d(:)
   real(r8), pointer :: Flrl_rofsur(:)
   real(r8), pointer :: Flrl_rofsub(:)
   real(r8), pointer :: Flrl_rofgwl(:)
   real(r8), pointer :: Flrl_rofi(:)
   real(r8), pointer :: Flrl_irrig(:)

   ! stream field pointers
   type, public :: stream_pointer_type
      real(r8), pointer :: strm_ptr(:)
   end type stream_pointer_type
   type(stream_pointer_type), allocatable :: strm_Flrl_rofsur_nonh2o_2d(:) ! 2dple nonh2o tracers
   real(r8), pointer :: strm_Flrl_rofsur_nonh2o_1d(:) ! onlyl 1 nonh2o tracer
   real(r8), pointer :: strm_Flrl_rofsur(:)
   real(r8), pointer :: strm_Flrl_rofsub(:)
   real(r8), pointer :: strm_Flrl_rofgwl(:)
   real(r8), pointer :: strm_Flrl_rofi(:)
   real(r8), pointer :: strm_Flrl_irrig(:)

   integer :: ntracers_nonh2o

   character(*), parameter :: nullstr = 'null'
   character(*), parameter :: u_FILE_u = &
        __FILE__

!===============================================================================
contains
!===============================================================================

   subroutine dlnd_datamode_rof_forcing_advertise(exportState, fldsExport, flds_scalar_name, logunit, mainproc, rc)

      ! determine export fields to advertise to mediator

      ! input/output arguments
      type(ESMF_State)                 :: exportState
      type(fldList_type) , pointer     :: fldsExport
      character(len=*)   , intent(in)  :: flds_scalar_name
      integer            , intent(in)  :: logunit
      logical            , intent(in)  :: mainproc
      integer            , intent(out) :: rc

      ! local variables
      character(len=CS) :: lnd2rof_tracers
      type(fldlist_type), pointer :: fldList
      !-------------------------------------------------------------------------------

      rc = ESMF_SUCCESS

      ! lnd2rof liquid tracers (liquid tracers OTHER than water)
      ! coupling the land input of tracers other than standard water to MOSART
      if (mainproc) then
         write(logunit,'(a)') 'reading in non-water tracers from land (if any) in drv_flds_in '
      end if
      lnd2rof_tracers = ' '
      call shr_lnd2rof_tracers_readnl('drv_flds_in', lnd2rof_tracers)
      if (lnd2rof_tracers /= ' ') then
         ntracers_nonh2o = shr_string_listGetNum(lnd2rof_tracers)
      else
         ntracers_nonh2o = 0
      end if

      !-------------------
      ! Advertise export fields to rof
      !-------------------

      call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
      call dshr_fldlist_add(fldsExport, "Sl_lfrin")

      ! The following puts all non-water tracers as an undidstributed dimension in the export state field
      if (ntracers_nonh2o > 1) then
         call dshr_fldList_add(fldsExport, 'Flrl_rofsur_nonh2o', ungridded_lbound=1, ungridded_ubound=ntracers_nonh2o)
      else if (ntracers_nonh2o == 1) then
         call dshr_fldList_add(fldsExport, 'Flrl_rofsur_nonh2o')
      end if
      call dshr_fldlist_add(FldsExport, 'Flrl_rofsur')
      call dshr_fldlist_add(FldsExport, 'Flrl_rofsub')
      call dshr_fldlist_add(FldsExport, 'Flrl_rofgwl')
      call dshr_fldlist_add(FldsExport, 'Flrl_rofi'  )
      call dshr_fldlist_add(FldsExport, 'Flrl_irrig' )

      fldlist => fldsExport ! the head of the linked list
      do while (associated(fldlist))
         call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
         if (ChkErr(rc,__LINE__,u_FILE_u)) return
         call ESMF_LogWrite('(dlnd_comp_advertise): Fr_lnd '//trim(fldList%stdname), ESMF_LOGMSG_INFO)
         if (mainproc) then
            write(logunit,'(a)') '(dlnd_comp_advertise): Fr_lnd '//trim(fldList%stdname)
         end if
         fldList => fldList%next
      enddo

   end subroutine dlnd_datamode_rof_forcing_advertise

   !===============================================================================

   subroutine dlnd_datamode_rof_forcing_init_pointers(exportState, sdat, model_frac, rc)

      ! input/output variables
      type(ESMF_State)      , intent(inout) :: exportState
      type(shr_strdata_type), intent(in)    :: sdat
      real(r8)              , intent(in)    :: model_frac(:)
      integer               , intent(out)   :: rc

      ! local variables
      integer          :: nf
      character(len=2) :: nchar
      character(CS)    :: strm_fld
      character(len=*), parameter :: subname='(dlnd_datamode_rof_forcing_init_pointers): '
      !-------------------------------------------------------------------------------

      rc = ESMF_SUCCESS

      ! Set pointers to export state
      call dshr_state_getfldptr(exportState, fldname='Sl_lfrin', fldptr1=lfrac, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      lfrac(:) = model_frac(:) ! Set fractional land pointer in export state
      if (ntracers_nonh2o > 1) then
         call dshr_state_getfldptr(exportState, fldname='Flrl_rofsur_nonh2o', fldptr2=Flrl_rofsur_nonh2o_2d, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
      else
         call dshr_state_getfldptr(exportState, fldname='Flrl_rofsur_nonh2o', fldptr1=Flrl_rofsur_nonh2o_1d, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
      end if
      call dshr_state_getfldptr(exportState, fldname='Flrl_rofsur', fldptr1=Flrl_rofsur, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      call dshr_state_getfldptr(exportState, fldname='Flrl_rofsub', fldptr1=Flrl_rofsub, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      call dshr_state_getfldptr(exportState, fldname='Flrl_rofgwl', fldptr1=Flrl_rofgwl, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      call dshr_state_getfldptr(exportState, fldname='Flrl_rofi', fldptr1=Flrl_rofi, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      call dshr_state_getfldptr(exportState, fldname='Flrl_irrig', fldptr1=Flrl_irrig, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return

      ! Set pointers to required stream fields

      if (ntracers_nonh2o > 1) then
         allocate(strm_Flrl_rofsur_nonh2o_2d(ntracers_nonh2o))
         do nf = 1,ntracers_nonh2o
            write(nchar,'(i2.2)') nf
            strm_fld = trim('Flrl_rofsur_nonh2o') // trim(nchar)
            call shr_strdata_get_stream_pointer( sdat, trim(strm_fld), strm_Flrl_rofsur_nonh2o_2d(nf)%strm_ptr, &
                 requirePointer=.true., &
                 errmsg=trim(subname)//'ERROR: '//trim(strm_fld)//&
                 ' must be associated for dlnd rof_forcing datamode', rc=rc)
            if (ChkErr(rc,__LINE__,u_FILE_u)) return
         end do
      else if (ntracers_nonh2o == 1) then
         call shr_strdata_get_stream_pointer(sdat, 'Flrl_rofsur_nonh2o' , strm_Flrl_rofsur_nonh2o_1d, &
              requirePointer=.true., &
              errmsg=trim(subname)//'ERROR: strm_Flrl_rofsur_1d '// &
              ' must be associated for dlnd rof_forcing mode', rc=rc)
         if (ChkErr(rc,__LINE__,u_FILE_u)) return
      end if

      call shr_strdata_get_stream_pointer(sdat, 'Flrl_rofsur' , strm_Flrl_rofsur, requirePointer=.true., &
           errmsg=trim(subname)//'ERROR: strm_Flrl_rofsur be associated for dlnd rof_forcing mode', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call shr_strdata_get_stream_pointer(sdat, 'Flrl_rofsub' , strm_Flrl_rofsub, requirePointer=.true., &
           errmsg=trim(subname)//'ERROR: strm_Flrl_rofsub be associated for dlnd rof_forcing mode', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call shr_strdata_get_stream_pointer(sdat, 'Flrl_rofgwl' , strm_Flrl_rofgwl, requirePointer=.true., &
           errmsg=trim(subname)//'ERROR: strm_Flrl_rofgwl be associated for dlnd rof_forcing mode', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call shr_strdata_get_stream_pointer(sdat, 'Flrl_rofi' , strm_Flrl_rofi, requirePointer=.true., &
           errmsg=trim(subname)//'ERROR: strm_Flrl_rofi be associated for dlnd rof_forcing mode', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      ! optional stream field pointer
      call shr_strdata_get_stream_pointer(sdat, 'Flrl_irrig' , strm_Flrl_irrig, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

   end subroutine dlnd_datamode_rof_forcing_init_pointers

   !===============================================================================
   subroutine dlnd_datamode_rof_forcing_advance()

      ! local variables
      integer :: ni,nf
      character(len=*), parameter :: subname='(dlnd_datamode_rof_forcing_advance): '
      !-------------------------------------------------------------------------------

      if (ntracers_nonh2o > 1) then
         ! Note that the inner dimension is the field index
         do nf = 1,ntracers_nonh2o
            do ni = 1,size(Flrl_rofsur_nonh2o_2d,dim=2)
               if (lfrac(ni) == 0._r8) then
                  Flrl_rofsur_nonh2o_2d(nf,ni) = 1.e30_r8
               else
                  Flrl_rofsur_nonh2o_2d(nf,ni) = strm_Flrl_rofsur_nonh2o_2d(nf)%strm_ptr(ni)
               end if
            end do
         end do
      else if (ntracers_nonh2o == 1) then
         do ni = 1,size(Flrl_rofsur_nonh2o_1d)
            if (lfrac(ni) == 0._r8) then
               Flrl_rofsur_nonh2o_1d(ni) = 1.e30_r8
            else
               Flrl_rofsur_nonh2o_1d(ni) = strm_Flrl_rofsur_nonh2o_1d(ni)
            end if
         end do
      end if

      do ni = 1,size(Flrl_rofsur)
         if (lfrac(ni) == 0._r8) then
            Flrl_rofsur(ni) = 1.e30_r8
            Flrl_rofsub(ni) = 1.e30_r8
            Flrl_rofgwl(ni) = 1.e30_r8
            Flrl_rofi(ni)   = 1.e30_r8
         else
            Flrl_rofsur(ni) = strm_Flrl_rofsur(ni)
            Flrl_rofsub(ni) = strm_Flrl_rofsub(ni)
            Flrl_rofgwl(ni) = strm_Flrl_rofgwl(ni)
            Flrl_rofi(ni)   = strm_Flrl_rofi(ni)
         end if
      end do

      if (associated(strm_Flrl_irrig)) then
         do ni = 1,size(Flrl_rofsur)
            if (lfrac(ni) == 0._r8) then
               Flrl_irrig(ni)  = 1.e30_r8
            else
               Flrl_irrig(ni)  = strm_Flrl_irrig(ni)
            end if
         end do
      else
         Flrl_irrig(:) = 0._r8
      end if

   end subroutine dlnd_datamode_rof_forcing_advance

end module dlnd_datamode_rof_forcing_mod
