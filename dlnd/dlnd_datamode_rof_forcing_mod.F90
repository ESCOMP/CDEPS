module dlnd_datamode_rof_forcing_mod

   use ESMF                    , only : ESMF_SUCCESS, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_State
   use ESMF                    , only : ESMF_StateItem_Flag
   use NUOPC                   , only : NUOPC_Advertise
   use shr_kind_mod            , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
   use shr_string_mod          , only : shr_string_listGetNum, shr_string_listGetName
   use dshr_methods_mod        , only : dshr_state_getfldptr, chkerr
   use dshr_strdata_mod        , only : shr_strdata_type
   use dshr_fldlist_mod        , only : fldlist_type, dshr_fldlist_add
   use dshr_dfield_mod         , only : dfield_type, dshr_dfield_add
   use shr_lnd2rof_tracers_mod , only : shr_lnd2rof_tracers_readnl

   implicit none
   private ! except

   public :: dlnd_datamode_rof_forcing_advertise
   public :: dlnd_datamode_rof_forcing_init_pointers
   public :: dlnd_datamode_rof_forcing_advance

   ! module pointer arrays
   real(r8), pointer :: lfrac(:)

   character(*), parameter :: Flrl_rofsur_nonh2o = 'Flrl_rofsur_nonh2o'
   character(*), parameter :: Flrl_rofsur        = 'Flrl_rofsur'
   character(*), parameter :: Flrl_rofsub        = 'Flrl_rofsub'
   character(*), parameter :: Flrl_rofgwl        = 'Flrl_rofgwl'
   character(*), parameter :: Flrl_rofi          = 'Flrl_rofi'
   character(*), parameter :: Flrl_irrig         = 'Flrl_irrig'

   character(len=11) :: fldnames_h2o(5) = &
        (/'Flrl_rofsur', 'Flrl_rofsub','Flrl_rofgwl','Flrl_rofi  ','Flrl_irrig '/)

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
         call dshr_fldList_add(fldsExport, Flrl_rofsur_nonh2o, ungridded_lbound=1, ungridded_ubound=ntracers_nonh2o)
      else if (ntracers_nonh2o == 1) then
         call dshr_fldList_add(fldsExport, Flrl_rofsur_nonh2o)
      end if
      call dshr_fldlist_add(FldsExport, Flrl_rofsur)
      call dshr_fldlist_add(FldsExport, Flrl_rofgwl)
      call dshr_fldlist_add(FldsExport, Flrl_rofsub)
      call dshr_fldlist_add(FldsExport, Flrl_rofi  )
      call dshr_fldlist_add(FldsExport, Flrl_irrig )

      fldlist => fldsExport ! the head of the linked list
      do while (associated(fldlist))
         call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
         if (ChkErr(rc,__LINE__,u_FILE_u)) return
         call ESMF_LogWrite('(dlnd_comp_advertise): Fr_lnd '//trim(fldList%stdname), ESMF_LOGMSG_INFO)
         fldList => fldList%next
      enddo

   end subroutine dlnd_datamode_rof_forcing_advertise

   !===============================================================================

   subroutine dlnd_datamode_rof_forcing_init_pointers(exportState, sdat, dfields, model_frac, logunit, mainproc, rc)

      ! input/output variables
      type(ESMF_State)      , intent(inout) :: exportState
      type(shr_strdata_type), intent(in)    :: sdat
      type(dfield_type)     , pointer       :: dfields
      real(r8)              , intent(in)    :: model_frac(:)
      integer               , intent(in)    :: logunit
      logical               , intent(in)    :: mainproc
      integer               , intent(out)   :: rc

      ! local variables
      integer                     :: n
      character(len=2)            :: nchar
      character(CS), allocatable  :: strm_flds(:)
      character(CS)               :: fieldname
      character(len=*), parameter :: subname='(dlnd_datamode_rof_forcing_init_pointers): '
      !-------------------------------------------------------------------------------

      rc = ESMF_SUCCESS

      ! Set fractional land pointer in export state
      call dshr_state_getfldptr(exportState, fldname='Sl_lfrin', fldptr1=lfrac, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      lfrac(:) = model_frac(:)

      ! Create stream-> export state mapping for the case where the
      ! stream field is 1d but the export state field is 2d
      ! Note that strm_flds is the model name for the stream field (1d)
      ! Note that state_fld is the model name for the export field (2d)
      if (ntracers_nonh2o > 0) then
         allocate(strm_flds(ntracers_nonh2o))
         do n = 1,ntracers_nonh2o
            write(nchar,'(i2.2)') n
            strm_flds(n) = 'Flrl_rofsur_nonh2o' // trim(nchar)
         end do
         call dshr_dfield_add(dfields, sdat, state_fld=Flrl_rofsur_nonh2o, strm_flds=strm_flds, state=exportState, &
              logunit=logunit, mainproc=mainproc, rc=rc)
         if (ChkErr(rc,__LINE__,u_FILE_u)) return
      end if

      ! Initialize dfields data type (to map streams to export state fields)
      ! Create dfields linked list - used for copying stream fields to export state fields
      do n = 1,size(fldnames_h2o)
         fieldname = trim(fldnames_h2o(n))
         call dshr_dfield_add( dfields, sdat, trim(fieldname), trim(fieldname), exportState, logunit, mainproc, rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
      end do

   end subroutine dlnd_datamode_rof_forcing_init_pointers

   !===============================================================================
   subroutine dlnd_datamode_rof_forcing_advance(exportState, rc)

      ! input/output variables
      type(ESMF_State)      , intent(inout) :: exportState
      integer               , intent(out)   :: rc

      ! local variables
      integer           :: n,nfld
      real(r8), pointer :: fldptr1(:)
      real(r8), pointer :: fldptr2(:,:)
      character(CS)     :: fieldname
      character(len=*), parameter :: subname='(dlnd_datamode_rof_forcing_advance): '
      !-------------------------------------------------------------------------------

      rc = ESMF_SUCCESS

      if (ntracers_nonh2o > 0) then
         ! Set special value over masked points
         call dshr_state_getfldptr(exportState, Flrl_rofsur_nonh2o, fldptr2=fldptr2, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
         do n = 1,size(fldptr2,dim=2)
            if (lfrac(n) == 0._r8) fldptr2(:,n) = 1.e30_r8
         end do
      end if

      do nfld = 1,size(fldnames_h2o)
         fieldname = trim(fldnames_h2o(nfld))
         call dshr_state_getfldptr(exportState, fieldname, fldptr1=fldptr1, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
         do n = 1,size(fldptr1)
            if (lfrac(n) == 0._r8) fldptr1(n) = 1.e30_r8
         end do
      end do

   end subroutine dlnd_datamode_rof_forcing_advance

end module dlnd_datamode_rof_forcing_mod
