module docn_import_atmdata_mod

   use ESMF             , only : ESMF_State, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
   use NUOPC            , only : NUOPC_Advertise
   use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
   use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
   use dshr_methods_mod , only : chkerr

   implicit none
   private ! except

   public :: docn_import_atmdata_advertise

   character(*) , parameter :: u_FILE_u = &
        __FILE__

!===============================================================================
contains
!===============================================================================

   subroutine docn_import_atmdata_advertise(importState, fldsimport, flds_scalar_name, rc)

      ! input/output variables
      type(esmf_State)   , intent(inout) :: importState
      type(fldlist_type) , pointer       :: fldsimport
      character(len=*)   , intent(in)    :: flds_scalar_name
      integer            , intent(out)   :: rc

      ! local variables
      type(fldlist_type), pointer :: fldList
      !-------------------------------------------------------------------------------

      ! Advertise import fields from DATM if appropriate
      call dshr_fldList_add(fldsImport, trim(flds_scalar_name))
      call dshr_fldList_add(fldsImport, 'Sa_z'       )
      call dshr_fldList_add(fldsImport, 'Sa_u'       )
      call dshr_fldList_add(fldsImport, 'Sa_v'       )
      call dshr_fldList_add(fldsImport, 'Sa_ptem'    )
      call dshr_fldList_add(fldsImport, 'Sa_dens'    )
      call dshr_fldList_add(fldsImport, 'Sa_pslv'    )
      call dshr_fldList_add(fldsImport, 'Sa_tbot'    )
      call dshr_fldList_add(fldsImport, 'Sa_pbot'    )
      call dshr_fldList_add(fldsImport, 'Sa_shum'    )
      call dshr_fldList_add(fldsImport, 'Faxa_rainc' )
      call dshr_fldList_add(fldsImport, 'Faxa_rainl' )
      call dshr_fldList_add(fldsImport, 'Faxa_snowc' )
      call dshr_fldList_add(fldsImport, 'Faxa_snowl' )
      call dshr_fldList_add(fldsImport, 'Faxa_swndr' )
      call dshr_fldList_add(fldsImport, 'Faxa_swvdr' )
      call dshr_fldList_add(fldsImport, 'Faxa_swndf' )
      call dshr_fldList_add(fldsImport, 'Faxa_swvdf' )
      call dshr_fldList_add(fldsImport, 'Faxa_swnet' )
      call dshr_fldList_add(fldsImport, 'Faxa_lwdn'  )
      call dshr_fldList_add(fldsImport, 'Faxa_swdn'  )

      call dshr_fldList_add(fldsImport, 'Sa_co2prog')
      call dshr_fldList_add(fldsImport, 'Sa_co2diag')

      call dshr_fldList_add(fldsImport, 'Faxa_bcph'   , ungridded_lbound=1, ungridded_ubound=3)
      call dshr_fldList_add(fldsImport, 'Faxa_ocph'   , ungridded_lbound=1, ungridded_ubound=3)
      call dshr_fldList_add(fldsImport, 'Faxa_dstwet' , ungridded_lbound=1, ungridded_ubound=4)
      call dshr_fldList_add(fldsImport, 'Faxa_dstdry' , ungridded_lbound=1, ungridded_ubound=4)

      call dshr_fldList_add(fldsImport, 'Faxa_ndep', ungridded_lbound=1, ungridded_ubound=2)

      fldlist => fldsImport ! the head of the linked list
      do while (associated(fldlist))
         call NUOPC_Advertise(importState, standardName=fldlist%stdname, rc=rc)
         if (ChkErr(rc,__LINE__,u_FILE_u)) return
         call ESMF_LogWrite('(docn_comp_advertise): To_ocn'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
         fldList => fldList%next
      enddo

   end subroutine docn_import_atmdata_advertise

end module docn_import_atmdata_mod
