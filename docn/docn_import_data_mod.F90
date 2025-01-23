module docn_import_data_mod

   use ESMF             , only : ESMF_State, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
   use NUOPC            , only : NUOPC_Advertise
   use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
   use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
   use dshr_methods_mod , only : chkerr

   implicit none
   private ! except

   public :: docn_import_data_advertise

   private :: docn_get_import_fields

   character(*) , parameter :: u_FILE_u = &
        __FILE__

   !===============================================================================
contains
   !===============================================================================

   subroutine docn_import_data_advertise(importState, fldsimport, flds_scalar_name, import_data_fields, rc)

      ! input/output variables
      type(esmf_State)   , intent(inout) :: importState
      type(fldlist_type) , pointer       :: fldsimport
      character(len=*)   , intent(in)    :: flds_scalar_name
      character(len=*)   , intent(in)    :: import_data_fields
      integer            , intent(out)   :: rc

      ! local variables
      type(fldlist_type), pointer :: fldList
      character(CL), allocatable  :: fieldnamelist(:)
      integer                     :: n
      !-------------------------------------------------------------------------------

      ! Translate the colon deliminted string (import_data_fields) into a character array (fieldnamelist)
      ! Note that the following call allocates the memory for fieldnamelist
      call docn_get_import_fields(import_data_fields, fieldnamelist, rc)

      ! Advertise import fields from datm, dice and med_aofluxes if appropriate
      call dshr_fldList_add(fldsImport, trim(flds_scalar_name))
      do n = 1,size(fieldnamelist)
         if (trim(fieldnamelist(n)) == 'Faxa_bcph' .or. trim(fieldnamelist(n)) == 'Faxa_ocph') then
            call dshr_fldList_add(fldsImport, trim(fieldnamelist(n)), ungridded_lbound=1, ungridded_ubound=3)
         else if (trim(fieldnamelist(n)) == 'Faxa_dstwet' .or. trim(fieldnamelist(n)) == 'Faxa_dstdry') then
            call dshr_fldList_add(fldsImport, trim(fieldnamelist(n)), ungridded_lbound=1, ungridded_ubound=4)
         else if (trim(fieldnamelist(n)) == 'Faxa_ndep') then
            call dshr_fldList_add(fldsImport, trim(fieldnamelist(n)), ungridded_lbound=1, ungridded_ubound=2)
         else
            call dshr_fldList_add(fldsImport, trim(fieldnamelist(n)))
         end if
      end do

      ! Deallocate memory from fieldnamelist
      deallocate(fieldnamelist) ! this was allocated in docn_get_import_fields

      fldlist => fldsImport ! the head of the linked list
      do while (associated(fldlist))
         call NUOPC_Advertise(importState, standardName=fldlist%stdname, rc=rc)
         if (ChkErr(rc,__LINE__,u_FILE_u)) return
         call ESMF_LogWrite('(docn_comp_advertise): To_ocn'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
         fldList => fldList%next
      enddo

   end subroutine docn_import_data_advertise


   !===============================================================================
   subroutine docn_get_import_fields(str, flds, rc)

      ! input/output variables
      character(len=*)               , intent(in)  :: str     ! colon deliminted string to search
      character(len=*) , allocatable , intent(out) :: flds(:) ! memory will be allocate for flds
      integer                        , intent(out) :: rc
      ! local variables
      integer          :: i,k,n ! generic indecies
      integer          :: nflds ! allocatable size of flds
      integer          :: count ! counts occurances of char
      integer          :: i0,i1 ! name = list(i0:i1)
      integer          :: nChar ! temporary
      logical          :: valid ! check if str is valid
      !---------------------------------------

      rc = ESMF_SUCCESS

      ! check that this is a str is a valid colon dlimited list
      valid = .true.
      nChar = len_trim(str)
      if (nChar < 1) then                     ! list is an empty string
         valid = .false.
      else if (str(1:1) == ':') then          ! first char is delimiter
         valid = .false.
      else if (str(nChar:nChar) == ':') then  ! last  char is delimiter
         valid = .false.
      else if (index(trim(str)," ") > 0) then ! white-space in a field name
         valid = .false.
      end if
      if (.not. valid) then
         call shr_sys_abort("ERROR: invalid list = "//trim(str))
      end if
      ! get number of fields in a colon delimited string list
      nflds = 0
      if (len_trim(str) > 0) then
         count = 0
         do n = 1, len_trim(str)
            if (str(n:n) == ':') count = count + 1
         end do
         nflds = count + 1
      endif
      ! allocate memory for flds)
      allocate(flds(nflds))
      do k = 1,nflds
         ! start with whole list
         i0 = 1
         i1 = len_trim(str)
         ! remove field names before kth field
         do n = 2,k
            i = index(str(i0:i1),':')
            i0 = i0 + i
         end do
         ! remove field names after kth field
         if (k < nFlds) then
            i = index(str(i0:i1),':')
            i1 = i0 + i - 2
         end if
         ! set flds(k)
         flds(k) = str(i0:i1)//"   "
      end do

   end subroutine docn_get_import_fields

end module docn_import_data_mod
