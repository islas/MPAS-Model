!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  mpas_log
!
!> \MPAS message logging manager
!> \author Anthony Islas
!> \date   Oct 22 2026
!> \details
!>  This module contains routines for providing logging extensions that require
!>  bootstrapping initialization before the logging module.
!
!-----------------------------------------------------------------------


module mpas_log_ext

   use, intrinsic :: iso_c_binding

   implicit none
   private


   !--------------------------------------------------------------------
   ! Public member functions
   !--------------------------------------------------------------------
   public :: mpas_log_ext_init

!***********************************************************************


   contains


!***********************************************************************
!
!  routine mpas_log_init
!
!> \brief   Initializes logging extensions
!> \author  Anthony Islas
!> \date    12 August 2024
!> \details
!>  This routine initializes logging extensions that require secondary 
!>  information from namelist input. To allow namelist input to have logging
!>  capabilities before this is initialized, this is separated out from the 
!>  mpas_log module. This should instead be called when namelist input is ready.
!
!-----------------------------------------------------------------------
   subroutine mpas_log_ext_init( domain, err)
      use mpas_c_interfacing, only : mpas_f_to_c_string_ret
      use mpas_derived_types, only : mpas_pool_type
      use mpas_pool_routines
      use mpas_log
      use mpas_debug_spec_handler


      implicit none

      !-----------------------------------------------------------------
      ! input variables
      !-----------------------------------------------------------------
      type (domain_type), pointer, intent(in) :: domain !< Input: domain information

      !-----------------------------------------------------------------
      ! output variables
      !-----------------------------------------------------------------
      integer, intent(out), optional :: err !< Output - Optional: error flag

      !-----------------------------------------------------------------
      ! local variables
      !-----------------------------------------------------------------
      type (mpas_pool_type), pointer    :: development
      character( len=StrKind ), pointer :: debug_specs
      integer :: err_tmp

      err_tmp = 0

      ! Initialize the debug handler object
      ! call mpas_pool_get_subpool( domain % blocklist % structs, "development", development )
      call mpas_pool_get_config ( domain % configs, "debug_specs", debug_specs )

      ! Set info from nml input
      call DebugSpecHandler_addDebugSpecs( mpas_log_info % debugSpecHandlerPtr, mpas_f_to_c_string_ret( debug_specs ) )

      if (present(err)) then
         err = err_tmp 
      endif
   !--------------------------------------------------------------------
   end subroutine mpas_log_ext_init

end module mpas_log_ext
