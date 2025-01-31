!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  mpas_log
!
!> \MPAS message logging manager
!> \author Matt Hoffman
!> \date   14 February 2017
!> \details
!>  This module contains the routines for managing the writing of messages
!>  to log files.
!>  The log module operates around a variable named mpas_log_info that contains
!>  all of the external information needed to perform logging operations
!>  for the current model configuration.
!>  mpas_log_info is a module level pointer to the active core's mpas_log_type
!>  instance.  This allows the core's domain instance to "own" the log manager,
!>  while the log module has internal access to it.  This approach has two
!>  benefits:
!>  1. Calls to mpas_write_log do not require any configuration metadata
!>  (e.g., a unit number) to be passed in, simplifying the call API.
!>  2. Because the log module uses a pointer to a mpas_log_type instance,
!>  different instances can be swapped in and out from higher levels of
!>  the modeling framework during execution.  This is necessary to allow
!>  multiple different MPAS cores to operate in the same climate model,
!>  but would also apply to multiple MPAS cores running in a single MPAS
!>  executable (if this is ever supported), or multiple domain instances
!>  of the same MPAS core running together.  (It is the responsibility of
!>  the higher-level driver code(s) to manage any swapping of mpas_log_type
!>  instances.)
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
