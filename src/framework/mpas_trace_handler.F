module mpas_trace_handler
  use, intrinsic :: iso_c_binding
  implicit none

  interface
    subroutine TraceHandler_ctor              ( pTraceHandler )              &
      bind( c, name="TraceHandler_ctor"               )
      ! get iso_c_binding types
      import
      implicit none
      ! return TraceHandler *
      type( c_ptr ), intent( inout ) :: pTraceHandler
    end subroutine TraceHandler_ctor

    subroutine TraceHandler_dtor              ( pTraceHandler )              &
      bind( c, name="TraceHandler_dtor"               )
      ! get iso_c_binding types
      import
      implicit none
      type( c_ptr ), intent( inout ) :: pTraceHandler
      ! return void
    end subroutine TraceHandler_dtor

    subroutine TraceHandler_addTraces        ( pTraceHandler, specifications ) &
      bind( c, name="TraceHandler_addTraces"         )
      ! get iso_c_binding types
      import
      implicit none
      type( c_ptr ), value :: pTraceHandler
      character( kind = c_char ), dimension(*), intent( in ) :: specifications
      ! return void
    end subroutine TraceHandler_addTraces

    function TraceHandler_canLog          ( pTraceHandler, traceLevel, tags, file )        &
      bind( c, name="TraceHandler_canLog"           ) result( canLog )
      ! get iso_c_binding types
      import
      implicit none
      type( c_ptr ), value :: pTraceHandler
      integer(   kind=c_int32_t ), intent( in ), value :: traceLevel
      character( kind = c_char  ), dimension(*), intent( in ) :: tags
      character( kind = c_char  ), dimension(*), intent( in ) :: file
      logical(   kind=c_bool    ) :: canLog
      ! return bool
    end function TraceHandler_canLog

  end interface
end module mpas_trace_handler
