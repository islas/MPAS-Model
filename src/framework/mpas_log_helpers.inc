#define MPAS_DEBUG_WRITE     mpas_debug_write

#ifndef SUPPRESS_DEBUG_MSG
#define mpas_debug_write     mpas_log_write
#else
#define mpas_debug_write     mpas_log_write_null
#endif