#ifndef __mpas_TraceHandler_hpp__
#define __mpas_TraceHandler_hpp__
#include <stdint.h>
#include <vector>
#include <set>
#include <string>

#include "Trace.hpp"

namespace mpas
{

class TraceHandler
{
public :
  TraceHandler()  {}
  ~TraceHandler() { traces_.clear(); }

  bool
  canLog( uint8_t traceLevel, std::set< std::string > tags );

  void
  addTraces( std::string specifications );

private :

  std::vector< mpas::Trace > traces_; ///< our traces determining what can be logged


};

}


extern "C"
{

void TraceHandler_ctor( mpas::TraceHandler **ppObj );
void TraceHandler_dtor( mpas::TraceHandler **ppObj );

void TraceHandler_addTraces( mpas::TraceHandler *pObj, char *specifications );
bool TraceHandler_canLog    ( mpas::TraceHandler *pObj, int traceLevel, char *tags );

}

#endif // __mpas_TraceHandler_hpp__
