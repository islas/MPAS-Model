#ifndef __mpas_DebugSpecHandler_hpp__
#define __mpas_DebugSpecHandler_hpp__
#include <stdint.h>
#include <vector>
#include <set>
#include <string>

#include "DebugSpec.hpp"

namespace mpas
{

class DebugSpecHandler
{
public :
  DebugSpecHandler()  {}
  ~DebugSpecHandler() { debugSpecs_.clear(); }

  bool
  canLog( uint8_t debugLevel, std::set< std::string > tags );

  void
  addDebugSpecs( std::string specifiers );

private :

  std::vector< mpas::DebugSpec > debugSpecs_; ///< our traces determining what can be logged


};

}


extern "C"
{

void DebugSpecHandler_ctor( mpas::DebugSpecHandler **ppObj );
void DebugSpecHandler_dtor( mpas::DebugSpecHandler **ppObj );

void DebugSpecHandler_addDebugSpecs( mpas::DebugSpecHandler *pObj, char *specifiers );
bool DebugSpecHandler_canLog    ( mpas::DebugSpecHandler *pObj, int debugLevel, char *tags, char *file );

}

#endif // __mpas_DebugSpecHandler_hpp__
