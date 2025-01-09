#include "DebugSpecHandler.hpp"

#include <stdint.h>
#include <vector>
#include <set>
#include <string>

#include "DebugSpec.hpp"

namespace mpas
{

bool
DebugSpecHandler::canLog( uint8_t debugLevel, std::set< std::string > tags )
{
  bool valid = false;

  for ( 
        std::vector< mpas::DebugSpec >::iterator it = debugSpecs_.begin();
        it != debugSpecs_.end();
        it++ )
  {
    valid = valid || it->valid( debugLevel, tags );

    if ( valid ) break;
  }

  return valid;

}

void
DebugSpecHandler::addDebugSpecs( std::string specifiers )
{
  std::vector< std::string > tokens = mpas::string::tokenize( specifiers, ",", true );

  for ( std::vector< std::string >::iterator it = tokens.begin();
        it != tokens.end();
        it++ )
  {
    debugSpecs_.push_back( mpas::DebugSpec( *it ) );
  }
}


}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// C bindings below
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

void
DebugSpecHandler_ctor( mpas::DebugSpecHandler **ppObj )
{
  mpas::DebugSpecHandler *pDebugSpecHandler = new mpas::DebugSpecHandler();
  (*ppObj) = pDebugSpecHandler;
}

void
DebugSpecHandler_dtor( mpas::DebugSpecHandler **ppObj )
{
  delete (*ppObj);
  (*ppObj) = 0;
  ppObj    = 0;
}

void
DebugSpecHandler_addDebugSpecs(
                                mpas::DebugSpecHandler *pObj,
                                char *specifiers
                                )
{
  pObj->addDebugSpecs( std::string( specifiers ) );
}

bool
DebugSpecHandler_canLog(
                        mpas::DebugSpecHandler *pObj,
                        int debugLevel,
                        char *tags,
                        char *file
                        )
{
  std::set< std::string > tagsTokenized;
  std::vector< std::string > tokens = mpas::string::tokenize( std::string( tags ), ",", true );

  for ( std::vector< std::string >::iterator it = tokens.begin();
        it != tokens.end();
        it++ )
  {
    if ( !( *it ).empty() )
    {
      tagsTokenized.insert( *it );
    }
  }

  tagsTokenized.insert( std::string( file ) );

  return pObj->canLog( static_cast< uint8_t >( debugLevel ), tagsTokenized );

}

