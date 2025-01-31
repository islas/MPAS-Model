#include "DebugSpecHandler.hpp"

#include <stdint.h>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

#include "DebugSpec.hpp"

namespace mpas
{

bool
DebugSpecHandler::canLog( uint8_t debugLevel, std::set< std::string > tags )
{
  size_t hash  = std::hash< std::string >{}( mpas::string::join( tags, "," ) );

  if (
      lookup_.find( debugLevel )          == lookup_.end() ||
      lookup_[ debugLevel ].find( hash ) == lookup_[ debugLevel ].end()
    )
  {
    // for this debug level this hash does not exist so it has to be evaluated
    bool   valid = false;
    for ( 
          std::vector< mpas::DebugSpec >::iterator it = debugSpecs_.begin();
          it != debugSpecs_.end();
          it++ )
    {
      valid = valid || it->valid( debugLevel, tags );

      if ( valid ) break;
    }

    // add it to our lookup table
    lookup_[ debugLevel ][ hash ] = valid;
  }
  return lookup_[ debugLevel ][ hash ];
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

