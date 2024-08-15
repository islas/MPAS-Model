#include "TraceHandler.hpp"

#include <stdint.h>
#include <vector>
#include <set>
#include <string>

#include "Trace.hpp"

namespace mpas
{

bool
TraceHandler::canLog( uint8_t traceLevel, std::set< std::string > tags )
{
  bool valid = false;

  for ( 
        std::vector< mpas::Trace >::iterator it = traces_.begin();
        it != traces_.end();
        it++ )
  {
    valid = valid || it->valid( traceLevel, tags );

    if ( valid ) break;
  }

  return valid;

}

void
TraceHandler::addTraces( std::string specifications )
{
  std::vector< std::string > tokens = mpas::string::tokenize( specifications, ",", true );

  for ( std::vector< std::string >::iterator it = tokens.begin();
        it != tokens.end();
        it++ )
  {
    traces_.push_back( mpas::Trace( *it ) );
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
TraceHandler_ctor( mpas::TraceHandler **ppObj )
{
  mpas::TraceHandler *pTraceHandler = new mpas::TraceHandler();
  (*ppObj) = pTraceHandler;
}

void
TraceHandler_dtor( mpas::TraceHandler **ppObj )
{
  delete (*ppObj);
  (*ppObj) = 0;
  ppObj    = 0;
}

void
TraceHandler_addTraces(
                        mpas::TraceHandler *pObj,
                        char *specifications
                        )
{
  pObj->addTraces( std::string( specifications ) );
}

bool
TraceHandler_canLog(
                    mpas::TraceHandler *pObj,
                    int traceLevel,
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

  return pObj->canLog( static_cast< uint8_t >( traceLevel ), tagsTokenized );

}

