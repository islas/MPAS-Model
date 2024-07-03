#ifndef __mpas_string_hpp__
#define __mpas_string_hpp__

#include <string>
#include <vector>

namespace mpas
{
namespace string
{
std::vector<std::string>
tokenize( std::string input, std::string delim, bool trimTokens = false );

std::string
trim( std::string input );

int32_t
numeric( std::string input );

}
}

#endif // __mpas_string_hpp__

