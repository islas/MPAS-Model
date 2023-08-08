import os
import numpy as  np
import runtime_data
import static_data

filename = "demo.py"
logstr   = "{file}:{func} : "
def initialize( ) :
  print( logstr.format( file=filename, func=initialize.__name__ ) )

def finalize( ) :
  print( logstr.format( file=filename, func=finalize.__name__ ) )

def main( ) :
  print( logstr.format( file=filename, func=main.__name__ ) )
  print( "Hello Fortran!" )

  aaa = runtime_data.aaa()

  print( type( aaa ) )
  print( aaa.flags )

  aaa[:] = np.random.normal( loc=0.0, scale=1.0, size=aaa.size )[:]
