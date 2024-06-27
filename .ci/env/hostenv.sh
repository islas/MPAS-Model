#!/bin/sh

# Better than uname and what we use in the HPC Workflows
hostname=$( python3 -c "import socket; print( socket.getfqdn() )" )
if [ $( contains ${hostname} hsn.de.hpc ) -eq 0 ]; then
  # Derecho HPC SuSE PBS
  . .ci/env/derecho.sh
else
  echo "No known environment for '${hostname}', using current"
fi