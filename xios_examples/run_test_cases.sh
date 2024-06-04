#!/bin/bash

#
# Run generic XIOS test case.
#
echo
echo Running generic XIOS test case
echo ------------------------------
echo
cd XIOS/generic_testcase
ln -s ../bin/generic_testcase.exe &&
ln -s ../bin/xios_server.exe &&
cp param.def param_backup.def &&
sed -i 's/nb_proc_atm=4/nb_proc_atm=1/g' param.def &&
mpiexec --allow-run-as-root -n 1 ./generic_testcase.exe : -n 1 ./xios_server.exe
echo
echo Cleaning up generic XIOS test case run
echo --------------------------------------
echo
rm generic_testcase.exe xios_server.exe
mv param_backup.def param.def
cd ../../

#
# Run XIOS demo examples.
#
echo
echo Running XIOS demo examples
echo --------------------------
echo
. arch/arch-GCC_LINUX_AARCH64.env
. arch/arch-GCC_LINUX_AARCH64.path
export XIOS_BINDIR=$PWD/XIOS/bin
export XIOS_INCDIR=$PWD/XIOS/inc
export XIOS_LIBDIR=$PWD/XIOS/lib
export MVER=XIOS/trunk@2252
export MPI_FLAVOUR='openmpi'
export OMPI_ALLOW_RUN_AS_ROOT=1
export OMPI_ALLOW_RUN_AS_ROOT_CONFIRM=1
python3 -m unittest discover -v -s xios_examples
echo
echo Cleaning up XIOS demo example runs
echo ----------------------------------
echo
