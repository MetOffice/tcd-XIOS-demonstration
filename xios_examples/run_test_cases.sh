#!/bin/bash
cd XIOS/generic_testcase
ln -s ../bin/generic_testcase.exe &&
ln -s ../bin/xios_server.exe &&
sed -i 's/nb_proc_atm=4/nb_proc_atm=1/g' param.def &&
mpiexec --allow-run-as-root -n 1 ./generic_testcase.exe : -n 1 ./xios_server.exe
