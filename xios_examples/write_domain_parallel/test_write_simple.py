import os
import subprocess
import unittest

import netCDF4
import numpy as np

import xios_examples.shared_testing as xshared

this_path = os.path.realpath(__file__)
this_dir = os.path.dirname(this_path)


class TestParallelWrite(xshared._TestCase):
    test_dir = this_dir
    transient_inputs = []
    transient_outputs = ["domain_output_1.nc"]
    rtol = 5e-04
    executable = './write_parallel.exe'

    def test_parallel_write(self):
        # run the compiled Fortran XIOS programme
        with open('{}/xios.xml'.format(self.test_dir)) as cxml:
            print(cxml.read(), flush=True)
        self.run_mpi_xios(nclients=2, nservers=2)
        outputfile_1 = self.transient_outputs[0]

        # Check the expected output file exists
        runfile_1 = '{}/{}'.format(self.test_dir, outputfile_1)
        self.assertTrue(os.path.exists(runfile_1))

        # Checks for output file

        rootgrp = netCDF4.Dataset(runfile_1, 'r')
        file_1_data = rootgrp['global_field_1']

        # Check file has 10 times
        self.assertTrue(file_1_data.shape[0] == 10)
        # Check average value of file for level 1, time 1
        expected = 2.8
        result = np.average(file_1_data[-1,0,:])
        diff = result - expected
        # prepare message for failure
        msg = (self.transient_outputs[0] + ': the expected result\n {exp}\n '
               'differs from the actual result\n {res} \n '
               'with diff \n {diff}\n')
        msg = msg.format(exp=expected, res=result, diff=diff)
        self.assertTrue(np.allclose(result, expected, rtol=self.rtol), msg=msg)

        
        


