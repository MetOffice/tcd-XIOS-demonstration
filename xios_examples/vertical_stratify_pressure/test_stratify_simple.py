import copy
import glob
import netCDF4
import numpy as np
import os
import subprocess
import glob
import unittest

import xios_examples.shared_testing as xshared

this_path = os.path.realpath(__file__)
this_dir = os.path.dirname(this_path)


class TestResampleAxis(xshared._TestCase):
    test_dir = this_dir
    transient_inputs = []
    transient_outputs = ["axis_output.nc"]
    rtol = 5e-04

    def test_pressure_stratification(self):
        # run the compiled Fortran XIOS programme
        with open('{}/xios.xml'.format(self.test_dir)) as cxml:
            print(cxml.read(), flush=True)
        self.run_mpi_xios()
        outputfile = self.transient_outputs[0]
        runfile = '{}/{}'.format(self.test_dir, outputfile)
        assert(os.path.exists(runfile))
        rootgrp = netCDF4.Dataset(runfile, 'r')

        expected = np.array([[[273.70905]],
                            [[228.19833]],
                            [[221.43733]],
                            [[223.00148]],
                            [[233.3793 ]]])

        result = rootgrp['temponP'][:]
        diff = result - expected
        # prepare message for failure
        msg = ('the expected resample data array\n {exp}\n '
               'differs from the resampled data array\n {res} \n '
               'with diff \n {diff}\n')
        msg = msg.format(exp=expected, res=result, diff=diff)
        if not np.allclose(result, expected, rtol=self.rtol):
            # print message for fail case,
            # as expected failures do not report msg.
            print(msg)
        self.assertTrue(np.allclose(result, expected, rtol=self.rtol), msg=msg)

