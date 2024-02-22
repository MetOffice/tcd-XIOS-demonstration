import copy
import glob
import netCDF4
import numpy as np
import os
import subprocess
import unittest

import xios_examples.shared_testing as xshared

this_path = os.path.realpath(__file__)
this_dir = os.path.dirname(this_path)

class TestResampleDomain(xshared._TestCase):
    test_dir = this_dir
    transient_inputs = ['stratify_input_in_domain_1.nc']
    transient_outputs = ['pressure_stratify.nc']
    rtol = 5e-03

    # def test_running(self):
    #     subprocess.run(['ncgen', '-k', 'nc4', '-o',
    #                     self.transient_inputs[0],
    #                     'stratify_input_in_domain_1.cdl'],
    #                     cwd=self.test_dir, check=True)
    #     # run the compiled Fortran XIOS programme
    #     subprocess.run(['mpiexec', '-n', '1', './resample.exe', ':',
    #                     '-n', '1', './xios_server.exe'],
    #                     cwd=self.test_dir, check=True)


# A list of input `.cdl` files where XIOS is known to produce different
# output from the expected output data
# for future investigation / ToDo
# this is a dict, where the name of the key is the name of the test
# to register as a known_failure (tname)
# and the value is a string explaining the failure
# this handles FAIL conditions but NOT ERROR conditions
known_failures = {}

# iterate through `.cdl` files in this test case folder
for f in glob.glob('{}/*.cdl'.format(this_dir)):
    # unique name for the test
    tname = 'test_{}'.format(os.path.splitext(os.path.basename(f))[0])
    # add the test as an attribute (function) to the test class
    if tname in known_failures:
        # set decorator @unittest.expectedFailure
        setattr(TestResampleDomain, tname,
                unittest.expectedFailure(TestResampleDomain.make_a_resample_test(f)))
    else:
        setattr(TestResampleDomain, tname,
                TestResampleDomain.make_a_resample_test(f))
