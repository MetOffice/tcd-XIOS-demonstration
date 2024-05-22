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
    transient_inputs = ['domain_input.nc', 'domain_input_ugrid.nc']
    transient_outputs = ['domain_output.nc', 'domain_output_ugrid.nc']
    rtol = 0.7
    mesh_file_cdl = 'mesh_C12.cdl'


# A list of input function names where XIOS is known to produce different
# output from the expected output data
# for future investigation / ToDo
# this is a dict, where the name of the key is the name of the test
# to register as a known_failure (tname)
# and the value is a string explaining the failure
# this handles FAIL conditions but NOT ERROR conditions
known_failures = {}

# iterate over analytic function names
for f in ['sinusiod', 'harmonic', 'vortex', 'gulfstream']:
    # unique name for the test
    tname = f'test_{f}'
    # add the test as an attribute (function) to the test class
    if os.environ.get('MVER', '').startswith('XIOS3/trunk'):
        # these tests are hitting exceptions with XIOS3
        # but not XIOS2, so skip for XIOS3 runner
        setattr(TestResampleDomain, tname,
                unittest.skip(TestResampleDomain.make_a_resample_test(f)))
    elif tname in known_failures:
        # set decorator @unittest.expectedFailure
        setattr(TestResampleDomain, tname,
                unittest.expectedFailure(TestResampleDomain.make_a_resample_test(f, nc_method='data_func', nclients=3)))
    else:
        setattr(TestResampleDomain, tname,
                TestResampleDomain.make_a_resample_test(f, nc_method='data_func', nclients=3))
