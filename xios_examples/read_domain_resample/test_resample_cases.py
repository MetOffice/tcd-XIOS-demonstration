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
    transient_inputs = ['domain_input.nc']
    transient_outputs = ['domain_output.nc']
    rtol = 5e-03


# A list of input `.cdl` files where XIOS is known to produce different
# output from the expected output data
# for future investigation / ToDo
# this is a dict, where the name of the key is the name of the test
# to register as a known_failure (tname)
# and the value is a string explaining the failure
# this handles FAIL conditions but NOT ERROR conditions
known_failures = {'test_domain_input_edge_simple_square_ten':
                  ('The bi-linear polynomial poorly reproduces the'
                   ' input x^2+y^2 function'),
                  'test_domain_input_simple_square_ten':
                  ('The bi-linear polynomial poorly reproduces the'
                   ' input x^2+y^2 function')
                  }

# iterate through `.cdl` files in this test case folder
for f in glob.glob('{}/*.cdl'.format(this_dir)):
    # unique name for the test
    tname = 'test_{}'.format(os.path.splitext(os.path.basename(f))[0])
    # add the test as an attribute (function) to the test class
    if os.environ.get('MVER', '') == 'XIOS3/trunk':
        # these tests are hitting exceptions with XIOS3
        # but not XIOS2, so skip for XIOS3 runner
        setattr(TestResampleDomain, tname,
                unittest.skip(TestResampleDomain.make_a_resample_test(f)))
    elif tname in known_failures:
        # set decorator @unittest.expectedFailure
        setattr(TestResampleDomain, tname,
                unittest.expectedFailure(TestResampleDomain.make_a_resample_test(f)))
    else:
        setattr(TestResampleDomain, tname,
                TestResampleDomain.make_a_resample_test(f))
