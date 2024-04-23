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

class TestContext(xshared._TestCase):
    test_dir = this_dir
    transient_inputs = []
    transient_outputs = ['output_stop5.nc', 'output_stop10.nc']
    rtol = 5e-03

    def setUp(self):
        for inx in ['iodef.xml']:
            with open('{}/{}'.format(self.test_dir, inx)) as cxml:
                print(inx, flush=True)
                print(cxml.read(), flush=True)


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
        # add the test as an attribute (function) to the test class
    # if os.environ.get('MVER', '') == 'XIOS3/trunk':
    #     # these tests are hitting exceptions with XIOS3
    #     # but not XIOS2, so skip for XIOS3 runner
    #     setattr(TestContext, tname,
    #             unittest.skip(TestContext.make_a_resample_test(f)))
    # elif tname in known_failures:
    if tname in known_failures:
        # set decorator @unittest.expectedFailure
        setattr(TestContext, tname,
                unittest.expectedFailure(TestContext.make_a_resample_test(f)))
    else:
        setattr(TestContext, tname,
                TestContext.make_a_resample_test(f))
