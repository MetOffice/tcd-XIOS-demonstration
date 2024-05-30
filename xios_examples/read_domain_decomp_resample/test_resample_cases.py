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

    @classmethod
    def setUpClass(cls):
        """
        First, patch the code for XIOS2 compliance, if used. Then,
        use the parent class to build the fortran code only once for this class.

        """
        if not os.environ.get('MVER', '').startswith('XIOS3/trunk'):
            patchsource = ""
            with open(f'{cls.test_dir}/resample.F90') as insource:
                lines = insource.readlines()
                for line in lines:
                    pline = line
                    if line == """    call xios_get_domain_attr("odatax::", &\n""":
                        pline = """    call xios_get_domain_attr("original_domain_read", &\n"""
                    elif line == """    call xios_get_domain_attr("edatax::", &\n""":
                        pline = """    call xios_get_domain_attr("resampled_domain_read", &\n"""
                    elif line == """    call xios_get_domain_attr("odata::", &\n""":
                        pline = """    call xios_get_domain_attr("original_domain", &\n"""
                    elif line == """    call xios_get_domain_attr("rdata::", &\n""":
                        pline = """    call xios_get_domain_attr("resample_domain", &\n"""
                    patchsource += pline
                cls.unpatched_source = lines
            with open(f'{this_dir}/resample.F90', 'w') as outsource:
                outsource.write(patchsource)
        super().setUpClass()

    @classmethod
    def tearDownClass(cls):
        """
        Finally, revert the patch, then,
        use super to clean the build for this class, after all tests have run.

        Use environment variable 'logs' to avoid clean up, e.g. to keep logs
        """
        if not os.environ.get('MVER', '').startswith('XIOS3/trunk'):
            with open(f'{cls.test_dir}/resample.F90', 'w') as revertsource:
                revertsource.write(''.join(cls.unpatched_source))
        super().tearDownClass()


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
    if tname in known_failures:
        # set decorator @unittest.expectedFailure
        setattr(TestResampleDomain, tname,
                unittest.expectedFailure(TestResampleDomain.make_a_resample_test(f, nclients=3, ncdump=True)))
    else:
        setattr(TestResampleDomain, tname,
                TestResampleDomain.make_a_resample_test(f, nclients=3, ncdump=True))
