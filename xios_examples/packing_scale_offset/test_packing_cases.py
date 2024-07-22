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

class TestPackDomain(xshared._TestCase):
    test_dir = this_dir
    transient_inputs = ['domain_input.nc']
    transient_outputs = ['domain_output.nc']
    executable = './pack.exe'

    @classmethod
    def make_a_pack_test(cls, inf):
        """
        this function makes a test case and returns it as a test function,
        suitable to be dynamically added to a TestCase for running.

        """
        # always copy for value, don't pass by reference.
        infile = copy.copy(inf)
        # expected by the fortran XIOS pack program's main.xml
        inputfile = cls.transient_inputs[0]
        outputfile = cls.transient_outputs[0]
        def test_pack(self):
            # create a netCDF file from the `.cdl` input
            subprocess.run(['ncgen', '-k', 'nc4', '-o', inputfile,
                            infile], cwd=cls.test_dir, check=True)
            cls.run_mpi_xios()

            # load the result netCDF file
            runfile = '{}/{}'.format(cls.test_dir, outputfile)
            assert(os.path.exists(runfile))
            rootgrp = netCDF4.Dataset(runfile, 'r')
            # read data from the packed, expected & diff variables
            expected = rootgrp['original_data'][:]
            result = rootgrp['packed_data'][:]
            rtol = rootgrp['packed_data'].scale_factor
            # prepare message for failure
            msg = ('\n the packed data array\n {res}\n '
                   'differs from the original data array\n {exp} \n '
                   'with diff outside expected tolerance {rtol}\n {diff}\n')
            msg = msg.format(exp=expected, res=result, rtol=rtol,
                             diff=result-expected)
            if not np.allclose(result, expected, rtol=rtol):
                # print message for fail case,
                # as expected failures do not report msg.
                print(msg)
            # assert that all of the `diff` varaible values are zero
            # self.assertTrue(not np.any(diff), msg=msg)
            self.assertTrue(np.allclose(result, expected, rtol=rtol) and
                            not np.allclose(result, expected, rtol=0.1*rtol),
                            msg=msg)
        return test_pack


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
        setattr(TestPackDomain, tname,
                unittest.expectedFailure(TestPackDomain.make_a_pack_test(f)))
    else:
        setattr(TestPackDomain, tname,
                TestPackDomain.make_a_pack_test(f))
