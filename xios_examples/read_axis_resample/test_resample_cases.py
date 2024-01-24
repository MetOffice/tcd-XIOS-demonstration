import copy
import glob
import netCDF4
import numpy as np
import os
import subprocess
import glob
import unittest

this_path = os.path.realpath(__file__)
this_dir = os.path.dirname(this_path)

class TestResample(unittest.TestCase):
    """
    UnitTest class to contain tests,
    1 test case function per input `.cdl` file

    """
    @classmethod
    def setUpClass(cls):
        """
        First, build the fortran code only once for this class.

        """
        subprocess.run(['make', 'clean'], cwd=this_dir, check=True)
        subprocess.run(['make'], cwd=this_dir, check=True)
        if os.environ.get('MVER', '') == 'XIOS3/trunk':
            with open(os.path.join(this_dir, 'iodef.xml'), 'r') as ioin:
                iodef_in = ioin.read()
            # patch in transport protocol choice for XIOS3
            # needed for CI runners
            in2 = '<variable_group id="parameters" >'
            in3 = ('<variable_group id="parameters" >\n'
                   '    <variable id="transport_protocol" '
                   'type="string" >p2p</variable>')
            iodef_out = iodef_in.replace(in2, in3)
            with open(os.path.join(this_dir, 'iodef.xml'), 'w') as ioout:
                ioout.write(iodef_out)

    def tearDown(self):
        """
        After each test function,
        report any errors from XIOS, then
        remove the input and output netCDF files.

        """

        for ef in glob.glob('{}/*.err'.format(this_dir)):
            print(ef)
            with open(ef, 'r') as efile:
                print(efile.read(), flush=True)

        os.remove('{}/axis_input.nc'.format(this_dir))
        os.remove('{}/axis_output.nc'.format(this_dir))

    @classmethod
    def tearDownClass(cls):
        """
        Finally, clean the build the fortran code only once for this class.

        """
        subprocess.run(['make', 'clean'], cwd=this_dir)

# A list of input `.cdl` files where XIOS is known to produce different
# output from the expected output data
# for future investigation / ToDo
known_failures = ['test_axis_input_edge_simple_square_ten.cdl']

# iterate through `.cdl` files in this test case folder
for f in glob.glob('{}/*.cdl'.format(this_dir)):
    def make_a_test(inf):
        """
        this function makes a test case and returns it as a function.

        """
        # always copy for value, don't pass by reference.
        infile = copy.copy(inf)
        # expected by the fortran XIOS resample program's main.xml
        outfile = 'axis_input.nc'
        def test_resample(self):
            # create a netCDF file from the `.cdl` input
            subprocess.run(['ncgen', '-k', 'nc4', '-o', 'axis_input.nc',
                            infile], cwd=this_dir, check=True)
            # run the compiled Fortran XIOS programme
            subprocess.run(['mpiexec', '-n', '1', './resample.exe', ':',
                            '-n', '1', './xios_server.exe'],
                            cwd=this_dir, check=True)
                            infile], cwd=this_dir)
            # load the result netCDF file
            rootgrp = netCDF4.Dataset('{}/axis_output.nc'.format(this_dir),
                                      'r')
            # read data from the resampled, expected & diff variables
            diff = rootgrp['resampled_minus_resample'][:]
            # prepare message for failure
            msg = ('the expected resample data array\n {exp}\n '
                   'differs from the resampled data array\n {res} \n '
                   'with diff \n {diff}\n')
            msg = msg.format(exp=rootgrp['resample_data'][:],
                             res=rootgrp['resampled_data'][:],
                             diff=diff)
            if np.any(diff):
                # print message for fail case,
                # as expected failures do not report msg.
                print(msg)
            # assert that all of the `diff` varaible values are zero
            self.assertTrue(not np.any(diff), msg=msg)
        return test_resample
    # unique name for the test
    tname = 'test_{}'.format(os.path.basename(f))
    # add the test as an attribute (function) to the test class
    if tname in known_failures:
        # set decorator @unittest.expectedFailure
        setattr(TestResample, tname, unittest.expectedFailure(make_a_test(f)))
    else:
        setattr(TestResample, tname, make_a_test(f))
