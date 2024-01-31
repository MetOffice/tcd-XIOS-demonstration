import copy
import glob
import netCDF4
import numpy as np
import os
import subprocess
import unittest

this_path = os.path.realpath(__file__)
this_dir = os.path.dirname(this_path)

class _TestCase(unittest.TestCase):
    """
    UnitTest class to contain tests,
    1 test case function per input `.cdl` file

    """
    test_dir = this_dir
    transient_inputs = []
    transient_outputs = []

    @classmethod
    def setUpClass(cls):
        """
        First, build the fortran code only once for this class.

        """
        subprocess.run(['make', 'clean'], cwd=cls.test_dir)
        subprocess.run(['make'], cwd=cls.test_dir)
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

        for t_in in self.transient_inputs:
            rf = '{}/{}'.format(self.test_dir, t_in)
            if os.path.exists(rf):
                os.remove(rf)
        for t_out in self.transient_outputs:
            rf = '{}/{}'.format(self.test_dir, t_out)
            if os.path.exists(rf):
                os.remove(rf)

    @classmethod
    def tearDownClass(cls):
        """
        Finally, clean the build the fortran code only once for this class.

        """
        subprocess.run(['make', 'clean'], cwd=cls.test_dir)

