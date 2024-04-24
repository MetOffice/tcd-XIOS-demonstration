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
    transient_outputs = ['output_stop10.nc']
    rtol = 5e-03

    def test_contexts(self):
        # run the compiled XIOS program
        with open('{}/iodef.xml'.format(self.test_dir)) as cxml:
            print(cxml.read(), flush=True)
        subprocess.run(['mpiexec', '-n', '1', './context_def_test.exe', ':',
                        '-n', '1', 'xios_server.exe'],
                        cwd=self.test_dir, check=True)
        cdl_file = 'output_stop10.cdl'
        reference_file = 'reference_{}'.format(self.transient_outputs[0])
        outputfile = self.transient_outputs[0]
        runfile = '{}/{}'.format(self.test_dir, outputfile)
        assert(os.path.exists(runfile))

        #recreate our reference file from the cdl file
        subprocess.run(['ncgen', '-k', 'nc4', '-o', reference_file,
                        cdl_file], cwd=self.test_dir, check=True)

        reference_file = '{}/{}'.format(self.test_dir, reference_file)
        test_results = netCDF4.Dataset(runfile, 'r')['field_A'][:]
        expected = netCDF4.Dataset(reference_file, 'r')['field_A'][:]
        diff = test_results - expected
        msg = ('the expected context data array differs from the results\n '
               'of the test\n')
        if not np.allclose(test_results, expected, rtol=self.rtol):
            print(msg)
        self.assertTrue(np.allclose(test_results, expected, rtol=self.rtol),
                        msg=msg)
