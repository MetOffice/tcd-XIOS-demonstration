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
    transient_outputs = ['output_stop10.nc','output_stop5.nc']
    rtol = 5e-03

    def test_context_stop(self):
        '''
        Test the two output files produced by stopping the context after
        5 and 10 timesteps
        '''
        # run the compiled XIOS program
        with open('{}/iodef.xml'.format(self.test_dir)) as cxml:
            print(cxml.read(), flush=True)
        subprocess.run(['mpiexec', '-n', '1', './context_def_test.exe', ':',
                        '-n', '1', 'xios_server.exe'],
                        cwd=self.test_dir, check=True)
        cdl_files = ['output_stop5.cdl', 'output_stop10.cdl']
        output_files = [f.replace('cdl', 'nc') for f in cdl_files]

        for cdl_file, outputfile in zip(cdl_files, output_files):
            reference_file = 'reference_{}'.format(outputfile)
            runfile = '{}/{}'.format(self.test_dir, outputfile)
            assert(os.path.exists(runfile))
            subprocess.run(['ncgen', '-k', 'nc4', '-o', reference_file,
                            cdl_file], cwd=self.test_dir, check=True)
            reference_file = '{}/{}'.format(self.test_dir, reference_file)
            test_results = netCDF4.Dataset(runfile, 'r')['field_A'][:]
            expected = netCDF4.Dataset(reference_file, 'r')['field_A'][:]
            diff = test_results - expected
            msg = ('The produced context data array in file {} '
                   'differs from that in the reference cdl file {}\n'.
                   format(outputfile, cdl_file))
            if not np.allclose(test_results, expected, rtol=self.rtol):
                print(msg)
            self.assertTrue(np.allclose(
                test_results, expected, rtol=self.rtol), msg=msg)
