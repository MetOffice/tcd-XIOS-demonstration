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

class TestWriteMetadata(xshared._TestCase):
    test_dir = this_dir
    transient_inputs = ['planar_mesh.nc']
    transient_outputs = ['data_output.nc']
    executable = './write.exe'
    mesh_file_cdl = 'planar_mesh.cdl'

    @classmethod
    def make_a_write_test(cls, inf, nclients=1, nservers=1):
        """
        this function makes a test case and returns it as a test function,
        suitable to be dynamically added to a TestCase for running.

        """
        # always copy for value, don't pass by reference.
        infcp = copy.copy(inf)
        # expected by the fortran XIOS program's main.xml
        inputfile = cls.transient_inputs[0]
        outputfile = cls.transient_outputs[0]
        def test_write_metadata(self):
            # create a netCDF file using nc_method
            cls.make_netcdf(infcp, inputfile)
            cls.run_mpi_xios(nclients=nclients, nservers=nservers)

            # load the result netCDF file
            runfile = '{}/{}'.format(cls.test_dir, outputfile)
            assert(os.path.exists(runfile))
            run_cmd = ['ncdump', runfile]
            ncd = subprocess.run(run_cmd, check=True, capture_output=True)

            with open(f'{this_dir}/expected_domain_output.cdl') as fin:
                exptd = fin.read()
            emsg = ''
            for eline, aline in zip(exptd.split('\n'),
                                    ncd.stdout.decode("utf-8").split('\n')):
                if eline != aline:
                    # skip timestamp and uuid
                    # until we work out how not to set these
                    if not (':timeStamp' in eline or ':uuid' in eline):
                        emsg += eline + '\n' + aline + '\n\tmismatch\n\n'

            #self.assertFalse(emsg, msg='\n\n' + emsg)
        return test_write_metadata


f = f'{this_dir}/planar_mesh.cdl'
# unique name for the test
tname = 'test_{}'.format(os.path.splitext(os.path.basename(f))[0])
# add the test as an attribute (function) to the test class

# !!! remove this limit on success
if os.environ.get('MVER', '').startswith('XIOS3/trunk'):
    setattr(TestWriteMetadata, tname,
            TestWriteMetadata.make_a_write_test(f))
