import copy
import glob
import netCDF4
import numpy as np
import os
import subprocess
import unittest
from pathlib import Path

import xios_examples.gen_netcdf as gn

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
    rtol = 5e-03
    executable = './resample.exe'
    mesh_file_cdl = None

    @classmethod
    def make_netcdf(cls, inf, inputfile, nc_method='cdl_files'):
        if nc_method == 'cdl_files':
            # create a netCDF file from the `.cdl` input
            subprocess.run(['ncgen', '-k', 'nc4', '-o', inputfile,
                            inf], cwd=cls.test_dir, check=True)
        elif nc_method == 'data_func':
            mesh_file_nc = None
            if cls.mesh_file_cdl is not None:
                mesh_file_nc = Path(cls.mesh_file_cdl).with_suffix('.nc')
                # create a mesh netCDF file from the mesh `.cdl` file
                subprocess.run(['ncgen', '-k', 'nc4', '-o', mesh_file_nc,
                                cls.mesh_file_cdl], cwd=cls.test_dir, check=True)
            # create a  netCDF file from an analytic function
            cwd = Path(cls.test_dir)
            if mesh_file_nc is not None:
                mesh_file_nc = cwd/mesh_file_nc
            gn.run(cwd/inputfile, func_str=inf, mesh_file=mesh_file_nc)

    @classmethod
    def run_mpi_xios(cls, nclients=1, nservers=1):
        # run the compiled Fortran XIOS programme
        if os.environ.get('PLATFORM', '') == 'Archer2':
            run_cmd = ['srun', '--distribution=block:block', '--hint=nomultithread',
                       '--het-group=0', '--nodes=1', '-n', str(nclients),
                       cls.executable, ':',
                       '--het-group=1', '--nodes=1', '-n', str(nservers),
                       './xios_server.exe']
            print(' '.join(run_cmd))
            subprocess.run(run_cmd,cwd=cls.test_dir, check=True)
        else:
            run_cmd = ['mpiexec', '-n', str(nclients), cls.executable, ':',
                       '-n', str(nservers), './xios_server.exe']
            if os.environ.get('MPI_FLAVOUR', '') == 'openmpi':
                # use hwthread for github ubuntu runner
                # but only for known openMPI (set by env var)
                run_cmd = run_cmd[0:1] + ['--use-hwthread-cpus'] + run_cmd[1:]
            print(' '.join(run_cmd))
            subprocess.run(run_cmd, cwd=cls.test_dir, check=True)

    @classmethod
    def setUpClass(cls):
        """
        First, build the fortran code only once for this class.

        """
        subprocess.run(['make', 'clean'], cwd=cls.test_dir, check=True)
        subprocess.run(['make'], cwd=cls.test_dir, check=True)
        if os.environ.get('MVER', '').startswith('XIOS3/trunk'):
            with open(os.path.join(cls.test_dir, 'xios.xml'), 'r') as ioin:
                iodef_in = ioin.read()
            # patch in transport protocol choice for XIOS3
            # needed for CI runners
            in2 = '<variable_group id="parameters" >'
            in3 = ('<variable_group id="parameters" >\n'
                   '    <variable id="transport_protocol" '
                   'type="string" >p2p</variable>')
            iodef_out = iodef_in.replace(in2, in3)
            with open(os.path.join(cls.test_dir, 'xios.xml'), 'w') as ioout:
                ioout.write(iodef_out)

    def tearDown(self):
        """
        After each test function,
        report any errors from XIOS, then
        remove the input and output netCDF files.

        Use environment variable 'files' to avoid clean up of transient files
        note; this can cause multi-test classes to fail with ncgen errors, use
        for single test functions only.
        """

        for ef in glob.glob('{}/*.err'.format(self.test_dir)):
            print(ef)
            with open(ef, 'r') as efile:
                print(efile.read(), flush=True)

        for t_in in self.transient_inputs:
            rf = '{}/{}'.format(self.test_dir, t_in)
            if os.path.exists(rf) and not os.environ.get("files"):
                os.remove(rf)
        for t_out in self.transient_outputs:
            rf = '{}/{}'.format(self.test_dir, t_out)
            if os.path.exists(rf) and not os.environ.get("files"):
                os.remove(rf)

    @classmethod
    def tearDownClass(cls):
        """
        Finally, clean the build for this class, after all tests have run.

        Use environment variable 'logs' to avoid clean up, e.g. to keep logs
        """
        if not os.environ.get('logs'):
            subprocess.run(['make', 'clean'], cwd=cls.test_dir)
        if os.environ.get('MVER', '').startswith('XIOS3/trunk'):
            with open(os.path.join(cls.test_dir, 'xios.xml'), 'r') as ioin:
                iodef_in = ioin.read()
            # patch back out transport protocol choice for XIOS3
            # to avoid spurious git diff
            in2 = '<variable_group id="parameters" >'
            in3 = ('<variable_group id="parameters" >\n'
                   '    <variable id="transport_protocol" '
                   'type="string" >p2p</variable>')
            iodef_out = iodef_in.replace(in3, in2)
            with open(os.path.join(cls.test_dir, 'xios.xml'), 'w') as ioout:
                ioout.write(iodef_out)


    @classmethod
    def make_a_resample_test(cls, inf, nc_method='cdl_files',
                             nclients=1, nservers=1):
        """
        this function makes a test case and returns it as a test function,
        suitable to be dynamically added to a TestCase for running.

        """
        # always copy for value, don't pass by reference.
        infcp = copy.copy(inf)
        # expected by the fortran XIOS resample program's main.xml
        inputfile = cls.transient_inputs[0]
        outputfile = cls.transient_outputs[0]
        def test_resample(self):
            # create a netCDF file using nc_method
            cls.make_netcdf(infcp, inputfile, nc_method=nc_method)
            cls.run_mpi_xios(nclients=nclients, nservers=nservers)

            # load the result netCDF file
            runfile = '{}/{}'.format(cls.test_dir, outputfile)
            assert(os.path.exists(runfile))
            rootgrp = netCDF4.Dataset(runfile, 'r')
            # read data from the resampled, expected & diff variables
            diff = rootgrp['resampled_minus_resample'][:]
            expected = rootgrp['resample_data'][:]
            result = rootgrp['resampled_data'][:]
            # prepare message for failure
            msg = ('the expected resample data array\n {exp}\n '
                   'differs from the resampled data array\n {res} \n '
                   'with diff \n {diff}\n')
            msg = msg.format(exp=expected, res=result, diff=diff)
            if not np.allclose(result, expected, rtol=cls.rtol):
                # print message for fail case,
                # as expected failures do not report msg.
                print(msg)
            # assert that all of the `diff` varaible values are zero
            # self.assertTrue(not np.any(diff), msg=msg)
            self.assertTrue(np.allclose(result, expected, rtol=cls.rtol),
                            msg=msg)
        return test_resample
