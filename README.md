# tcd-XIOS-demonstration

Demonstration code for XML I/O server XIOS usage.

Demonstrations of using XIOS are provided with Continuous Integration testing with respect to XIOS2 trunk.

## Environments

Environments are managed, with a little complication, to enable running on scientific desktop and on Github  Continuous Integration.

There is a helper script that one can source, in the root directory, to ensure that the scientific desktop LFRic environment is loaded, using the environment variable setup for the Makefiles, which is also compatible with the Github CI runner.

`. desktopEnv`

## Running demonstrations as test cases

The code in this repository is organised with test runners, which enable some or all of the cases to be prepared and run.

To run all cases:
(where `$REPO_ROOT` is the root directory of this repository)

```
cd $REPO_ROOT
python -m unittest discover -v -s xios_examples
```

Individual tests, or subsets of tests, may be run by explicitly targeting tests using python imports, e.g.

```
python -m unittest xios_examples.read_axis_resample.test_resample_cases
```

## Building Docker container and run test case

The instructions below assumes you have [Docker](https://docs.docker.com/engine/install/) installed on your system. Note the instructions in this document assume you are using the docker commandline tool. 

To build a docker container, run for example the following command in the root directory of this repository:

```
docker build -t tcd_demo_xios_build --build-arg build_arch=GCC_LINUX_AARCH64 --build-arg xios_source=http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS3/trunk --build-arg patch_file=./patches/xios3/revert_svn2517_transport.patch .
```

Note in the above the `--build-arg patch_file=` flag is optional. In the example above, the name of the container image created will be `tcd_demo_xios_build`. You can replace it with any name you wish. Finally the `--build-arg build_arch=` flag value will be dependent on the host architecture you are planning to build and run the container on. See the currently available options in the `arch` directory.  

Once the container has successfully built, you can run the example test cases (in the root of this repository):

```
docker run --mount type=bind,source=./xios_examples,target=/code/xios_examples tcd_demo_xios_build ./xios_examples/run_test_cases.sh
```
