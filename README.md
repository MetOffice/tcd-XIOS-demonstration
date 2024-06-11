# tcd-XIOS-demonstration

Demonstration code for XML I/O server XIOS usage.

Demonstrations of using XIOS are provided with Continuous Integration testing with respect to XIOS2 trunk.

<em>Note</em>: This repo is a fork of the original UK MetOffice repo.

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
docker build -t tcd_demo_xios_build --build-arg build_arch=GCC_LINUX_AARCH64 --build-arg xios_source=http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/trunk@2252 .
```

Once the container has successfully built, you can run the example test cases:

```
docker run tcd_demo_xios_build ./xios_examples/run_test_cases.sh
```
