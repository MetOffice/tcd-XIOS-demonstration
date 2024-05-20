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
