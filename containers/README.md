# Container Recipes

Below follows instructions how to build XIOS in a container.

## Building Docker container and run test case

The instructions below assumes you have [Docker](https://docs.docker.com/engine/install/) installed on your system. Note the instructions in this document assume you are using the docker commandline tool. 

To build a docker container, run for example the following command in root directory of this repository:

```
docker build -t tcd_demo_xios_build --file containers/Dockerfile --build-arg build_arch=GCC_LINUX_AARCH64 --build-arg xios_source=http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS3/trunk --build-arg patch_file=./patches/xios3/revert_svn2517_transport.patch .
```

Note in the above the `--build-arg patch_file=` flag is optional. In the example above, the name of the container image created will be `tcd_demo_xios_build`. You can replace it with any name you wish. Finally the `--build-arg build_arch=` flag value will be dependent on the host architecture you are planning to build and run the container on. See the currently available options in the `arch` directory.  

Once the container has successfully built, you can run the example test cases (in the root of this repository):

```
docker run --mount type=bind,source=./xios_examples,target=/home/xiosuser/xios_examples tcd_demo_xios_build ./xios_examples/run_test_cases.sh
```
