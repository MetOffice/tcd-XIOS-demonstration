# Pull base image
FROM ubuntu:22.04

# Set work directory
WORKDIR /code

# Set up some environment variables to be used in XIOS build step
ARG build_arch
ARG xios_source
ARG patch_file
ENV arch $build_arch
ENV xios $xios_source
ENV patch $patch_file
ENV workdir /code

# Set work directory
WORKDIR "${workdir}"

# Copy project relevant files
COPY arch arch
COPY dependencies dependencies
COPY patches patches
COPY xios_examples xios_examples

# Install dependencies
RUN apt update
RUN apt -yq install subversion
RUN apt install -y build-essential
RUN perl -MCPAN -e 'install "URI"'
RUN for dep in $(cat dependencies); do apt --yes install $dep; done

# Build XIOS
RUN svn co "${xios}" XIOS && \
    cp arch/* XIOS/arch/ && \
    cd XIOS && \
    if [ ! -z "${patch}" ]; then patch -p0 < "${workdir}"/"${patch}" ; fi && \
    ./make_xios --job 4 --arch "${arch}" --debug
