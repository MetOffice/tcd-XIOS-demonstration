# Pull base image
FROM ubuntu:22.04

# Set work directory
WORKDIR /code

# Copy project
COPY . .

# Install dependencies
RUN apt  update
RUN apt -yq install subversion
RUN apt install -y build-essential
RUN perl -MCPAN -e 'install "URI"'
RUN for dep in $(cat dependencies); do apt --yes install $dep; done

# Build XIOS
RUN svn co http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/trunk@2252 XIOS
RUN cp arch/* XIOS/arch/
RUN cd XIOS && ./make_xios --job 2 --arch GCC_LINUX_AARCH64 --debug
