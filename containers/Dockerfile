#Pull base image
FROM ubuntu:22.04

# Set up some environment variables to be used in XIOS build step
ARG build_arch
ARG xios_source
ENV arch $build_arch
ENV xios $xios_source

ENV user xiosuser 
ENV homedir /home/$user/

# Create a non-root user
RUN groupadd -g 999 "${user}" && \
    useradd -m -r -u 999 -g "${user}" "${user}"

# Set work directory
WORKDIR "${homedir}"

# Copy project relevant files
COPY --chown="${user}":999 arch arch
COPY --chown="${user}":999 dependencies dependencies

# Copy XIOS source code
COPY --chown="${user}":999 "${xios}" XIOS

# Install dependencies (as root)
RUN apt update
RUN apt -yq install subversion
RUN apt install -y build-essential
RUN perl -MCPAN -e 'install "URI"'
RUN for dep in $(cat dependencies); do apt --yes install $dep; done

# Set default user to non-root user
USER "${user}"

# Build XIOS
RUN cp arch/* XIOS/arch/ && \
    cd XIOS && \
    ./make_xios --job 4 --arch "${arch}" --debug
