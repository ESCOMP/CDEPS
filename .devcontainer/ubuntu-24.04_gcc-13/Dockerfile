FROM ubuntu:24.04

# Define arguments for image
ARG CTNR_NAME="cdepsdev_ubuntu-24.04_gcc-13" \
    CTNR_DESCRIPTION="CDEPS development container" \
    CTNR_VERSION="1.0.0" \
    CTNR_USER="cdepsdev" \
    SPACK_VERSION="v1.2.1" \
    SPACK_REPO_VERSION="v2026.06.0"

# Set system environment variables
ENV DEBIAN_FRONTEND=noninteractive \
    LANG=en_US.UTF-8 \
    LANGUAGE=en_US:en \
    LC_ALL=en_US.UTF-8

# Install system packages via apt
RUN <<INSTALL_PACKAGES
    apt-get update
    apt-get install -yqq \
        automake \
        autotools-dev \
        build-essential \
        bzip2 \
        ca-certificates \
        cmake \
        curl \
        file \
        g++-13 \
        gcc-13 \
        gfortran-13 \
        git \
        gpg \
        gzip \
        libcurl4-gnutls-dev \
        libjpeg-dev \
        libssl-dev \
        locales \
        lsb-release \
        m4 \
        make \
        openssh-client \
        pkg-config \
        python3 \
        python3-dev \
        python3-venv \
        python3-pip \
        sudo \
        tar \
        tcl-dev \
        unzip \
        vim \
        wget
    locale-gen en_US.UTF-8
    update-locale LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8
    ln -sf gcc-13 /usr/bin/gcc
    ln -sf g++-13 /usr/bin/g++
    ln -sf gfortran-13 /usr/bin/gfortran
    ln -sf python3 /usr/bin/python
    arch=$(dpkg-architecture -qDEB_HOST_MULTIARCH)
    find "/usr/lib/${arch}" -name "libjpeg*" \
      -exec ln -sf {} /usr/lib/.  \;
    apt-get clean
INSTALL_PACKAGES

# Create non-root user with sudo privileges
ENV HOME=/home/${CTNR_USER}
RUN <<SETUP_USER
    groupadd ${CTNR_USER}
    useradd -m -d ${HOME} -s /bin/bash -g ${CTNR_USER} ${CTNR_USER}
    adduser ${CTNR_USER} sudo
    echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
SETUP_USER
USER ${CTNR_USER}
WORKDIR ${HOME}

# Install spack via git clone
ENV SPACK_ROOT=${HOME}/spack
RUN <<INSTALL_SPACK
    mkdir -p ${SPACK_ROOT}
    git clone --depth=2 --branch=${SPACK_VERSION} --no-single-branch \
      https://github.com/spack/spack.git ${SPACK_ROOT}
    . ${SPACK_ROOT}/share/spack/setup-env.sh
    spack repo update --tag ${SPACK_REPO_VERSION} builtin
INSTALL_SPACK
ENV PATH="${SPACK_ROOT}/bin:${PATH}"

# Configure external spack packages
RUN <<SPACK_PKG_EXTERNAL
    . ${SPACK_ROOT}/share/spack/setup-env.sh
    target=$(dpkg-architecture -qDEB_HOST_MULTIARCH | cut -d- -f1)
    spack compiler find
    echo "Discovering external packages, this may take a few minutes..."
    spack external find --all
    # Install essential packages
    spack install gcc || exit 1
    spack install python || exit 1
    spack install cmake || exit 1
SPACK_PKG_EXTERNAL

# Install spack packages
RUN <<SPACK_PKG_INSTALL
    . ${SPACK_ROOT}/share/spack/setup-env.sh
    spack install --fail-fast \
        openmpi@4.1.6 \
        esmf@8.9.0 +debug \
        hdf5@1.14.6 +fortran \
        netcdf-c@4.10.0 \
        netcdf-fortran@4.6.2 \
        parallelio@2.6.6 \
        parallel-netcdf@1.14.1 +fortran || exit 1
    spack clean --all
SPACK_PKG_INSTALL

# Environment setup
ENV OPENMPI_ROOT=${HOME}/stack/openmpi \
  ESMF_ROOT=${HOME}/stack/esmf \
  HDF5_ROOT=${HOME}/stack/hdf5 \
  NETCDF_ROOT=${HOME}/stack/netcdf \
  PNETCDF_ROOT=${HOME}/stack/pnetcdf \
  PIO_ROOT=${HOME}/stack/pio

RUN <<SPACK_CREATE_VIEWS
    . ${SPACK_ROOT}/share/spack/setup-env.sh
    spack view -d false symlink ${OPENMPI_ROOT} openmpi
    spack view -d false symlink ${ESMF_ROOT} esmf
    spack view -d false symlink ${HDF5_ROOT} hdf5
    spack view -d false symlink ${NETCDF_ROOT} netcdf-c netcdf-fortran
    spack view -d false symlink ${PNETCDF_ROOT} parallel-netcdf
    spack view -d false symlink ${PIO_ROOT} parallelio
SPACK_CREATE_VIEWS

# Copy user environment setup script
COPY --chown=$CTNR_USER:$CTNR_USER --chmod=0755 envsetup.sh ${HOME}/.envsetup.sh

# Source the user environment setup script
RUN <<SETUPENV
    tmp=$(mktemp)
    printf '%s\n' \
        '# Source the user environment setup script' \
        'if [ -f "${HOME}/.envsetup.sh" ]; then' \
        '    . "${HOME}/.envsetup.sh"' \
        'fi' '' > "$tmp"
    [ -f ${HOME}/.bashrc ] && cat ${HOME}/.bashrc >> "$tmp"
    mv "$tmp" ${HOME}/.bashrc
SETUPENV

# Copy README file
COPY --chown=$CTNR_USER:$CTNR_USER --chmod=0644 README.md ${HOME}/README.md

# Default command
CMD ["/bin/bash", "-l"]

# Set environment variable for devcontainer title
ENV DEVCONTAINER_NAME=${CTNR_NAME}

# Add OCI labels for image metadata
# https://specs.opencontainers.org/image-spec/annotations/
LABEL org.opencontainers.image.version="${CTNR_VERSION}" \
      org.opencontainers.image.title="${CTNR_NAME}" \
      org.opencontainers.image.description="${CTNR_DESCRIPTION}"
