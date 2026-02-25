FROM gitlab-registry.cern.ch/cce/docker_build/vivado:2019.2

RUN yum update -y && \
    yum install wget which zlib-devel perl-devel gettext -y && \
    wget https://github.com/git/git/archive/v2.7.2.tar.gz -O /tmp/git.tar.gz && \
    cd /tmp && tar -zxf git.tar.gz && \
    cd /tmp/git-2.7.2 && \
    make configure && \
    ./configure && \
    make install && \
    yum clean all && \
    rm -rf /tmp/git*

# How to use this image:
# 1. Build the image with `docker build -t vivado-git:2019.2 .`
# 2. Ensure your gitlab-runner `config.toml` is able to use this image for CI jobs:
#    - Set `image: vivado-git:2019.2` for the relevant jobs.
#    - Set `pull_policy = "if-not-present"` to use local image if avaiable