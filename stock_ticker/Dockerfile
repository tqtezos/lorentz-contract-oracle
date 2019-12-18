FROM ubuntu:18.04

MAINTAINER Michael J. Klein "michael@tqgroup.io"

RUN apt-get update -y && \
    apt-get install -y python3-pip python3-dev \
    libsodium-dev libsecp256k1-dev libgmp-dev \
    locales pkg-config

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

COPY ./requirements.txt /app/requirements.txt
COPY ./env /app/env

WORKDIR /app

RUN pip3 install -r requirements.txt
RUN . ./env/bin/activate

COPY . /app

ENTRYPOINT [ "flask" ]
CMD [ "run", "--host", "0.0.0.0" ]
