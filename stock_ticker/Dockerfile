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

RUN pip3 install --user pipenv
COPY ./requirements.txt ./requirements.txt
RUN pip3 install -r requirements.txt
RUN pip3 install 'flask==1.1.2'
RUN pip3 install 'alpha-vantage==2.1.3'
RUN pip3 install 'apscheduler==3.6.3'

COPY . /

WORKDIR /

ENTRYPOINT [ "flask" ]
CMD [ "run", "--host", "0.0.0.0" ]
