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

# COPY . /app
COPY ./requirements.txt /app/requirements.txt
# COPY ./env /app/env

WORKDIR /app

# ENV PATH="/app/env/bin:$PATH"

RUN pip3 install -r requirements.txt

COPY . /app

# ENTRYPOINT [ "/app/run_flask.sh" ]

# RUN which flask

# RUN ls /app
# RUN ls /app/env
# RUN ls /app/env/bin

# ENTRYPOINT [ "bash" ]
# CMD [ "-c", "source env/bin/activate && flask run --host 0.0.0.0" ]

# ENTRYPOINT [ "env/bin/python" ]

ENTRYPOINT [ "flask" ]
CMD [ "run", "--host", "0.0.0.0" ]

