
FROM alpine:3.19

RUN apk add alpine-sdk

RUN addgroup gaborcsardi && \
    adduser -D gaborcsardi -G gaborcsardi && \
    addgroup gaborcsardi abuild

RUN apk add git bash

COPY dot-abuild/gaborcsardi-673e2275.rsa.pub /etc/apk/keys/gaborcsardi-673e2275.rsa.pub
COPY dot-abuild /home/gaborcsardi/.abuild
COPY aports /home/gaborcsardi/aports
COPY build.sh /home/gaborcsardi/build.sh
RUN chown -R gaborcsardi:gaborcsardi /home/gaborcsardi

USER gaborcsardi
WORKDIR /home/gaborcsardi
