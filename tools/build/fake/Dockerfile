FROM rhub/r-minimal

# webfakes + some optional dependencies + packages used in run.R
RUN installr -d -t "linux-headers curl-dev" \
    callr cli desc jsonlite pkgcache processx webfakes withr zip

# for the git app
RUN apk add --no-cache git git-daemon

COPY R /R
WORKDIR /R

CMD ["R", "-s", "-f", "/R/run.R"]

# CRAN
EXPOSE 3100

# BIOC
EXPOSE 3101

# PPM
EXPOSE 3102

# Name check app
EXPOSE 3103

# git
EXPOSE 3104

# GH
EXPOSE 3105

# CRAN2
EXPOSE 3106
