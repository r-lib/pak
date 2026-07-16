FROM r-base:4.4.1

RUN apt-get update && apt-get install -y \
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    git \
    dos2unix \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /pak
COPY . .

# Fix line endings on all files
RUN find . -type f \( -name "configure*" -o -name "Makevars*" -o -name "*.sh" \) -exec dos2unix {} \; 2>/dev/null; true

# Install pak dependencies first
RUN R --quiet --slave -e "install.packages(c('callr', 'cli', 'curl', 'desc', 'jsonlite', 'pkgbuild', 'pkgsearch', 'processx', 'ps', 'yaml'), repos='https://cloud.r-project.org', Ncpus=2)"

# Install pak - allow warnings, don't fail on embedded library compilation
RUN R CMD INSTALL . 2>&1 | tail -20; true

# Test that pak can be loaded
RUN R --quiet --slave -e "library('pak'); cat('pak loaded successfully\n')"

CMD ["R"]
