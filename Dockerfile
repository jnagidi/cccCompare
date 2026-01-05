FROM rocker/r-ver:4.4.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libicu-dev \
    libwebp-dev \
    libwebp7 \
    libwebpmux3 \
    make \
    g++ \
    gcc \
    libgomp1 \
    libtool \
    libc-dev \
    && rm -rf /var/lib/apt/lists/*

ENV RENV_CONFIG_CACHE_ENABLED="TRUE"

ARG PAT_GITHUB
ENV GITHUB_PAT=${PAT_GITHUB}

ARG UDS4_API
ENV UDS4_API=$UDS4_API

WORKDIR /home/app

COPY . /home/app

RUN R -e "source('./renv/activate.R'); renv::restore(confirm = TRUE)"

RUN chown -R root:root /home/app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/app', host='0.0.0.0', port=3838)"]
