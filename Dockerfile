FROM rocker/shiny:4.4.1

# -----------------------------
# Install system dependencies
# -----------------------------
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

# -----------------------------
# Enable renv cache
# -----------------------------
ENV RENV_CONFIG_CACHE_ENABLED="TRUE"

# -----------------------------
# GitHub PAT for private packages
# -----------------------------
ARG GITHUB_PAT
ENV GITHUB_PAT=${GITHUB_PAT}

# Optional: force git-based installs (avoids GitHub tarball API issues)
ENV RENV_GITHUB_SOURCE=git


ARG UDS4_API
ENV UDS4_API=${UDS4_API}

# -----------------------------
# App setup
# -----------------------------
WORKDIR /home/app
COPY . /home/app

# -----------------------------
# Install renv and restore dependencies
# -----------------------------
RUN R -e "install.packages('renv', repos='https://packagemanager.posit.co/cran/latest')"
RUN R -e "renv::restore(confirm = FALSE)"

# -----------------------------
# Fix permissions
# -----------------------------
RUN chown -R root:root /home/app

# -----------------------------
# Shiny port and start
# -----------------------------
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/home/app', host='0.0.0.0', port=3838)"]
