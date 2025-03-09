FROM r-base:4.4.2

# Install system dependencies
RUN apt-get update && apt-get install -y \
  libcurl4-openssl-dev \
  cmake \ 
  libsodium-dev \
  libssl-dev \
  && rm -rf /var/lib/apt/lists/*

# Install R packages
# RUN R -e "install.packages(c('plumber', 'MASS', 'caret', 'mgcv', 'arm', 'jsonlite', 'tictoc', 'plyr', 'gbm', 'testthat', 'here'))"

# Copy application files
COPY . /app

RUN ["chmod", "+x", "./app/setup.sh"]
RUN ./app/setup.sh

# Set working directory
WORKDIR /app

# Run the R script
CMD ["R", "--slave", "-f", "api/index.R"]