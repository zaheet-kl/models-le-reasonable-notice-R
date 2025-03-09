FROM r-base:4.4.2

# Install system dependencies
RUN apt-get update && apt-get install -y \
  libcurl4-openssl-dev \
  cmake \ 
  libsodium-dev \
  libssl-dev \
  && rm -rf /var/lib/apt/lists/*

# Copy setup files
COPY ./setup.sh /app/setup.sh

RUN ["chmod", "+x", "./app/setup.sh"]
RUN ./app/setup.sh

# Set working directory
WORKDIR /app