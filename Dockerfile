FROM rocker/r-ver:4.6.1

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl libcurl4-openssl-dev libssl-dev libxml2-dev libpoppler-cpp-dev \
    libuv1-dev \
    libfontconfig1-dev libfreetype6-dev && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY renv.lock .Rprofile DESCRIPTION install_packages.R /app/
COPY renv/activate.R renv/settings.json /app/renv/
RUN Rscript install_packages.R
COPY . /app

EXPOSE 3838
CMD ["Rscript", "-e", "shiny::runApp('/app/app.R', host='0.0.0.0', port=3838, launch.browser=FALSE)"]
