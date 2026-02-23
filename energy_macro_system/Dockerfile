# Energy–Macro Shock Propagation Simulator — Shiny (DigitalOcean App Platform)
# Build from repo root: docker build -f energy_macro_system/Dockerfile energy_macro_system
# Or from app dir:  cd energy_macro_system && docker build -t energy-macro-shiny .

FROM --platform=linux/amd64 rocker/shiny:4.3.2

# System libraries required by R packages (httr, openssl, xml2, etc.)
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Install all R packages required by app.R and R/*.R (see install_packages.R)
COPY install_packages.R /tmp/install_packages.R
RUN Rscript /tmp/install_packages.R

# Copy app into Shiny server directory
COPY . /srv/shiny-server/

RUN chown -R shiny:shiny /srv/shiny-server && \
    mkdir -p /srv/shiny-server/data && chown shiny:shiny /srv/shiny-server/data

EXPOSE 8080

USER shiny
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', '8080')))"]
