# Energy–Macro Shock Propagation Simulator — Shiny (DigitalOcean App Platform)
# Build from repo root: docker build -f energy_macro_system/Dockerfile energy_macro_system
# Or from app dir:  cd energy_macro_system && docker build -t energy-macro-shiny .

FROM rocker/shiny:4.3.2

RUN R -e "install.packages(c('shiny', 'bslib', 'dplyr', 'tidyr', 'readr', 'ggplot2', 'DT', 'httr', 'jsonlite', 'lubridate', 'vars', 'MASS', 'Matrix'), repos = 'https://cloud.r-project.org', Ncpus = 2)"

COPY . /srv/shiny-server/

RUN chown -R shiny:shiny /srv/shiny-server && \
    mkdir -p /srv/shiny-server/data && chown shiny:shiny /srv/shiny-server/data

EXPOSE 8080

USER shiny
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', '8080')))"]
