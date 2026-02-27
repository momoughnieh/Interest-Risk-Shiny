FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y git libwebpmux3

RUN git clone https://github.com/momoughnieh/Interest-Risk-Shiny.git /srv/shiny-server/Interest-Risk-Shiny

RUN Rscript /srv/shiny-server/Interest-Risk-Shiny/risk_app/requirements.R

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/Interest-Risk-Shiny/risk_app', host='0.0.0.0', port=3838)"]
