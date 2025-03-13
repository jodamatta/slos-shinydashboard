FROM rocker/r-ver:4.2.0

ENV RENV_VERSION=v1.0.2
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
RUN R -e "options(renv.config.repos.override = 'https://packagemanager.posit.co/cran/latest')"
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('shinydashboard')"
RUN R -e "install.packages('shinyWidgets')"
RUN R -e "install.packages('MLmetrics')"
RUN R -e "install.packages('ranger')"
RUN R -e "install.packages('caretEnsemble')"
RUN R -e "install.packages('httr')"
RUN R -e "install.packages('magrittr')"

COPY . /app

WORKDIR /app

RUN R -e "renv::restore()"

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('app.R', host='0.0.0.0', port=3838)"]
