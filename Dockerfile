FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c(\"shiny\", \"shinydashboard\", \"shinyWidgets\", \"MLmetrics\", \"ranger\", \"caretEnsemble\", \"httr\", \"magrittr\", \"dplyr\", \"ggplot2\", \"ems\"), repos='http://cran.rstudio.com/')"

# Copy app files
WORKDIR /srv/shiny-server/
COPY . /srv/shiny-server/

# Expose Shiny port
EXPOSE 3838

# Run app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
