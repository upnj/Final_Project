# start from the rstudio/plumber image
FROM rstudio/plumber

# install the linux libraries needed for plumber and R packages
RUN apt-get update -qq && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libpng-dev \
    pandoc \
    libxml2-dev \
    libgit2-dev

# install R packages with a specified CRAN mirror
RUN R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); \
    install.packages(c(\
    'plumber', \
    'tidymodels', \
    'rpart', \
    'dplyr', \
    'yardstick', \
    'randomForest'\
    ))"


# copy the API script and dataset
COPY API.R /
COPY diabetes_binary_health_indicators_BRFSS2015.csv /

# open port 8000 to traffic
EXPOSE 8000

# when the container starts, start the API.R script
ENTRYPOINT ["R", "-e", \
    "pr <- plumber::plumb('API.R'); pr$run(host='0.0.0.0', port=8000)"]