FROM r-base:3.6.2

RUN apt-get update \
    && apt-get install -y r-cran-devtools r-cran-biocmanager r-cran-ggplot2 \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript \
  -e "BiocManager::install(c('shiny', 'grid','RColorBrewer','shinyWidgets','plotly','DT','visNetwork','aws.s3','tibble','dplyr','tidyr'))"
RUN Rscript \
  -e "devtools::install_github('rstudio/flexdashboard@ccb5f1ad057f42da24818d6ff3acb0f4e6b944cb')"

COPY / /app

WORKDIR /app
EXPOSE 8888
CMD [ \
    "/usr/bin/Rscript", \
    "-e", "library(rmarkdown)", \
    "-e", "run('depmap.Rmd', shiny_args=list(host='0.0.0.0', port=8888))" \
]
