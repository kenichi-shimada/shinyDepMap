FROM r-base:3.6.2

RUN apt-get update \
    && apt-get install -y r-cran-devtools r-cran-biocmanager r-cran-ggplot2 \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript \
  -e "BiocManager::install(c('shiny','markdown','RColorBrewer','shinyWidgets','plotly','DT','igraph','visNetwork','tibble','dplyr','tidyr','yonder'))"

COPY / /app/shinyDepMap

WORKDIR /app
EXPOSE 8888
CMD [ \
    "/usr/bin/Rscript", \
    "-e", "library(shiny)", \
    "-e", "runApp('shinyDepMap', host='0.0.0.0', port=8888)" \
]
