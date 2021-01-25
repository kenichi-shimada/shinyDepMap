FROM r-base:3.6.2

RUN apt-get update \
    && apt-get install -y r-cran-devtools r-cran-biocmanager r-cran-ggplot2 \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript \
  -e "BiocManager::install(c('shiny','markdown','RColorBrewer','shinyWidgets','plotly','DT','igraph','visNetwork','tibble','shinyjs','dplyr','tidyr','yonder'))"

COPY / /app/shinyDepMap

# Pre-download data file so it's included in the image and users don't need
# to wait for it to download on every run. Validate total size is correct
# as an extra sanity check against interrupted / timed-out downloads.
RUN wget \
  -O /app/shinyDepMap/depmap_initial_19q3_v3_local_run.rda \
  --progress=dot:giga \
  https://ndownloader.figshare.com/files/25893237 \
  && [ "$(stat -c %s /app/shinyDepMap/depmap_initial_19q3_v3_local_run.rda)x" = 415441828x ]

WORKDIR /app
EXPOSE 8888
CMD [ \
    "/usr/bin/Rscript", \
    "-e", "library(shiny)", \
    "-e", "runApp('shinyDepMap', host='0.0.0.0', port=8888)" \
]
