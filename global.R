library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)
library(plotly)
library(DT)
library(igraph)
library(visNetwork)
library(tibble)
library(shinyjs)
library(dplyr)
library(tidyr)
library(markdown)
library(yonder)

source("modules/modals.R",local=TRUE)
source("modules/essentiality.R",local=TRUE)
source("modules/cluster.R",local=TRUE)

# Download data file if not present in the current directory.
filename <- "depmap_initial_19q3_v3_local_run.rda"
if (!file.exists(filename) | file.size(filename) != 415441828) {
    cat("\nDownloading data file...\n\n")
    options(timeout=.Machine$integer.max)
    if (download.file("https://ndownloader.figshare.com/files/25893237", filename, mode="wb")) {
        file.remove(filename)
        stop("Could not download data file. Please check your network connection and try again.")
    } else {
        cat("\nDownload successful\n\n")
    }
}
# Load the data file, which should exist at this point.
cat("Loading data file...\n")
x <- load(filename)

mix.ratios <- c("shRNA","80:20","60:40","40:60 (default)","20:80","CRISPR")
names(eids.ess) <- names(thres.eff) <- mix.ratios

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

# df <- df %>% mutate(pri=uniq.cols[round(rank(df$selectivity)/nrow(df)*n)+1])
# hover: add probability

scoreDefinitionServer <- function(input, output, session) {
  return(
    list(
      defButton = reactive({ input$defButton }),
      mix_ratio = reactive({ input$mix_ratio }),
      threshold = reactive({ input$threshold })
    )
  )
}

##
