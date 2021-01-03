library(ggplot2) 
library(RColorBrewer)
library(shinyWidgets)
library(plotly)
library(DT)
library(igraph)
library(visNetwork)
library(aws.s3) # this should be removed
library(tibble)
library(shinyjs)
library(dplyr)
library(tidyr)
library(markdown)
library(yonder)

source("modules/modals.R",local=TRUE)
source("modules/essentiality.R",local=TRUE)
source("modules/cluster.R",local=TRUE)

x <- load("data/depmap_initial_19q3_v3_local_run.rda")

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
