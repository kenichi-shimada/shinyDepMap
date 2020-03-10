library(ggplot2) 
library(RColorBrewer)
library(shinyWidgets)
library(plotly)
library(DT)
library(igraph)
library(visNetwork)
# library(aws.s3)
library(tibble)
library(dplyr)
library(tidyr)
library(markdown)
library(yonder)

source("modules/modals.R",local=TRUE)
source("modules/essentiality.R",local=TRUE)
source("modules/cluster.R",local=TRUE)

x <- load("data/depmap_initial_19q3_local_run.rda")

##
if(0){
	# install.packages("aws.signature", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))
	# install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))
	library(shiny)
	setwd("~/Dropbox (HMS)/projects/shinyapps/shinyDepMap/")
	runApp("depmap-local")
}
