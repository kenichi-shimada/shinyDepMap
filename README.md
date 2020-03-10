# shinyDepMap 
A shiny-based interactive web tool to analyze [DepMap](https://depmap.org/) data
(based on their 19q3 release). There are several options for using it:

1. [Use the public version of the tool](https://labsyspharm.shinyapps.io/depmap/)
2. [Run the tool locally in R](#run-with-r)
3. [Run the tool locally using Docker](#run-with-docker)

## Run with R

### Preparation
To launch this tool locally in R, clone or download this repository and install
the required R pacakges as follows:

```r
install.packages("devtools")
install.packages("BiocManager")

BiocManager::install(c("ggplot2","shiny","markdown","RColorBrewer","shinyWidgets","plotly","DT","igraph","visNetwork","tibble","dplyr","tidyr","yonder"))
```

### Launch the app
In the R process, modify the path to the parent directory of the source directory and run the code. 
```r
library(shiny)
setwd("/_the_parent_dir_of_the_source_dir_/")
runApp("shinyDepMap")
```

## Run with Docker 
	(to be implemented)
	An alternative to the _Run with R_ option. Run the following commands, then open
	your web browser to http://127.0.0.1:8888/ .

	```
	docker pull labsyspharm/shinydepmap:latest
	docker run -p 8888:8888 labsyspharm/shinydepmap:latest
	```

## Bug reports
Please contact [Kenichi Shimada](mailto:kenichi_shimada@hms.harvard.edu) in case
you find a bug.

## Version history
The current version is v2.0. 
The older version (v1.0) can be found in the *depmap_flexdashboard* branch of this repo.

## Session info

	R version 3.6.1 (2019-07-05)
	Platform: x86_64-apple-darwin15.6.0 (64-bit)
	Running under: macOS High Sierra 10.13.6

	Matrix products: default
	BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
	LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

	locale:
	[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

	attached base packages:
	[1] stats     graphics  grDevices utils     datasets  methods   base     

	other attached packages:
	 [1] yonder_0.2.0       markdown_1.1       tidyr_0.8.3        dplyr_0.8.3       
	 [5] tibble_2.1.3       visNetwork_2.0.8   igraph_1.2.4.1     DT_0.8            
	 [9] plotly_4.9.0       shinyWidgets_0.4.8 RColorBrewer_1.1-2 ggplot2_3.2.1     
	[13] shiny_1.4.0       

	loaded via a namespace (and not attached):
	 [1] Rcpp_1.0.2        pillar_1.4.2      compiler_3.6.1    later_1.0.0      
	 [5] tools_3.6.1       digest_0.6.21     viridisLite_0.3.0 jsonlite_1.6     
	 [9] gtable_0.3.0      pkgconfig_2.0.3   rlang_0.4.0       crosstalk_1.0.0  
	[13] yaml_2.2.0        xfun_0.9          fastmap_1.0.1     withr_2.1.2      
	[17] httr_1.4.1        htmlwidgets_1.3   grid_3.6.1        tidyselect_0.2.5 
	[21] glue_1.3.1        data.table_1.12.2 R6_2.4.0          purrr_0.3.2      
	[25] magrittr_1.5      scales_1.0.0      promises_1.1.0    htmltools_0.4.0  
	[29] assertthat_0.2.1  mime_0.7          xtable_1.8-4      colorspace_1.4-1 
	[33] httpuv_1.5.2      lazyeval_0.2.2    munsell_0.5.0     crayon_1.3.4     

