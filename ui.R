  function(req) {
  list(
    htmltools::htmlDependency(
      "font-awesome", 
      "5.3.1", "www/shared/fontawesome", package = "shiny", 
      stylesheet = c("www/css/main.css")
    ),
    # DT:::DTDependency("default"),
    # DT:::extDependency("Buttons", "default", list()),
    tags$head(
      tags$link(href="https://fonts.googleapis.com/css?family=Lato:400,700&display=swap", rel="stylesheet"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/slider.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/main.css"),
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
      tags$script(src = "js/main.js"),
    ),
    tags$head( tags$style( type = "text/css", '
          #cluster-prob2 .irs-line-mid{
            background: #428bca ;
            border: 1px solid #428bca ;
          }
          #cluster-prob2 .irs-line-right{
            background: #428bca ;
          }
          #cluster-prob2 .irs-bar {
            background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
            border-top: 1px solid #CCC ;
            border-bottom: 1px solid #CCC ;
          }
          #cluster-prob2 .irs-bar-edge {
            background: inherit ;
            border: inherit ;
          }

        ')),    
    webpage(
      nav = navbar(
        tags$a(
          class = "navbar-brand",
          href="https://hits.harvard.edu/the-program/laboratory-of-systems-pharmacology/",
          target = "_blank",
          tags$img(class = "h-2", src = "../assets/img/logo.png")
        ),
        tags$h5("shinyDepMap") %>%
          font("white") %>%
          yonder::margin(top = 2),
        navInput(
          appearance = "pills",
          id = "nav",
          choices = list(
            list(icon("home"), "Home"),
            list(icon("dna"), "Gene essentiality"),
            list(icon("star-of-life"), "Gene cluster")
          ),
          values = c(
            "home",
            "essentiality",
            "cluster"
            )
        ) %>%
          yonder::margin(left = "auto"),
        buttonInput(
          id = "about",
          label = "About"
        ) %>% 
          background("indigo") %>% 
          font(color = "white"),
        buttonInput(
          id = "funding",
          label = "Funding"
        ) %>% 
          background("indigo") %>% 
          font("white"),
        tags$a(
          href = "https://github.com/kenichi-shimada/shinyDepMap",
          target = "_blank",
          icon("github", class = "fa-lg")
        ) %>% 
          font(color = "white") %>% 
          yonder::margin(l = 2)
      ) %>%
        active("blue") %>% 
        padding(0, r = 3, l = 3) %>% 
        yonder::margin(b = 2) %>% 
        background("indigo"), # %>% shadow(),
      container(
        navContent(
          # home ----
          navPane(
            id = "page_home",
            fade = FALSE,
            class = "active",
            columns(
              column(
                d4("shinyDepMap") %>% 
                  font(align = "center") %>%
                  yonder::margin (top = 1),
                h4("A web-tool to explore the Cancer Dependency Map (DepMap) project") %>%
                  font(align = "center") %>%
                  yonder::margin(b = 4),
                columns( # ├ applications ----
                  ##
                  column(
                    width = 12,
                    h1("Applications") %>% 
                      yonder::margin(bottom = 3) %>% 
                      font(align = "center")
                  ),
                  ## selectivity
                  column(
                    linkInput(
                      id = "link_essentiality",
                      label = list(
                        tags$img(src = "assets/img/essential.png",
                          border="pixels") %>% 
                          height(10),
                        # icon("circle", class = "fa-6x selectivity--pink"),
                        h2("Gene essentiality") %>% 
                          yonder::margin(top = 2),
                        h5("(All protein-encoding genes)")
                      ) 
                    ) %>% 
                      font(color = "black"),
                    p("Gene essentiality varies across different genes and tested cell lines.",br(),
                      "Explore which gene is essential in hundreds of cell lines."),
                    p(
                      div(
                        class = "d-inline-block",
                        tags$button(
                          class = "btn btn-blue",
                          type = "button",
                          `data-toggle` = "modal",
                          `data-target` = "#modal_essentiality",
                          icon("window-restore"),
                          "Use cases"
                        ),
                        essentialityModal()
                      )
                    )
                  ) %>% 
                    display("flex") %>% 
                    flex(direction = "column") %>% 
                    font(align = "center") %>%
                    shadow("small"),
                  ## similarity
                  column(
                    linkInput(
                      id = "link_cluster",
                      label = list(
                        tags$img(src = "assets/img/clusters.png") %>% 
                          height(10),
                        # icon("circle", class = "fa-6x similarity--green"),
                        h2("Gene cluster") %>% 
                          yonder::margin(top = 2),
                        h5("(Essential genes)")
                      ) 
                    ) %>% 
                      font(color = "black"),
                    p("Genes that work in complexes/pathways show similar dependency across cell lines.",br(),
                      "Discover the functions of essential genes via the gene clusters."),
                    p(
                      div(
                        class = "d-inline-block",
                        tags$button(
                          class = "btn btn-blue shadow-sm",
                          type = "button",
                          `data-toggle` = "modal",
                          `data-target` = "#modal_cluster",
                          icon("window-restore"),
                          "Use cases"
                        ),
                        clusterModal()
                      )  
                    )
                  ) %>% 
                    display("flex") %>% 
                    flex(direction = "column") %>% 
                    font(align = "center") %>%
                    shadow("small"),
                  column(
                    width = 12,
                    h5(
                      tags$a(
                        href="https://labsyspharm.shinyapps.io/depmap_v1",
                        target="_blank",
                        "Older version (ver.1)")
                    )
                  ) %>%
                      yonder::margin(bottom = 3) %>% 
                      font(align = "center")
                    
                )
              ) 
            )
          )
          ,
          # essentiality ----
          navPane(
            id = "page_essentiality",
            essentialityUI(
              id = "essentiality"
            )
          # )
          ),
          # cluster ----
          navPane(
            id = "page_cluster",
            clusterUI(
              id = "cluster"
            )
          )
        )
      )
    )
  )  
}
