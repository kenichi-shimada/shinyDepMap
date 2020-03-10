function(req) {
  list(
    htmltools::htmlDependency(
      "font-awesome", 
      "5.3.1", "www/shared/fontawesome", package = "shiny", 
      stylesheet = c("css/all.min.css", "css/v4-shims.min.css")
    ),
    # DT:::DTDependency("default"),
    # DT:::extDependency("Buttons", "default", list()),
    tags$head(
      tags$link(href="https://fonts.googleapis.com/css?family=Lato:400,700&display=swap", rel="stylesheet"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/slider.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/main.css"),
      tags$script(src = "js/main.js"),
    ),
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
                h4("A web-tool to explore the Cancer Dependency Map (DepMap) project.") %>%
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
                        h5("(15,847 protein-encoding genes)")
                      ) 
                    ) %>% 
                      font(color = "black"),
                    p("Gene essentiality varies across different genes and cells to test.",br(),
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
                        h5("(2,492 essential genes)")
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
                    shadow("small")
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
