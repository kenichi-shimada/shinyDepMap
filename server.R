function(input, output, session) {
  navToPage <- function(name) {
    updateNavInput("nav", selected = name)
    showNavPane(paste0("page_", name))
  }
  
  observeEvent(input$nav, {
    navToPage(input$nav)
  }) 

  .modal_about <- modal(
    id = NULL,
    size = "lg",
    header = h5("About"),
    HTML(htmltools::includeMarkdown("inst/about.md"))
  )
  observeEvent(input$about, {
    showModal(.modal_about)
  })
  
  .modal_funding <- modal(
    id = NULL,
    size = "md",
    header = h5("Funding"),
    p("This work is financially supported by Japan Society for the Promotion of Science Overseas Research Fellowship (to Shimada K), NIH R35GM131753 (to Mitchison TJ), and P50GM107618.")
  )
  observeEvent(c(input$funding),{
    showModal(.modal_funding)
  })
  
  observeEvent(c(input$link_essentiality), {
    navToPage("essentiality")
  })

  observeEvent(c(input$link_cluster), {
    navToPage("cluster")
  })
  
  callModule(
    module = essentialityServer,
    id = "essentiality"
  )

  callModule(
    module = clusterServer,
    id = "cluster"
  )
  
}