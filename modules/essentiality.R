essentialityUI <- function(id) {
  ns <- NS(id)
  
  columns(
    column(
      width = 3,
      card(
        header = navInput(
          appearance = "tabs",
          id = ns("nav"),
          choices = c("Input", "Help"),
          values = c("filters", "instructions"),
          selected = "filters"
        ),
        navContent(
          navPane(
            id = ns("pane_filters"),
            fade = FALSE,
            formGroup(
              label = "Type in a gene name",
              input = textInput(
                id = ns("sym.query"),
                value = "",
                placeholder=""
              ),
            help = "A full or partial gene symbol is accepted"
            ),# %>% yonder::margin(b = -3),
            formGroup(
              label = NULL,
              input = buttonInput(
                id = ns("goButton"), 
                label = "Go",
                tabindex="-1"
              )
            ),
            formGroup(
              label = "Select a matched gene",
              input = selectInput(
                id = ns("sym1")
              ),
              help = "to look at its dependency scores"
            )
          ),
          navPane(
            id = ns("pane_instructions"),
            fade = FALSE,
            # p("The Gene essentiality app helps you find essential and selective genes"),
            # p("To use this app:"),
            tags$ol(
              class = "pl-2",
              tags$li("Type in a gene symbol (e.g., \"CDK4\") or its prefix (e.g., \"CDK\") ",
                      "in the textbox ", tags$b('Type in a gene name')," in the top-left corner"),
              tags$li("Click the ",tags$b('Go'), "button"),
              tags$li("The app will show genes matched with your query in ",tags$b('Efficacy/Selectivity plot')," and ",tags$b('Matched genes')," tab"),
              tags$ul(
                tags$li(tags$b("Efficacy"),": how essential a gene is in the sensitive cells (the more negative, the more essential)"),
                tags$li(tags$b("Selectivity"),": how different the gene essentiality is between the sensitive and resistant cells (the higher, the more selective)"),
               ),
              tags$li("Click a gene symbol on the ",tags$b("Matched genes"),
                  " and you will find what is known about the gene from the Gene Cards website"),
              tags$li("Select one of the matched genes from the ",tags$b('Select a hit gene')," dropdown in the lower-left",
                      "and you will find its dependency scores across hundreds of cell lines in the ",tags$b('Dependency scores')," tab")
            )
          )
        )
      )

    ),
    column(
      width = 5,
      class = "function-tab",
      card(
        header=h6("Efficacy/Selectivity (15,847 genes)"),
        div(
          plotlyOutput(
            outputId = ns("all_eff_sel"),
            height = "80vh"
          )
        ) 
      )
    ),
    column(
      width = 4,
      class = "function-tab",
      card(
        header = navInput(
          appearance = "tabs",
          id = ns("nav2"),
          choices = c("Matched genes", "Dependency scores"),
          values = c("gene_list", "dep_scores"),
          selected = "gene_list"
        ),
        navContent(
          navPane(
            id = ns("pane_gene_list"),
            fade = FALSE,
            div(
              dataTableOutput(
                outputId = ns("all_gene_list"),
                width = "100%",
                height = "70vh"
              )
            ) #%>% yonder::margin(all = 0)
          ),
          navPane(
            id = ns("pane_dep_scores"),
            fade = FALSE,
            div(
              plotlyOutput(
                outputId = ns("all_lineage"),
                width = "100%",
                height = "75vh"
              )
            ) #%>% yonder::margin(all = 0)
          )
        ) #%>% yonder::margin(all = 0),
      )
    )
  )
} 


essentialityServer <- function(input, output, session) {
  ns <- session$ns
  
  observeEvent(input$nav, {
    switch(
      input$nav,
      filters = showNavPane(ns("pane_filters")),
      instructions = showNavPane(ns("pane_instructions"))
    )
  })

  observeEvent(input$nav2, {
    switch(
      input$nav2,
      gene_list = showNavPane(ns("pane_gene_list")),
      dep_scores = showNavPane(ns("pane_dep_scores"))
    )
  })

  all.scatter <- reactive({
    nlayer <- 31
    dthres <- 0.10
    suppressWarnings(
      p <- ggplot(df,aes(efficacy,selectivity)) +
        geom_vline(xintercept=c(0,thres.eff), col=c("grey50","grey70")) +
        geom_hline(yintercept=c(0,1),col=c("grey50","grey70")) + 
        stat_density_2d(aes(fill = stat(level)),geom="polygon",bins=nlayer) +
        scale_fill_gradientn(colors=rev(blues9)) + 
        geom_point(data=(df %>% filter(dens <= dthres)),
          aes(efficacy, selectivity,text=text), 
          color=blues9[9], size = 1) +
        theme_bw() +
          theme(legend.position="none",plot.margin=unit(c(0,0,0,0),"lines"),
            axis.text=element_text(size=10),axis.title=element_text(size=10),
            plot.title=element_text(size=10),
            axis.text.x = element_text(angle = 0)) +
        geom_point(data=dummy[1,],aes(efficacy,selectivity),col=NA,size=1) + # sym.query
        geom_point(data=dummy[2,],aes(efficacy,selectivity),col=NA,size=1) # sym1
    )
    ply <- plotly::ggplotly(p,tooltip="text")
    ndata <- length(ply$x$data)
    sapply(ply$x$data,function(x)length(x$text))  
    for(i in seq(nlayer+4)){
      ply$x$data[[i]]$hoverinfo <- "none"
    }
    ply
  })

  output$all_eff_sel <- renderPlotly({
    options(warn=0)
    all.scatter()    
  })

  matched.genes <- reactive({ ## in response to symbol, get the probe
    input$goButton
    Sys.sleep(1)
    sym.query <- toupper(isolate(input$sym.query))
    if(is.null(sym.query)|| length(sym.query) ==0 || sym.query ==""){
      this.df <- df[F,]
    }else{
      this.df <- df %>% 
        filter(grepl(paste0("^",sym.query),df$sym,ignore.case=T)) %>%
        arrange(efficacy)

      if(any(this.df$sym==sym.query)){
        idx <- which(this.df$sym==sym.query)
        ri <- c(idx,seq(nrow(this.df))[-idx])
        this.df <- this.df[ri,]
      }
    }
    this.set <- this.df$sym

  })

  observeEvent(matched.genes(), {

    # if(matched.genes()=="")
    updateSelectInput(
      id = "sym1",
      choices = matched.genes(),
      values = matched.genes(),
      selected = matched.genes()[1],
      session = session
    )
  },ignoreInit=TRUE,ignoreNULL=TRUE)


  # update.plot <- reactive({
  #   input$sym1
  # })

  observeEvent(input$sym1, {
    sym1 <- input$sym1
    i.sym1 <- which(df$sym==sym1)
    grep.sym.query <- which(df$sym %in%  matched.genes())
    if(length(matched.genes())>2){
      grep.sym.query <- grep.sym.query[grep.sym.query != i.sym1]
    }
    if(length(matched.genes())==2){
      grep.sym.query <- rep(grep.sym.query,2)
    }

    df.1 <- df[grep.sym.query,] %>%
      mutate(text = paste0(sym.all,"<br />eff:",efficacy,", sel:",selectivity))
    df.2 <- df[rep(i.sym1,2),] %>%
      mutate(text = paste0(sym.all,"<br />eff:",efficacy,", sel:",selectivity))

    plotlyProxy('all_eff_sel') %>%
          plotlyProxyInvoke("restyle", 
            list(x = list(df.1$efficacy), 
              y = list(df.1$selectivity),
              text = list(df.1$text),
              marker=list(color='rgba(255,128,0,1)',size=6)),
            list(36)) %>%
          plotlyProxyInvoke("restyle", 
            list(x = list(df.2$efficacy), 
              y = list(df.2$selectivity),
              text = list(df.2$text),
              marker=list(color='rgba(255,25,0,1)',size=6)),
            list(37))
  },ignoreInit=TRUE,ignoreNULL=TRUE)


  dtable <- reactive({
    sym1 <- input$sym1
    eids <- df$eid[df$sym %in% matched.genes()]
    # stop(paste(sym.query,collapse=","))

    dt <- df %>% 
      filter(eid %in% eids) %>%
      arrange(efficacy,desc(selectivity)) %>%
      select(sym, efficacy, selectivity) %>%
      mutate(selected = as.numeric(sym == sym1)) %>%
      mutate(sym = paste0('<a href="https://www.genecards.org/cgi-bin/carddisp.pl?gene=',sym,'" target="_blank">',sym,'</a>')) %>%
      `colnames<-`(c("Gene","Efficacy","Selectivity","selected")) %>%
      datatable(extensions = 'Scroller', 
        options = list(
          columnDefs = list(list(targets = 4, visible = FALSE)),
  　　        rowReorder = TRUE,
          deferRender = TRUE,
          scrollY = "70vh",
          pageLength=30,
          scroller = TRUE,
          order = list(list(2, 'asc'), list(3, 'desc')),
          sDom  = '<"top">lt<"bottom">ip'),
        selection=list(mode="none"),
        editable=FALSE,escape=FALSE) %>%
          formatStyle(
        4,
        target = 'row',
        backgroundColor = styleEqual(c(1,0), c('#ffcccb', NA))
    )
    return(dt)   
  })

  output$all_gene_list <- renderDataTable({
    options(warn=0)
    # validate(
    #   need(length(input$sym1)!=0 && length(matched.genes())!=0,"No genes selected")
    # )
    dtable()
  })

  dep.score.plot <- reactive({
    sym1 <- input$sym1
    if(length(sym1)==0 || length(matched.genes())==0){
      ply <- NULL
    }else{
      sym.query <- toupper(isolate(input$sym.query))
      sym1 <- isolate(input$sym1)
      eid1 <- df$eid[df$sym==sym1]
      # this.ef <- s3readRDS(paste0("scores_all_19q3/",eid1,".rds"),bucket="depmap")
      this.ef <- p.scores[eid1,]

      ef.1 <- data.frame(ef=this.ef,DepMap_ID=names(this.ef),stringsAsFactors=FALSE) %>%
        left_join(cl.info,by="DepMap_ID") %>%
        mutate(cell.name=sub("^([^_]+)_.+","\\1",CCLE_Name))

      med.ef <- tapply(ef.1$ef,ef.1$lineage,mean)
      o <- order(med.ef,decreasing=F)
      cell.levs <- names(med.ef)[o]

      ef.1 <- ef.1 %>% 
        mutate(lineage=factor(lineage,levels=cell.levs)) %>%
        mutate(text=paste0(
          "Cell: ",cell.name,"<br />",
          "Type: ",lineage,"<br />",
          "Subtype: ",lineage_subtype,"<br />",
          "Sub-subtype: ",lineage_sub_subtype,"<br />",
          "Efficacy: ",round(ef,3),"<br />"))

      pvio <- ef.1 %>%
        ggplot(aes(x=lineage,y=ef)) +
        geom_violin(scale="area",aes(fill=lineage,color=lineage)) +
        geom_jitter(height = 0, width = 0.2, size=.5) +
        coord_flip() +
        theme_bw() +
        theme(legend.position="none",plot.margin=unit(c(0,0,0,0),"lines"),
          axis.text=element_text(size=8),axis.title=element_text(size=10),
          plot.title=element_text(size=15, hjust=0.5),
          axis.text.x = element_text(angle = 0)) +
        ggtitle(sym1) + 
        labs(x="",y="Dependency score")
      ply <- plotly::ggplotly(pvio) %>% config(displayModeBar=F)
      ndata <- length(ply$x$data)
      for(i in seq(ndata-1))ply$x$data[[i]]$hoverinfo <- "none"
        ply$x$data[[ndata]]$text <- ef.1$text
      ply

      return(ply)
    }
  })

  output$all_lineage <- renderPlotly({
    options(warn=0)
    validate(need(!is.null(dep.score.plot()),""))
    dep.score.plot()    
  })
}
