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
            ),

            tags$hr(),
            materialSwitch(
              inputId = ns("definition"),
              label = "Tuning dependency score:",
              value=FALSE
            ),

            setSliderColor(c("Teal","Teal"),c(1,2)),

            conditionalPanel(
              paste0("input[\'",ns("definition"),"\']"),
              sliderTextInput(
                inputId = ns("mix_ratio"),
                label = "Mix Ratio (shRNA:CRISPR)",
                choices = mix.ratios,
                selected = "40:60 (default)",
                hide_min_max = FALSE,
                grid=TRUE
              )
            ),
            conditionalPanel(
              paste0("input[\'",ns("definition"),"\']"),
              sliderTextInput(
                inputId = ns("threshold"),
                label = "Efficacy threshold (percentile)",
                choices = c("0.5","1 (default)","2.5","5","10","25")[-1],
                selected = "1 (default)",
                hide_min_max = FALSE,
                grid=TRUE
              )
            ),
            conditionalPanel(
              paste0("input[\'",ns("definition"),"\']"),
              buttonInput(
                id = ns("defButton"), 
                label = "Recompute dependency score",
                tabindex="-1"
              ) %>%
              background("teal")
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
                      "in the textbox in the top-left and click ",tags$b('Go'), "button"),
              tags$li("Gene symbols matched with your query are highlighted in ",
                tags$b('Efficacy/Selectivity plot')," and listed on ",tags$b('Matched genes')),
              tags$ul(
                tags$li(tags$b("Efficacy"),"describes how essential a gene is in the sensitive cells (the more negative, the more essential)"),
                tags$li(tags$b("Selectivity"),"describes how different the gene essentiality is between the sensitive and resistant cells (the more positive, the more selective)"),
               ),
              tags$li("Click a gene symbol on the ",tags$b("Matched genes"),
                  " and you will jump to the gene's description in the Gene Cards website."),
              tags$li(tags$b('Select a matched gene'),"from a dropdown menu in the lower-left",
                      "and you will find the gene's dependency scores across hundreds of cell lines in the ",tags$b('Dependency scores')," tab"),
              tags$li("[Optional] Two parameters, ", tags$b("Mix Ratio"), "and ", tags$b("Efficacy threshold"),
                  ", affect the definition of the Dependency scores, which influences the gene essentiality. ",
                  " Note: please change these parameters at your own risk; ",
                  "the Dep score to which shRNA contributes more than CRISPR seems less accurate and we don't recommend its use."),
              tags$ul(
                tags$li("Toggle the Dependency parameters on"),
                tags$li("Choose a ",tags$b("Mix Ratio (shRNA/CRISPR)")),
                tags$li("Choose a ",tags$b("Efficacy threshold (percentile)")),
                tags$li("Click ",tags$b("Recompute dependency score"),"button"),
                tags$li("Change in the ",tags$b("Efficacy threshold"), "affect the clusters"),
              )              
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

essentialityServer <- function(input, output, session, defs) {
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

  observeEvent(defs$defButton(), {
    updateSliderTextInput(session,
      inputId = "mix_ratio",
      selected = defs$mix_ratio()
    )

    updateSliderTextInput(session,
      inputId = "threshold",
      selected = defs$threshold()
    )
  },ignoreInit=TRUE)

  im <- reactive({
    input$defButton
    im <- isolate(input$mix_ratio)
    if(!is.null(im)){
      i.mix.ratio <- match(im,mix.ratios)
    }
    return(i.mix.ratio)
  })

  it <- reactive({
    input$defButton
    it <- isolate(input$threshold)
    if(!is.null(it)){
      i.thres <- match(it,c("0.5","1 (default)","2.5","5","10","25"))
    }
    return(i.thres)
  })

  df <- reactive({
    input$defButton
    this <- dfs[[isolate(im())]][[isolate(it())]] %>%
      left_join(com,by="eid")
  })

  matched.genes <- reactive({ ## in response to symbol, get the probe
    input$goButton
    # Sys.sleep(1)
    sym.query <- toupper(isolate(input$sym.query))
    if(is.null(sym.query)|| length(sym.query) ==0 || sym.query ==""){
      this.df <- df()[F,]
    }else{
      this.df <- df() %>% 
        filter(grepl(paste0("^",sym.query),df()$sym,ignore.case=T)) %>%
        arrange(efficacy)

      if(any(this.df$sym==sym.query)){
        idx <- which(this.df$sym==sym.query)
        ri <- c(idx,seq(nrow(this.df))[-idx])
        this.df <- this.df[ri,]
      }
    }
    this.set <- this.df$sym

  })

  all.scatter <- reactive({
    df.1 <- df()

    # stop(paste(im(),it(),sep="")) # works ok
    sym1 <- isolate(input$sym1)
    eid1 <- df.1$eid[df.1$sym==sym1]
    sym.query <- toupper(isolate(input$sym.query))
    df <- df()

    is.sym.query <- df$sym %in% isolate(matched.genes())

    df.2 <- df %>% 
      filter(is.sym.query) %>%
      mutate(text = paste0(sym.all,"<br />eff:",efficacy,", sel:",selectivity))

    n1 <- nrow(df.2)

    if(n1!=0){
      idx <- which(df.2$sym==sym1)

      cols.1 <- rep(c('rgba(255,128,0,1)','rgba(255,25,0,1)'),c(n1-1,1))
      o <- c(seq(nrow(df.2))[-idx],idx)
      df.2 <- df.2[o,]

      if(n1==1){
        df.2 <- rbind(df.2,df.2)
      }

      ttl <- paste0(sym1," (query: ",sym.query,")")
    }else{
      df.2 <- df.2[NULL,]
      cols.1 <- 'rgba(0,0,0,1)'
      ttl <- ""      
    }

    sym.all2 <- ifelse(eid1=="","",(df.1 %>% filter(eid == eid1))$sym.all)

    n.dthres <- 3000

    dthres <- sort(df.1$dens)[n.dthres]

    xrng <- range(df.1$efficacy) + c(-1,1)*0.03
    yrng <- range(df.1$selectivity) + c(-1,1)*0.03

    dxy <- MASS::kde2d(df.1$efficacy, df.1$selectivity,n=60)
    names(dxy)[1:2] <- c("efficacy","selectivity")
    dxy$z <- t(dxy$z)
    max.z <- max(dxy$z)


    nlayer <- 150
    lst <- lapply(seq(nlayer),function(i){
      i1 <- seq(0,1,length=nlayer)[i]
      i2 <- apply(col2rgb(colorRampPalette(rev(blues9))(nlayer)),2,function(x){
          paste0("rgba(",x[1],",",x[2],",",x[3],",1)")
        })[i]
      return(c(i1,i2))
    })

    lst[[1]] <- c(0,'rgba(0,0,0,0)')

    ply <- plot_ly() %>%
      add_lines(
        x= rep(thres.eff[[isolate(im())]],2), 
        y=c(-1,4),
        name = "thres",
        line = list(
          color = 'rgba(0,0,0,1)',
          shape = "linear",
          width = 0.5
        )) %>%
      add_trace(
        data = dxy,
        x= ~efficacy,
        y= ~selectivity,
        z= ~z,
        type="contour",
        zauto=FALSE,
        zmin=dthres*.7,
        zmax=max.z,
        colorscale=lst,#list(c(0, 'rgba(0,0,255,0)'), c(1, 'rgba(255,0,0,1)')),
        contours=list(coloring="fill",showlines=FALSE),
        ncontours=nlayer,
        hoverinfo="none",
        showscale=FALSE
      ) %>%
      add_markers(
        data = (df.1 %>% filter(dens <= dthres)),
        x = ~efficacy, 
        y = ~selectivity,
        hoverinfo = 'text',
        text= ~sym,
        marker = list(
          color= blues9[9],
          size=4)
      ) %>%
      add_markers(
        data = df.2,
        x = ~efficacy, 
        y = ~selectivity,
        hoverinfo = 'text',
        text= ~sym,
        marker = list(
          color= cols.1,#'rgba(0,0,0,0)',
          size=6
        )
      ) %>%
      layout(title=ttl,#list(title=ttl),# ifelse(eid1=="","",sym.all2),
        xaxis=list(range = xrng, width = 0.5),
        yaxis=list(range = yrng),
        showlegend = FALSE)
    ply

    # stop(paste(sapply(ply$x$visdat,function(x)nrow(x())),collapse=" "))
  })

  output$all_eff_sel <- renderPlotly({
    options(warn=0)
    all.scatter()    
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

  observeEvent(input$sym1, {  
    sym1 <- input$sym1
    sym.query <- toupper(isolate(input$sym.query))
    df <- df()

    is.sym.query <- df$sym %in%  matched.genes()

    df.1 <- df %>% 
      filter(is.sym.query) %>%
      mutate(text = paste0(sym.all,"<br />eff:",efficacy,", sel:",selectivity))

    n1 <- nrow(df.1)

    if(n1!=0){
      idx <- which(df.1$sym==sym1)

      cols.1 <- rep(c('rgba(255,128,0,1)','rgba(255,25,0,1)'),c(n1-1,1))
      o <- c(seq(nrow(df.1))[-idx],idx)
      df.1 <- df.1[o,]

      if(n1==1){
        df.1 <- rbind(df.1,df.1)
      }

      ttl <- paste0(sym1," (query: ",sym.query,")")
    }else{
      df.1 <- df.1[NULL,]
      cols.1 <- 'rgba(0,0,0,1)'
      ttl <- ""      
    }

    # stop(paste(nrow(df.1)," ",nrow(df.2)))
    # stop(paste(df.2$efficacy," ",class(df.2$efficacy)))
    plotlyProxy('all_eff_sel') %>%
          plotlyProxyInvoke("relayout",
              list(title=ttl)
            ) %>%
          plotlyProxyInvoke("restyle", 
            list(x = list(df.1$efficacy), 
              y = list(df.1$selectivity),
              text = list(df.1$text),
              marker=list(color=cols.1,size=6)),
            list(3))
  } ,ignoreInit=TRUE,ignoreNULL=TRUE)


  dtable <- reactive({
    df1 <- df()
    sym1 <- input$sym1

    eids <- df1$eid[df1$sym %in% matched.genes()]
    # stop(paste(sym.query,collapse=","))

    dt <- df1 %>% 
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
    if(length(input$sym1)==0){    
      validate(need(input$sym1,"Please type in a gene name first."))
    }else{
      validate(need(!is.null(matched.genes())>0,"There are no matched genes."))
    }
    dtable()
  })

  dep.score.plot <- reactive({
    input$defButton
    sym1 <- input$sym1

    if(length(sym1)==0 || length(matched.genes())==0){
      ply <- NULL
    }else{
      # sym.query <- toupper(isolate(input$sym.query))
      sym1 <- isolate(input$sym1)
      eid1 <- df()$eid[df()$sym==sym1]
      # cp <- c(crossoverpoints[,i.mix.ratio],
      #   bile_duct=1, fibroblast=1, prostate=1, cervix=1)

      this.ef <- p.scores[[isolate(im())]][eid1,]
      # this.ef <- s3readRDS(paste0("scores_all_19q3_v3/",isolate(im()),"/",eid1,".rds"),bucket="depmap")
      ef.1 <- data.frame(ef=this.ef,DepMap_ID=names(this.ef),stringsAsFactors=FALSE) %>%
        left_join(cl.info,by="DepMap_ID") %>%
        mutate(cell.name=sub("^([^_]+)_.+","\\1",CCLE_Name)) 
      # cbind(cp,table(ef.1$lineage)[names(cp)])

      if(0){
        n.ess <- tapply(ef.1$ef <= eff.ths[isolate(im())],ef.1$lineage,sum)[names(cp)] >= cp
        tis <- names(which(n.ess))

        ef.1$lineage <- paste0(ef.1$lineage,c("","*")[(ef.1$lineage %in% tis)+1])
      }

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

      pvio <- ggplot(ef.1,aes(x=lineage,y=ef)) +
        geom_hline(yintercept=c(0,thres.eff[[isolate(im())]]), col="grey70") +
        geom_violin(scale="area",aes(fill=lineage,color=lineage)) +
        geom_jitter(height = 0, width = 0.2, size=.5) +
        coord_flip() +
        theme_bw() +
        theme(legend.position="none",plot.margin=unit(c(0,0.2,0,0),"lines"),
          axis.text=element_text(size=8),axis.title=element_text(size=10),
          plot.title=element_text(size=15, hjust=0.5),
          axis.text.x = element_text(angle = 0)) +
        ggtitle(sym1) + 
        labs(x="",y=paste0("Dependency score\n(",
          rev(c("CRISPR","C:R=80:20","C:R=60:40","C:R=40:60","C:R=20:80","shRNA"))[isolate(im())],
          ")"))
      ply <- plotly::ggplotly(pvio) #%>% config(displayModeBar=F)
      ndata <- length(ply$x$data)
      for(i in seq(ndata-1))ply$x$data[[i]]$hoverinfo <- "none"
        ply$x$data[[ndata]]$text <- ef.1$text
      ply
      return(ply)
    }
  })

  output$all_lineage <- renderPlotly({
    options(warn=0)
    validate(need(!is.null(dep.score.plot()),"Please type in a gene name first."))
    dep.score.plot()    
  })
}
