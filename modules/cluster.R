clusterUI <- function(id) {
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
        useShinyjs(),
        navContent(
          navPane(
            id = ns("pane_filters"),
            pickerInput(
              inputId = ns("eid2"),
              label = "Essential gene",
              choices = c("",eids.ess[["40:60 (default)"]]),
              selected = "",
              options = list(`live-search` = TRUE)
            ),
            setSliderColor(c("","","","Teal","Teal","Teal"),c(4,5)),
            tags$hr(),
            tags$h6("Tuning clusters (see Help for details):"),
            sliderTextInput(
                inputId = ns("cs2"),
                label = tags$h6("Cluster size"),
                choices = c("Small","Medium","Large"),
                selected ="Small",
                hide_min_max = TRUE,
                grid=TRUE
            ),
             sliderInput(
              inputId = ns("prob2"),
              label = tags$h6("Probability threshold"),
              min = 0,
              max = 1,
              step = 0.1,
              value = 0
            ),
            buttonInput(
              id = ns("clustButton"), 
              label = "Recompute clusters",
              tabindex="-1"
            ),
            tags$hr(),
            materialSwitch(
              inputId = ns("more_params"),
              label = "Tuning dependency score (for power users):",
              value=FALSE
            ),
            conditionalPanel(
              paste0("input[\'",ns("more_params"),"\']"),
              sliderTextInput(
                inputId = ns("mix_ratio"),
                label = tags$h6("Mixing ratio (shRNA:CRISPR)"), choices = mix.ratios,
                selected = "40:60 (default)",
                hide_min_max = FALSE,
                grid=TRUE
              )
            ),
            conditionalPanel(
              paste0("input[\'",ns("more_params"),"\']"),
              sliderTextInput(
                inputId = ns("threshold"),
                label = tags$h6("Efficacy threshold (percentile)"),
                choices = c("0.5","1 (default)","2.5","5","10","25")[-1],
                selected = "1 (default)",
                hide_min_max = FALSE,
                grid=TRUE
              )
            ),
            conditionalPanel(
              paste0("input[\'",ns("more_params"),"\']"),
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
            tags$ol(
              class = "pl-2",
              tags$li("Select an ",tags$b('Essential gene')," from the top-left dropdown menu"),
              tags$li("The cluster containing the query gene is highlighted in ",
                      tags$b('Efficacy/selectivity'),", ", tags$b('t-SNE plot'),", and listed in the ",tags$b('Clustered genes')),
              tags$li(tags$b('Connectivity')," shows the intra-cluster connectivity, ",
                      "where strongly correlated genes (Spearman > 0.1) are connected by edges."),
              tags$li(tags$b('Correlation')," shows the Spearman cor. coef. between the query gene and the other essential genes, ",
                      "grouped by clusters."),
              tags$li("Recompute clusters by changing ",tags$b("Cluster size"),
                      " and ",tags$b("Probability threshold"), "and clicking ",
                      tags$b("Recompute clusters"), "button (see the paper for details)"),
              tags$ul(
                tags$li(tags$b("Cluster size"), "controls the size and tightness of the clusters. ",
                  "'Large' clusters contain more genes per cluster while 'Small' cluster contains more strongly correlated genes"),
                tags$li(tags$b("Probability threshold"),
                  "filters genes based on the probability at which each gene belongs to the assigned cluster, computed by ECHODOTS algorithm.",
                  "A probability close to 1 indicates that the gene belongs to the assigned cluster with high confidence.",
                  "When the threshold of X (e.g., X = 0.5) is selected, genes whose probability is within [X,1] will be shown")
              ),
              tags$li("[Optional] Users can tune parameters that determines the Dependency scores, efficacy, and selectivity.",
                  "Please note that changing these parameters can significantly harm the consequence. ",
                  "e.g., the Dep score to which shRNA contributes more than CRISPR seems less accurate and we don't recommend its use for analyses.",
                  "Refer to the original paper for more information."),
              tags$ul(
                tags$li(tags$b("Mix Ratio"),"is the mixing ratio between shRNA and CRISPR, at which the unified Dependency score is computed via linear combination"),
                tags$li(tags$b("Efficacy threshold"), "determines efficacy and selectivity.",
                  "When this threshold is X (e.g., X = 1),",tags$b("efficacy")," is defined as the bottom X-th percentile, and",
                  tags$b("selectivity"),"as the difference between X-th and 100-Xth percentiles, of each gene's Dependency scores across all the cell lines"),
              )
            )
          )
        )
      )
    ),
    column(
      width = 4,
      card(
        header=h6("Efficacy/Selectivity plot"),
        div(
          plotlyOutput(
            outputId = ns('summary_efficacy_selectivity'), # plot 1
            height = "35vh"
          )
        ) 
      ) %>% yonder::margin(bottom = 2),
      card(
        header=h6("t-SNE plot - functional similarlity"),
        div(
          plotlyOutput(
            outputId = ns('tsne'), # plot 1
            height = "35vh"
          )
        )
      )
    ),
    column(
      width = 5,
      class = "function-tab",
      card(
        header = navInput(
          appearance = "tabs",
          id = ns("nav2"),
          choices = c("Clustered genes", "Connectivity","Correlation"),
          values = c("gene_set", "intra_cluster","inter_cluster"),
          selected = "gene_set"
        ),
        navContent(
          navPane(
            id = ns("pane_gene_set"),
            fade = FALSE,
            div(
              DTOutput(
                outputId = ns("gene_set"),
                height = "70vh"
              )
            ) 
          ),
          navPane(
            id = ns("pane_intra_cluster"),
            fade = FALSE,
            div(
              visNetworkOutput(
                outputId = ns("intra_cluster"),
                height = "75vh"
              ),
              conditionalPanel(
                condition = paste0("input[\'",ns("eid2"),"\']!=''"),
                downloadLink(ns("download_graph"),"Download GraphML file (for Cytoscape)")
              ) #%>% yonder::margin(all = 0)
            )
          ),
          navPane(
            id = ns("pane_inter_cluster"),
            fade = FALSE,
            div(
              plotlyOutput(
                outputId = ns("inter_cluster"),
                height = "75vh"
              )
            ) #%>% yonder::margin(all = 0)
          )
        )
      )
    )
  )  
}

clusterServer <- function(input, output, session, defs) {
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
      gene_set = showNavPane(ns("pane_gene_set")),
      intra_cluster = showNavPane(ns("pane_intra_cluster")),
      inter_cluster = showNavPane(ns("pane_inter_cluster")),
    )
  })


  observeEvent(defs$defButton(), {
    # cat(defs$threshold())
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
    im <- input$mix_ratio
    if(!is.null(im)){
      i.mix.ratio <- match(im,mix.ratios)
    }else{
      i.mix.ratio <- 4
    }
    return(i.mix.ratio)
  })

  it <- reactive({
    it <- input$threshold
    if(!is.null(it)){
      i.thres <- match(it,c("0.5","1 (default)","2.5","5","10","25"))
    }else{
      i.thres <- 3
    }
    return(i.thres)
  })

  df.0 <- reactive({
    input$defButton
    this.df <- dfs[[isolate(im())]][[isolate(it())]]
    this.mem <- mems[[isolate(im())]][-2]
    df.1 <- this.df %>% left_join(com,by="eid") %>%
      filter(is.ess) %>% 
      left_join(this.mem,by="eid") %>%
      mutate(x=unname(x)) %>%
      mutate(y=unname(y)) %>%
      mutate(text=paste0(sym,
          "<br />eff:",efficacy,", sel:",selectivity,
          # "<br />x:",x,", y:",y,
          "<br />clust:",mem1,", ",mem2,", ",mem3,
          "<br />prob:",lik1,", ",lik2,", ",lik3))
    return(df.1)
  })

  ## connection to Amazon
  icoef2 <- reactive({
    input$defButton
    if(is.null(input$eid2)||input$eid2==""){
      return(NULL)
    }else{
      coefs[[isolate(im())]][input$eid2,]
      # s3readRDS(paste0("coef_ess_19q3_v3/",isolate(im()),"/",input$eid2,".rds"),bucket="depmap")
    }
  })

  ief2 <- reactive({
    input$defButton
    if(is.null(input$eid2)||input$eid2==""){
      return(NULL)
    }else{
      p.scores[[isolate(im())]][input$eid2,]
      # s3readRDS(paste0("scores_all_19q3_v3/",isolate(im()),"/",input$eid2,".rds"),bucket="depmap")
    }
  })

  shinyInput <- function(FUN, query, id, ...) {
    inputs <- as.character(FUN(paste0(id, query), ...))
  }

  efficacy.selectivity <- reactive({
    input$defButton ## input$clustButton is handled separately

    eid2 <- isolate(input$eid2)
    cs2 <- isolate(input$cs2)
    prob2 <- isolate(input$prob2)

    cs.idx2 <- paste0("mem",match(cs2,cs))
    prob.idx2 <- paste0("lik",match(cs2,cs))

    n <- 50
    uniq.cols <- colorRampPalette(brewer.pal(9,"YlGnBu")[-(1:3)])(n+1)
    df <- df.0()

    xrng <- range(df$efficacy) + c(-1,1)*0.03
    yrng <- range(df$selectivity) + c(-1,1)*0.03

    this.cl <- ifelse(eid2=="","",as.character((df %>% filter(eid==eid2))[[cs.idx2]]))
    sym.all2 <- ifelse(eid2=="","",(df %>% filter(eid == eid2))$sym.all)

    df.1 <- df %>% 
      mutate(pri=uniq.cols[round(rank(df$selectivity)/nrow(df)*n)+1]) %>%
      mutate(is.grey=!!as.symbol(prob.idx2) < prob2 | efficacy >= thres.eff[[isolate(im())]]) %>%
      mutate_cond(is.grey,pri='rgba(220, 220, 220, 1)') %>%
      mutate_cond(is.grey,text="") %>%
      arrange(desc(is.grey))

    df.2 <- df %>% 
      filter(!!as.symbol(cs.idx2) == this.cl) %>%
      mutate(is.grey=!!as.symbol(prob.idx2) < prob2 | efficacy >= thres.eff[[isolate(im())]]) %>%
      arrange(desc(is.grey))

    n1 <- nrow(df.2)
    n2 <- sum(df.2$is.grey)

    if(n1!=0 & n1 > n2){
      cols.2 <- rep(c('rgba(153,153,153,1)','rgba(255,128,0,1)','rgba(255,25,0,1)'),c(n2,n1-n2-1,1))
      idx <- which(df.2$eid == eid2)
      o <- c(seq(nrow(df.2))[-idx],idx)
      df.2 <- df.2[o,]

      if(nrow(df.2)==1){
        df.2 <- rbind(df.2,df.2)
      }
      ttl <- ifelse(eid2=="","",paste0(sym.all2,", ",this.cl))
    }else{
      df.2 <- df.2[NULL,]
      cols.2 <- 'rgba(0,0,0,1)'
      ttl <- ""      
    }

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
      add_markers(
        data = df.1,
        x = ~efficacy, 
        y = ~selectivity,
        hoverinfo = 'text',
        text= ~text,
        marker = list(
          color= ~pri,
          size=2.5)
      ) %>%
      add_markers(
        data = df.2,
        x = ~efficacy, 
        y = ~selectivity,
        hoverinfo = 'text',
        text= ~text,
        marker = list(
          color= cols.2,
          size=6
        )
      ) %>%
      layout(title= ifelse(eid2=="","",paste0(sym.all2,", ",this.cl)),
        margin = list(
          l = 50,
          r = 10,
          b = 50,
          t = 50
        ),
        xaxis=list(title="efficacy",range = xrng, width = 0.5),
        yaxis=list(title="selecivity",range = yrng),
        showlegend = FALSE)
    ply
  })

  output$summary_efficacy_selectivity <- renderPlotly({
    options(warn=0)
    
    efficacy.selectivity()
  })

  tsne.plot <- reactive({
    input$defButton ## input$clustButton is handled separately

    eid2 <- isolate(input$eid2)
    cs2 <- isolate(input$cs2)
    prob2 <- isolate(input$prob2)

    cs.idx2 <- paste0("mem",match(cs2,cs))
    prob.idx2 <- paste0("lik",match(cs2,cs))

    n <- 50
    uniq.cols <- colorRampPalette(brewer.pal(9,"YlGnBu")[-(1:3)])(n+1)
    df <- df.0()

    this.cl <- ifelse(eid2=="","",as.character((df %>% filter(eid==eid2))[[cs.idx2]]))
    sym.all2 <- ifelse(eid2=="","",(df %>% filter(eid == eid2))$sym.all)

    df.1 <- df %>% 
      mutate(pri=uniq.cols[round(rank(df$selectivity)/nrow(df)*n)+1]) %>%
      mutate(is.grey=!!as.symbol(prob.idx2) < prob2 | efficacy >= thres.eff[[isolate(im())]]) %>%
      mutate_cond(is.grey,pri='rgba(220, 220, 220, 1)') %>%
      mutate_cond(is.grey,text="") %>%
      arrange(desc(is.grey))

    df.2 <- df %>% 
      filter(!!as.symbol(cs.idx2) == this.cl) %>% 
      mutate(is.grey=!!as.symbol(prob.idx2) < prob2 | efficacy >= thres.eff[[isolate(im())]]) %>%
      arrange(desc(is.grey))

    n1 <- nrow(df.2)
    n2 <- sum(df.2$is.grey)

    if(n1!=0 & n1 > n2){
      cols.2 <- rep(c('rgba(153,153,153,1)','rgba(255,128,0,1)','rgba(255,25,0,1)'),c(n2,n1-n2-1,1))
      idx <- which(df.2$eid == eid2)
      o <- c(seq(nrow(df.2))[-idx],idx)
      df.2 <- df.2[o,]

      if(nrow(df.2)==1){
        df.2 <- rbind(df.2,df.2)
      }
      ttl <- ifelse(eid2=="","",paste0(sym.all2,", ",this.cl))
    }else{
      df.2 <- df.2[NULL,]
      cols.2 <- 'rgba(0,0,0,1)'
      ttl <- ""      
    }

    ax <- list(
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      title = ""
    )

    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 30
    )
    ply <- plot_ly() %>%
      add_lines(
        x= rep(thres.eff[[isolate(im())]],2), 
        y=c(-1,4),
        name = "thres",
        line = list(
          color = 'rgba(0,0,0,1)',
          shape = "linear",
          width = 0.5
        )
      ) %>%
      add_markers(
        data = df.1,
        x = ~x, 
        y = ~y,
        hoverinfo = 'text',
        text= ~text,
        marker = list(
          color= ~pri,
          size=2.5)
      ) %>%
      add_markers(
        data = df.2,
        x = ~x, 
        y = ~y,
        hoverinfo = 'text',
        text= ~text,
        marker = list(
          color= cols.2,
          size=6
        )
      ) %>%
      layout(title= ttl,
        showlegend = FALSE,
        xaxis=ax,
        yaxis=ax,
        margin =m
      )
    # stop(paste(sapply(ply$x$visdat,function(x)nrow(x())),collapse=" "))

    ply
  })

  output$tsne <- renderPlotly({
    options(warn=0)
    tsne.plot()    
  })

  # toListen2 <- reactive(
  #   list(input$eid2,input$clustButton)
  # )

  observeEvent(input$eid2,{
    options(warn=0)
    eid2 <- input$eid2
    cs2 <- input$cs2
    prob2 <- input$prob2

    cs.idx2 <- paste0("mem",match(cs2,cs))
    prob.idx2 <- paste0("lik",match(cs2,cs))

    n <- 50
    uniq.cols <- colorRampPalette(brewer.pal(9,"YlGnBu")[-(1:3)])(n+1)

    df <- df.0()

    if(eid2!=""){
      this.cl <- as.character((df %>% filter(eid==eid2))[[cs.idx2]])
      sym.all2 <- (df %>% filter(eid == eid2))$sym.all
    }else{
      this.cl <- sym.all2 <- ""
    }

    df <- df %>% mutate(pri=uniq.cols[round(rank(df$selectivity)/nrow(df)*n)+1])

    df.2 <- df %>% 
      filter(!!as.symbol(cs.idx2) == this.cl) %>% 
      mutate(is.grey=!!as.symbol(prob.idx2) < prob2 | efficacy >= thres.eff[[isolate(im())]]) %>%
      arrange(desc(is.grey))
    n1 <- nrow(df.2)
    n2 <- sum(df.2$is.grey)

    if(n1!=0 & n1 > n2){
      cols.2 <- rep(c('rgba(153,153,153,1)','rgba(255,128,0,1)','rgba(255,25,0,1)'),c(n2,n1-n2-1,1))
      idx <- which(df.2$eid == eid2)
      o <- c(seq(nrow(df.2))[-idx],idx)
      df.2 <- df.2[o,]

      if(nrow(df.2)==1){
        df.2 <- rbind(df.2,df.2)
      }
      ttl <- ifelse(eid2=="","",paste0(sym.all2,", ",this.cl))
    }else{
      df.2 <- df.2[NULL,]
      cols.2 <- 'rgba(0,0,0,1)'
      ttl <- ""      
    }
    xrng <- range(df$efficacy) + c(-1,1)*0.03

    ## efficacy/selectivity plot
    plotlyProxy('summary_efficacy_selectivity') %>%
      plotlyProxyInvoke("relayout",
        list(title = ttl,
          xaxis=list(range = xrng, width = 0.5,title="efficacy"))) %>%
      plotlyProxyInvoke("restyle", 
        list(x = list(df.2$efficacy), 
          y = list(df.2$selectivity),
          text = list(df.2$text),
          marker=list(color=cols.2,size=6)),
        list(2))

    ## tsne plot
    plotlyProxy('tsne') %>%
      plotlyProxyInvoke("relayout",list(title = ttl)) %>%
      plotlyProxyInvoke("restyle", 
        list(x = list(df.2$x), 
          y = list(df.2$y),
          text = list(df.2$text),
          marker=list(color=cols.2,size=6)),
        list(2))
  },ignoreInit=TRUE)

  # observeEvent(cButtons(),{
  observeEvent(input$clustButton,{  ## defButton
    options(warn=0)
    eid2 <- input$eid2
    cs2 <- input$cs2
    prob2 <- input$prob2

    cs.idx2 <- paste0("mem",match(cs2,cs))
    prob.idx2 <- paste0("lik",match(cs2,cs))

    n <- 50
    uniq.cols <- colorRampPalette(brewer.pal(9,"YlGnBu")[-(1:3)])(n+1)

    df <- df.0()
    xrng <- range(df$efficacy) + c(-1,1)*0.03

    df.1 <- df %>% 
      mutate(pri=uniq.cols[round(rank(df$selectivity)/nrow(df)*n)+1]) %>%
      mutate(is.grey=!!as.symbol(prob.idx2) < prob2 | efficacy >= thres.eff[[isolate(im())]]) %>%
      mutate_cond(is.grey,pri='rgba(220, 220, 220, 1)') %>%
      mutate_cond(is.grey,text="") %>%
      arrange(desc(is.grey))

    eff.sel.proxy <- plotlyProxy('summary_efficacy_selectivity') %>%
      plotlyProxyInvoke("relayout",
        list(xaxis=list(range = xrng, width = 0.5,title="efficacy"))) %>%
      plotlyProxyInvoke("restyle", 
        list(x = list(df.1$efficacy), 
          y = list(df.1$selectivity),
          text = list(df.1$text),
          marker=list(color= df.1$pri,size=2.5)),
        list(1)) 

    tsne.proxy <- plotlyProxy('tsne') %>%
      plotlyProxyInvoke("restyle", 
        list(x = list(df.1$x), 
          y = list(df.1$y),
          text = list(df.1$text),
          marker=list(color= df.1$pri,size=2.5)),
        list(1)) 

    if(eid2!=""){
      this.cl <- ifelse(eid2=="","",as.character((df %>% filter(eid==eid2))[[cs.idx2]]))
      sym.all2 <- ifelse(eid2=="","",(df %>% filter(eid == eid2))$sym.all)

      ttl <- paste0(sym.all2,", ",this.cl)

      df.2 <- df %>% 
        filter(!!as.symbol(cs.idx2) == this.cl) %>% 
        mutate(is.grey=!!as.symbol(prob.idx2) < prob2 | efficacy >= thres.eff[[isolate(im())]]) %>%
        arrange(desc(is.grey))
      n1 <- nrow(df.2)
      n2 <- sum(df.2$is.grey)

      if(n1!=0 & n1 > n2){
        cols.2 <- rep(c('rgba(153,153,153,1)','rgba(255,128,0,1)','rgba(255,25,0,1)'),c(n2,n1-n2-1,1))
        idx <- which(df.2$eid == eid2)
        o <- c(seq(nrow(df.2))[-idx],idx)
        df.2 <- df.2[o,]

        if(nrow(df.2)==1){
          df.2 <- rbind(df.2,df.2)
        }
        ttl <- ifelse(eid2=="","",paste0(sym.all2,", ",this.cl))
      }else{
        df.2 <- df.2[NULL,]
        cols.2 <- 'rgba(0,0,0,1)'
        ttl <- ""      
      }

      eff.sel.proxy %>% 
        plotlyProxyInvoke("relayout",list(title = ttl)) %>%
        plotlyProxyInvoke("restyle", 
          list(x = list(df.2$efficacy), 
            y = list(df.2$selectivity),
            text = list(df.2$text),
            marker=list(color=cols.2,size=6)),
          list(2))

      tsne.proxy %>%
        plotlyProxyInvoke("relayout",list(title = ttl)) %>%
        plotlyProxyInvoke("restyle", 
          list(x = list(df.2$x), 
            y = list(df.2$y),
            text = list(df.2$text),
            marker=list(color=cols.2,size=6)),
          list(2)) 

    }
  },ignoreInit=TRUE)

  ess.gene.set <- reactive({
    # input$defButton
    toListen3()
    # if(any(!is.null(cButtons()))){cat(paste(cButtons(),collapse=" "))}

    eid2 <- input$eid2
    cs2 <- isolate(input$cs2)
    prob2 <- isolate(input$prob2)
    df.1 <- df.0()

    prob.idx2 <- paste0("lik",match(cs2,cs))
    cs.idx2 <- paste0("mem",match(cs2,cs))

    this.cl <- ifelse(eid2=="","",as.character((df.1 %>% filter(eid==eid2))[[cs.idx2]]))

    eids <- (df.1 %>% 
      filter(!!as.symbol(prob.idx2) >= prob2) %>%
      filter(efficacy < thres.eff[[isolate(im())]]) %>%
      filter(!!as.symbol(cs.idx2) %in% this.cl))$eid

    dt <- df.1 %>% filter(eid %in% eids) %>%
      select(sym,eid,efficacy, selectivity,mem1,mem2,mem3) %>% 
      arrange(efficacy,desc(selectivity)) %>%
      mutate(sym = paste0('<a href="https://www.genecards.org/cgi-bin/carddisp.pl?gene=',sym,'" target="_blank">',sym,'</a>')) %>%
      `colnames<-`(c("Gene","eid","Eff","Sel","S","M","L")) %>%
      mutate(selected = as.numeric(eid == eid2)) %>%
      mutate(S=sapply(S,function(i){
        if(is.na(i)||i=="NA"){
          return("none")
        }else{
          button <- ns("c1_button")
          shinyInput(linkInput, i, 'button_', 
              label = i,
              onclick = paste0('Shiny.setInputValue(\"',button,'\",  this.id)'))
        }
      })) %>%
      mutate(M=sapply(M,function(i){
        if(is.na(i)||i=="NA"){
          return("none")
        }else{
          button <- ns("c2_button")
          shinyInput(linkInput, i, 'button_', 
              label = i,
              onclick = paste0('Shiny.setInputValue(\"',button,'\",  this.id)'))
        }
      })) %>%
      mutate(L=sapply(L,function(i){
        if(is.na(i)||i=="NA"){
          return("none")
        }else{
          button <- ns("c3_button")
          shinyInput(linkInput, i, 'button_', 
              label = i,
              onclick = paste0('Shiny.setInputValue(\"',button,'\",  this.id)'))
        }
      })) %>%
      datatable(extensions = 'Scroller', 
        options = list(
          columnDefs = list(
            list(targets = c(2,8), visible = FALSE),
            list(className = 'dt-center', targets = 1:7)
            ),
  　　        rowReorder = TRUE,
          deferRender = TRUE,
          scrollY = "70vh",
          pageLength = 1000,
          scroller = TRUE,
          # class="compact",
          # scrollCollapse = TRUE,
          # order = list(list(3, 'asc'), list(4, 'desc')),
          paging = TRUE,
          sDom  = '<"top">lt<"bottom">ip'),
        selection=list(mode="none"),
        editable=FALSE, escape=FALSE,
        callback = JS("setTimeout(function() { table.draw(true); }, 500);")) %>%
        formatStyle(
        8,
        target = 'row',
        backgroundColor = styleEqual(c(1,0), c('#ffcccb', NA))
      )
    return(dt)
  })

  toListen3 <- reactive(
    list(input$clustButton,input$defButton)
  )

  observeEvent(toListen3(),{
    eid2 <- input$eid2
    cs2 <- input$cs2
    prob2 <- input$prob2
    df.1 <- df.0()

    prob.idx2 <- paste0("lik",match(cs2,cs))
    cs.idx2 <- paste0("mem",match(cs2,cs))

    this.cl <- ifelse(eid2=="","",as.character((df.1 %>% filter(eid==eid2))[[cs.idx2]]))

    dt <- df.1 %>% 
      filter(!!as.symbol(prob.idx2) >= prob2 & efficacy < thres.eff[[im()]]) %>%
      arrange(efficacy,desc(selectivity))

    this.eids.ess <- eids.ess[[input$mix_ratio]]
    eids <- (dt %>% filter(!!as.symbol(cs.idx2) == this.cl))$eid

    if(length(eids)==0){ # No genes in the cluster satisfy prob and eff-thres
      this.eids.ess <- this.eids.ess[this.eids.ess %in% dt$eid]
      updatePickerInput(session,
        inputId = "eid2",
        choices = c("",this.eids.ess),
        selected = "")
    }else{
      this.eids.ess <- this.eids.ess[this.eids.ess %in% dt$eid]
      if(eid2 %in% eids){ # eid2 satsifies prob and eff-thres
        input.eid <- eid2
      }else{ # NOT eid2 BUT other genes in the cluster satsify prob and eff-thres
        input.eid <- eids[1]
      }
      updatePickerInput(session,
        inputId = "eid2",
        choices = c("",this.eids.ess),
        selected = input.eid
      )
    }
  }, ignoreInit = TRUE)

  ## buttons
  observeEvent(input$c1_button,{
    eid2 <- input$eid2
    this.cl <- sub("button_","",input$c1_button)

    updateSliderTextInput(session,
      inputId = "cs2",
      selected = "Small"
    )
    eids <- (df.0() %>% filter(mem1 == this.cl))$eid

    if(!any(eid2 %in% eids)){
      dt <- df.0() %>% 
        filter(eid %in% eids) %>%
        select(eid,efficacy, selectivity) %>%
        arrange(efficacy,desc(selectivity))
      eid2 <- dt$eid[1]
      updatePickerInput(session,
        inputId = "eid2",
        selected = eid2)
    }

    updateSliderInput(session,
      inputId = "prob2",
      value = 0
    )

    click("clustButton")

  }, ignoreInit = TRUE)

  observeEvent(input$c2_button,{
    eid2 <- input$eid2
    this.cl <- sub("button_","",input$c2_button)

    updateSliderTextInput(session,
      inputId = "cs2",
      selected = "Medium"
    )
    eids <- (df.0() %>% filter(mem2 == this.cl))$eid

    if(!any(eid2 %in% eids)){
      dt <- df.0() %>% 
        filter(eid %in% eids) %>%
        select(eid,efficacy, selectivity) %>%
        arrange(efficacy,desc(selectivity))
      eid2 <- dt$eid[1]
      updatePickerInput(session,
        inputId = "eid2",
        selected = eid2)
    }

    updateSliderInput(session,
      inputId = "prob2",
      value = 0
    )

    click("clustButton")

  }, ignoreInit = TRUE)

  observeEvent(input$c3_button,{
    eid2 <- input$eid2
    this.cl <- sub("button_","",input$c3_button)

    updateSliderTextInput(session,
      inputId = "cs2",
      selected = "Large"
    )
    eids <- (df.0() %>% filter(mem3 == this.cl))$eid

    if(!any(eid2 %in% eids)){
      dt <- df.0() %>% 
        filter(eid %in% eids) %>%
        select(eid,efficacy, selectivity) %>%
        arrange(efficacy,desc(selectivity))
      eid2 <- dt$eid[1]
      updatePickerInput(session,
        inputId = "eid2",
        selected = eid2)
    }

    updateSliderInput(session,
      inputId = "prob2",
      value = 0
    )

    click("clustButton")

  }, ignoreInit = TRUE) 

  output$gene_set <- renderDT({
    options(warn=0)
    validate(
        need(input$eid2!="","Please select an essential gene and a cluster size (default:Small).\nGenes in the cluster will be shown here.")
    )
    ess.gene.set()
  })

  intra.cluster.net <- reactive({
    options(warn=0)

    # input$defButton
    toListen3()
    # cButtons()
    eid2 <- input$eid2
    cs2 <- isolate(input$cs2)
    prob2 <- isolate(input$prob2)
    df.1 <- df.0()

    prob.idx2 <- paste0("lik",match(cs2,cs))
    cs.idx2 <- paste0("mem",match(cs2,cs))

    this.cl <- ifelse(eid2=="","",as.character((df.1 %>% filter(eid==eid2))[[cs.idx2]]))

    eids <- (df.1 %>% 
      filter(!!as.symbol(prob.idx2) >= prob2) %>%
      filter(efficacy < thres.eff[[isolate(im())]]) %>%
      filter(!!as.symbol(cs.idx2) %in% this.cl))$eid

    s3load(paste0("graph_19q3_v3/",isolate(im()),"/",this.cl,".rda"),bucket="depmap") # nodes,edges

    nodes <- nodes %>% filter(id %in% eids)    
    edges <- edges %>% filter(from %in% eids & to %in% eids)

    return(list(nodes=nodes,edges=edges))
  })

  output$intra_cluster <- renderVisNetwork({
    validate(
      need(input$eid2!="",
        "Please select an essential gene and a cluster size (default:Small).\nSpearman correlation coefficients between genes (> 0.1) within the cluster will be shown here."
      )
    )

    g <- intra.cluster.net()
    nodes <- g$nodes
    edges <- g$edges

    visNetwork(nodes, edges,width = "80%") %>%
      visIgraphLayout(randomSeed=123) %>%
      visNodes(
        shape = "dot",
        color = list(
          background = "#0085AF",
          border = "#013848",
          highlight = "#FF8000"
        ),
        shadow = list(enabled = TRUE, size = 10)
      ) %>%
      visEdges(
        shadow = FALSE,
        color = list(color = "#7BA3C4", highlight = "#C62F4B")
      ) %>%
      # visLayout(randomSeed = 123) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = F),
             nodesIdSelection = list(selected = input$eid2),
                 selectedBy = "group") %>% 
      visInteraction(dragView=FALSE,zoomView=FALSE) 
  })

  output$download_graph <- downloadHandler(
    filename = function(){
      paste0(input$eid2,"_",input$cs2,".graphml")
    },
    content = function(file){
      g <- intra.cluster.net()
      nodes <- g$nodes
      edges <- g$edges

      uniq.nodes <- nodes$label
      names(uniq.nodes) <- nodes$id

      nodes.1 <- nodes %>% 
        mutate(EntrezID=id) %>%
        mutate(id = label) %>%
        mutate(Small = sub(".+<br />(S[0-9]+),.+","\\1",title)) %>%
        mutate(Medium = sub(".+<br />.+(M[0-9]+),.+","\\1",title)) %>%
        mutate(Large = sub(".+<br />.+(L[0-9]+)","\\1",title)) %>%
        select(id,EntrezID,Small,Medium,Large)

      edges.1 <- edges %>%
        mutate(from = uniq.nodes[as.character(from)]) %>%
        mutate(to = uniq.nodes[as.character(to)]) %>%      
        mutate(Sp.corr = sub(".+<br />","",title)) %>%
        select(from,to,width,Sp.corr)

      ig <-graph_from_data_frame(d=edges.1, vertices=nodes.1, directed=F)
      write_graph(ig,file,format="graphml")
    }
  )


  inter.cluster.net <- reactive({
    options(warn=0)

    # input$defButton    
    # cButtons()    
    toListen3()

    eid2 <- input$eid2
    cs2 <- isolate(input$cs2)
    prob2 <- isolate(input$prob2)
    coef2 <- isolate(icoef2())
    df.1 <- df.0()

    prob.idx2 <- paste0("lik",match(cs2,cs))
    cs.idx2 <- paste0("mem",match(cs2,cs))

    sym.all2 <- ifelse(eid2=="","",(df.1 %>% filter(eid == eid2))$sym.all)

    sym2 <- (df.1 %>% filter(eid == eid2))$sym
    this.cl <- ifelse(eid2=="","",as.character((df.1 %>% filter(eid==eid2))[[cs.idx2]]))
    eids <- (df.1 %>% 
      filter(!!as.symbol(prob.idx2) >= prob2) %>%
      filter(efficacy < thres.eff[[isolate(im())]]))$eid

    coef.df <- data.frame(coef=coef2,
        eid=names(coef2),
        stringsAsFactors=FALSE) %>%
      left_join(df.1,by="eid") %>%
      filter(eid != eid2 & eid %in% eids)

    mem <- as.character(coef.df[[cs.idx2]])
    mem[is.na(mem)] <- "none"

    med.mem <- tapply(coef.df$coef,mem,mean)
    o <- order(med.mem,decreasing=F)
    clust.levs <- names(med.mem)[o]
    clust.levs <- c(clust.levs[clust.levs != this.cl],this.cl)

    coef.df <- coef.df %>% mutate(mem=factor(mem,levels=clust.levs)) %>%
      mutate(text=paste0(sym,"(",mem,") - ",sym2,"(",this.cl,")<br />",coef))

    suppressWarnings(
      sum.coef.df <- coef.df %>% 
        group_by(mem) %>% 
        summarize(maxs=max(coef),mins=min(coef),mids=mean(coef)) %>%
        arrange(mids) %>%
        mutate(mem=factor(mem,levels=clust.levs))
    )

    suppressWarnings({ # doesn't work..
      pvio <- ggplot(sum.coef.df, aes(x=mem,y=mids,color=mem)) +
        geom_hline(yintercept=0, col="grey50") +
        geom_linerange(aes(ymin=mins,ymax=maxs))+
        geom_jitter(data=coef.df,aes(x=mem,y=coef),height=0, width=0.1, size=.3,
          color="black")+
        coord_flip() +
        theme_bw() +
        ggtitle(paste0(sym.all2)) + 
        labs(
          x=paste0("Essential genes, grouped by clusters (",cs2," size)"),
          y=paste0("Spearman correlation against ",sym2,"\n(",
            c("shRNA","R:C=80:20","R:C=60:40","R:C=40:60","R:C=20:80","CRISPR")[isolate(im())],
          ")")
        ) +
        theme(legend.position="none",plot.margin=unit(c(0,0.2,0,0.5),"lines"),
          axis.text=element_text(size=10),axis.title=element_text(size=10),
          plot.title=element_text(size=15, hjust=0.5),
          axis.text.x = element_text(angle = 0),
          axis.title.x = element_text(hjust=-20))          

      ply <- ggplotly(pvio) # %>% config(displayModeBar=F)
      ndata <- length(ply$x$data)

      for(i in seq(ndata-1)){
        ply$x$data[[i]]$hoverinfo <- "none"
      }

      ply$x$data[[ndata]]$text <- coef.df$text
    })

    return(ply)
  })

  output$inter_cluster <- renderPlotly({
    validate(
      need(input$eid2!="",
        "Please select an essential gene and a cluster size (default:Small).\nSpearman correlation coefficients across the essential genes will be shown here."
      )
    )
    inter.cluster.net()
  })

}

