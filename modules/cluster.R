clusterUI <- function(id) {
  ns <- NS(id)
  
  columns(
    column(
      width = 3,
      # height = "100%",
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
              label = "Essential gene",
              class = "bs-select",
              input = shinyWidgets::pickerInput(
                inputId = ns("eid2"),
                choices = c("",eids.ess),
                selected = "",
                options = list(`live-search` = TRUE)
              )
            ),
            formGroup(
              label = "Cluster size",
              input = shinyWidgets::sliderTextInput(
                inputId = ns("cs2"),
                label = "",
                choices = c("Small","Medium","Large"),
                selected ="Small",
                hide_min_max = TRUE,
                grid=TRUE
              )
            )
          ),
          navPane(
            id = ns("pane_instructions"),
            fade = FALSE,
            tags$ol(
              class = "pl-2",
              tags$li("Select an ",tags$b('Essential gene')," from the dropdown menu in the top-left",
                      "and a ",tags$b("Cluster size")," (set as \"Small\" by default)"),
              tags$li("The app will find the cluster containing the selected gene and show them in the ",
                      tags$b('Efficacy/selectivity and t-SNE plots'),"and the",tags$b('Clustered genes')," tab"),
              tags$ul(
                # tags$li(tags$b('Efficacy/selectivity plot'),": the same as the Gene essentiality app, except this one only shows the essential genes"),
                tags$li(tags$b('t-SNE plot'),": each point represent one gene; closer points have similar dependency scores across 423 cells"),
                tags$li("By clicking a different cluster (S-,M-,L-) in the ",tags$b('Clustered genes') ,"tab, you can find genes that belong to that cluster"),
              ),
              tags$li(tags$b('Connectivity')," tab shows the connectivity between the genes in the cluster, ",
                      "where strongly correlated genes (Spearman > 0.1) are connected by edges"),
              tags$li(tags$b('Correlation')," tab shows the Spearman correlation coefficients between the selected gene and the other essential genes")
            )
          )
        )
      )
    ),
    column(
      width = 4,
      # class = "two-panels",      
      card(
        header=h6("Efficacy/Selectivity plot (2,492 genes)"),
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
              DT::dataTableOutput(
                outputId = ns("gene_set"),
                height = "70vh"
              )
            ) 
          ),
          navPane(
            id = ns("pane_intra_cluster"),
            fade = FALSE,
            div(
              visNetwork::visNetworkOutput(
                outputId = ns("intra_cluster"),
                height = "75vh"
              )
            ) #%>% yonder::margin(all = 0)
          ),
          navPane(
            id = ns("pane_inter_cluster"),
            fade = FALSE,
            div(
              plotly::plotlyOutput(
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

clusterServer <- function(input, output, session) {
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

  toListen <- reactive(
    list(input$eid2,input$cs2)
  )

  ## connection to Amazon
  icoef2 <- reactive({
    if(is.null(input$eid2)||input$eid2==""){
      return(NULL)
    }else{
      # s3readRDS(paste0("coef_ess_19q3/",input$eid2,".rds"),bucket="depmap")
      coef[input$eid2,]
    }
  })

  ief2 <- reactive({
    if(is.null(input$eid2)||input$eid2==""){
      return(NULL)
    }else{
      # s3readRDS(paste0("scores_all_19q3/",input$eid2,".rds"),bucket="depmap")
      p.scores[input$eid2,]
    }
  })

  shinyInput <- function(FUN, query, id, ...) {
    inputs <- as.character(FUN(paste0(id, query), ...))
  }

  sum.eff.sel <- reactive({
    eid2 <- input$eid2
    cs2 <- input$cs2

    df.0 <- df.0 %>% mutate(pri=selectivity)
    col <- scale_colour_gradientn(colours = brewer.pal(9,"YlGnBu")[4:9])

    p <- ggplot(df.0,aes(text=text)) +
      geom_vline(xintercept=thres.eff, col="grey70") +
      geom_hline(yintercept=c(0,1),col=c("grey50","grey70")) + 
      geom_point(data = df.0,aes(efficacy, selectivity, col=pri), size = .5) +
      col +
      # geom_point(aes(efficacy, selectivity, col=pri), size = .5) + col +
      theme_bw() +
        theme(legend.position="none",plot.margin=unit(c(0,0,0,0),"lines"),
          axis.text=element_text(size=10),axis.title=element_text(size=10),
          plot.title=element_text(size=10),
          axis.text.x = element_text(angle = 0))

    if(eid2==""){
      p <- p + 
        geom_point(data=dummy[1,],aes(efficacy,selectivity),col=NA,size=1.2) + # 6th trace, lik < lik2
        geom_point(data=dummy[2,],aes(efficacy,selectivity),col=NA,size=1.2) # 7th trace, lik >= lik2
    }else{
      cs.idx2 <- paste0("mem",match(cs2,cs))
      cl2 <- (df.0 %>% filter(eid==eid2))[[cs.idx2]]
      if(cl2 == "NA"){
        eids <- eid2
      }else{
        eids <- df.0[df.0[[cs.idx2]] %in% cl2,"eid"]
      }

      df.2 <- df.0  %>% filter(eid %in% eids)
      df.3 <- df.2 %>% filter(eid == eid2)

      # warning(nrow(df.2))
      # warning(nrow(df.3))

      if(nrow(df.2)>0){
        p <- p +
          geom_point(data=df.2,aes(efficacy,selectivity),col='rgba(255,128,0,1)',
            size=1.2) # 6th trace, lik < lik2
      }
      if(nrow(df.3)>0){
        p <- p +
          geom_point(data=df.3,aes(efficacy,selectivity),col='rgba(255,25,0,1)',
            size=1.2) # 7th trace, lik >= lik2
      }
    }   

    ply <- ggplotly(p,tooltip="text")
    sapply(ply$x$data,function(x)length(x$x))
    for(i in 1:3)ply$x$data[[i]]$hoverinfo <- "none"
    ply
  })

  output$summary_efficacy_selectivity <- plotly::renderPlotly({
    options(warn=0)
    sum.eff.sel()
  })

  observeEvent(toListen(),{
    eid2 <- input$eid2
    cs2 <- input$cs2

    if(eid2==""){
      return(NULL)
    }else{
      cs.idx2 <- paste0("mem",match(cs2,cs))
      cl2 <- (df.0 %>% filter(eid==eid2))[[cs.idx2]]
      if(cl2 == "NA"){
        eids <- eid2
      }else{
        eids <- df.0[df.0[[cs.idx2]] %in% cl2,"eid"]
      }

      grep.eids <- which(df.0$eid %in% eids)

      if(any(grep.eids)){
        df.1 <- df.0[grep.eids,] %>%
          mutate(text=paste0(sym,
                "<br />eff:",efficacy,", sel:",selectivity,
                "<br />clust:",mem1,",",mem2,",",mem3))


        df.2 <- df.1[rep(match(eid2,eids),2),]

        plotlyProxy('summary_efficacy_selectivity') %>%
              plotlyProxyInvoke("restyle", 
                list(x = list(df.1$efficacy), 
                  y = list(df.1$selectivity),
                  text = list(df.1$text),
                  marker=list(color='rgba(255,128,0,1)',size=6)),
                list(4)) %>%
              plotlyProxyInvoke("restyle", 
                list(x = list(df.2$efficacy), 
                  y = list(df.2$selectivity),
                  text = list(df.2$text),
                  marker=list(color='rgba(255,25,0,1)',size=6)),
                list(5))
        }
    }
  },ignoreInit=TRUE)

  tsne.plot <- reactive({
    options(warn=0)

    eid2 <- input$eid2
    cs2 <- input$cs2

    df.0 <- df.0 %>% arrange(selectivity) %>% mutate(pri=selectivity)
    col <- scale_colour_gradientn(colours = brewer.pal(9,"YlGnBu")[4:9])

    p <- ggplot(data=df.0,aes(text=text)) +
        geom_point(aes(x,y,col=pri),size=.5) + col +
      theme_bw() +
        theme(legend.position="none",
        plot.margin=unit(c(0,0.5,0,1),"lines"),
        axis.text=element_blank(),
        axis.title=element_blank(),
        plot.title=element_blank(),
        axis.ticks=element_blank())

    if(eid2==""){
      p <- p + 
          geom_point(data=dummy[1,], aes(x,y), col=NA,size=1) +
          geom_point(data=dummy[2,],aes(x,y), col=NA,size=1)
    }else{
      cs.idx2 <- paste0("mem",match(cs2,cs))
      cl2 <- (df.0 %>% filter(eid==eid2))[[cs.idx2]]
      if(cl2 == "NA"){

        eids <- eid2
      }else{
        eids <- df.0[df.0[[cs.idx2]] %in% cl2,"eid"]
      }


      df.2 <- df.0  %>% filter(eid %in% eids)
      df.3 <- df.2 %>% filter(eid == eid2)

      if(nrow(df.2)>0){
        p <- p +
          geom_point(data=df.2,aes(x,y),col="rgba(255,128,0,1)",size=1.2) # 6th trace, lik < lik2
      }
      if(nrow(df.3)>0){
        p <- p +
          geom_point(data=df.3,aes(x,y),col="rgba(255,25,0,1)",size=1.2) # 7th trace, lik >= lik2
      }
    }

    ply <- ggplotly(p,tooltip="text")
    ply
  })

  output$tsne <- plotly::renderPlotly({
    tsne.plot()    
  })

  observeEvent(toListen(),{
    eid2 <- input$eid2
    cs2 <- input$cs2

    if(eid2==""){
      return(NULL)
    }else{
      cs.idx2 <- paste0("mem",match(cs2,cs))
      cl2 <- (df.0 %>% filter(eid==eid2))[[cs.idx2]]
      if(cl2 == "NA"){
        eids <- eid2
      }else{
        eids <- df.0[df.0[[cs.idx2]] %in% cl2,"eid"]
      }

      grep.eids <- which(df.0$eid %in% eids)

      # warning(paste(gen.path2,eid2=="",lik2,collapse=" "))

      if(any(grep.eids)){
        df.1 <- df.0[grep.eids,] %>%
          mutate(text=paste0(sym,
                "<br />eff:",efficacy,", sel:",selectivity,
                "<br />clust:",mem1,",",mem2,",",mem3))

        df.2 <- df.1[rep(match(eid2,eids),2),]

        plotlyProxy('tsne') %>%
              plotlyProxyInvoke("restyle", 
                list(x = list(df.1$x), 
                  y = list(df.1$y),
                  text = list(df.1$text),
                  marker=list(color='rgba(255,128,0,1)',size=6)),
                list(1)) %>%
              plotlyProxyInvoke("restyle", 
                list(x = list(df.2$x), 
                  y = list(df.2$y),
                  text = list(df.2$text),
                  marker=list(color='rgba(255,25,0,1)',size=6)),
                list(2))
        }
    }
  },ignoreInit=TRUE)

  ess.gene.set <- reactive({
    eid2 <- input$eid2
    cs2 <- input$cs2

    this.mem <- paste0("mem",match(cs2,cs))
    this.cl <- (df.0 %>% filter(eid==eid2))[[this.mem]]
    if(is.na(this.cl)||this.cl == "NA"){
      eids <- eid2
    }else{
      eids <- df.0[df.0[[this.mem]] %in% this.cl,"eid"]
    }

    dt <- df.0 %>% filter(eid %in% eids) %>%
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
          pageLength = 500,
          scroller = TRUE,
          order = list(list(3, 'asc'), list(4, 'desc')),
          # paging = FALSE,
          sDom  = '<"top">lt<"bottom">ip'),
        selection=list(mode="none"),
        editable=FALSE, escape=FALSE) %>%
        formatStyle(
        8,
        target = 'row',
        backgroundColor = styleEqual(c(1,0), c('#ffcccb', NA))
      )
    return(dt)
  })

  ## buttons
  observeEvent(input$c1_button,{
    # stop(input$c1_button)
    eid2 <- input$eid2
    cl2 <- sub("button_","",input$c1_button)

    updateSliderTextInput(session,
      inputId = "cs2",
      selected = "Small"
    )
    eids <- df.0$eid[df.0$mem1==cl2]
    # stop(paste(eids,collapse=";"))

    if(!any(eid2 %in% eids)){
      dt <- df.0 %>% filter(eid %in% eids) %>%
        select(eid,efficacy, selectivity) %>%
        arrange(efficacy,desc(selectivity))
      eid2 <- dt$eid[1]
      updatePickerInput(session,
        inputId = "eid2",
        selected = eid2)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$c2_button,{
    # stop(input$c2_button)

    eid2 <- input$eid2
    cl2 <- sub("button_","",input$c2_button)

    updateSliderTextInput(session,
      inputId = "cs2",
      selected = "Medium"
    )
    eids <- df.0$eid[df.0$mem2==cl2]
    # stop(paste(eids,collapse=";"))

    if(!any(eid2 %in% eids)){
      dt <- df.0 %>% filter(eid %in% eids) %>%
        select(eid,efficacy, selectivity) %>%
        arrange(efficacy,desc(selectivity))
      eid2 <- dt$eid[1]
      updatePickerInput(session,
        inputId = "eid2",
        selected = eid2)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$c3_button,{
    # stop(input$c3_button)
    eid2 <- input$eid2
    cl2 <- sub("button_","",input$c3_button)

    updateSliderTextInput(session,
      inputId = "cs2",
      selected = "Large"
    )
    eids <- df.0$eid[df.0$mem3==cl2]
    # stop(paste(eids,collapse=";"))

    if(!any(eid2 %in% eids)){
      dt <- df.0 %>% filter(eid %in% eids) %>%
        select(eid,efficacy, selectivity) %>%
        arrange(efficacy,desc(selectivity))
      eid2 <- dt$eid[1]
      updatePickerInput(session,
        inputId = "eid2",
        selected = eid2)
    }
  }, ignoreInit = TRUE) 

  output$gene_set <- renderDT({
    options(warn=0)
    validate(
        need(input$eid2!="","Please choose an essential gene and a cluster size (default:Small).\nGenes in the cluster will be shown here.")
    )
    ess.gene.set()
  })

  intra.gene.net <- reactive({
    options(warn=0)

    if(0){
      eid2 <- "2879" # gpx4
      eid2 <- "598" # bcl2l1
      eid2 <- "64978" # mrpl38
      cs2 <- "Small"
      cs2 <- "Large"
      # coef2 <- s3readRDS(paste0("coef_ess_19q3/2879.rds"),bucket="depmap")
    }
    eid2 <- input$eid2
    cs2 <- input$cs2

    cs.idx2 <- paste0("mem",match(cs2,cs))
    cl2 <- (df.0 %>% filter(eid==eid2))[[cs.idx2]]
    if(is.na(cl2)){
      return(NULL)
    }

    # s3load(paste0("graph_19q3/",cl2,".rda"),bucket="depmap") # nodes,edges
    graph <- graphs[[cs.idx2]][[cl2]]
    nodes <- graph$nodes
    edges <- graph$edges

    p <- visNetwork(nodes, edges,width = "80%") %>%
      visIgraphLayout() %>%
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
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = F),
             nodesIdSelection = list(enabled =TRUE, selected = eid2),
                 selectedBy = "group") %>% 
      visInteraction(dragView=FALSE,zoomView=FALSE) %>%
      visLayout(randomSeed = 11)
  })

  output$intra_cluster <- renderVisNetwork({
    validate(
      need(input$eid2!="",
        "Please choose an essential gene and a cluster size (default:Small).\nSpearman correlation coefficients between genes (> 0.1) within the cluster will be shown here."
      )
    )
    intra.gene.net()
  })

  inter.gene.net <- reactive({
    options(warn=-1)

    if(0){
      eid2 <- "2879"
      cs2 <- "small"
      coef2 <- s3readRDS(paste0("coef_ess_19q3/2879.rds"),bucket="depmap")
    }
    eid2 <- input$eid2
    cs2 <- input$cs2
    coef2 <- icoef2()

    cs.idx2 <- paste0("mem",match(cs2,cs))

    sym.all2 <- df.0$sym.all[df.0$eid == eid2]
    sym2 <- df.0$sym[df.0$eid == eid2]
    cl2 <- df.0[df.0$eid == eid2,cs.idx2]

    coef.1 <- data.frame(coef=coef2,eid=names(coef2),stringsAsFactors=FALSE) %>%
      left_join(df.0,by="eid") %>%
      filter(eid != eid2)

    mem <- as.character(coef.1[[cs.idx2]])
    mem[is.na(mem)] <- "none"

    med.mem <- tapply(coef.1$coef,mem,mean)
    o <- order(med.mem,decreasing=F)
    clust.levs <- names(med.mem)[o]

    coef.1 <- coef.1 %>% mutate(mem=factor(mem,levels=clust.levs)) %>%
      mutate(text=paste0(sym,"(",mem,") - ",sym2,"(",cl2,")<br />",coef))

    sum.coef.1 <- coef.1 %>% 
      group_by(mem) %>% 
      summarize(maxs=max(coef),mins=min(coef),mids=mean(coef)) %>%
      arrange(mids) %>%
      mutate(mem=factor(mem,levels=clust.levs))

    suppressWarnings({ # doesn't work..
      pvio <- ggplot(sum.coef.1, aes(x=mem,y=mids,color=mem)) +
        geom_linerange(aes(ymin=mins,ymax=maxs))+
        geom_jitter(data=coef.1,aes(x=mem,y=coef),height=0, width=0.1, size=.3,
          color="black")+
        coord_flip() +
        theme_bw() +
        theme(legend.position="none",plot.margin=unit(c(0,0,0,0),"lines"),
          axis.text=element_text(size=10),axis.title=element_text(size=10),
          plot.title=element_text(size=15, hjust=0.5),
          axis.text.x = element_text(angle = 0)) +
        ggtitle(paste0(sym.all2, "\t(",cs2," size clusters)")) + 
        labs(x="",y="Spearman correlation between genes")
      ply <- ggplotly(pvio) %>% 
        add_segments(x = 0, xend = 0, y = 0, yend = 600, 
          line=list(color="black")) %>% 
        config(displayModeBar=F)
      ndata <- length(ply$x$data)
      for(i in seq(ndata-1))ply$x$data[[i]]$hoverinfo <- "none"
        ply$x$data[[ndata]]$text <- coef.1$text
     })
    return(ply)
  })

  output$inter_cluster <- plotly::renderPlotly({
    validate(
      need(input$eid2!="",
        "Please choose an essential gene and a cluster size (default:Small).\nSpearman correlation coefficients across the essential genes will be shown here."
      )
    )
    inter.gene.net()
  })

}

