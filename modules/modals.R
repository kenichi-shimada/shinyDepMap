essentialityModal <- function() {
  modal(
    id = "modal_essentiality",
    header = h3("Gene essentiality app"),
    size = "lg",
    div(
      class = "div_modal",
      p("shinyDepMap first asks whether a gene of interest is essential or not in any cells.",
        "If essential, it also asks whether the gene is essential in all the cell lines or only in a subset.",
        "Selectively essential genes are the genes required by only a subset of cells, which may be a good candidate gene for the targeted therapy. The",
        tags$b("Gene essentiality app"), "summarizes the gene essentiality using two parameters:"),
      tags$ul(
        tags$li(tags$b("Efficacy"),": how essential the gene is in the sensitive cell lines ",
          "(the more negative the efficacy is, the more essential the gene is)."),
        tags$li(tags$b("Selectivity"),": how selectively essential the gene is between ",
          "sensitive and resistant cell lines (the more positive the selectivity is, ",
          "the more selective the gene is).")
      ),
      p("Using arbitrary thresholds (efficacy = -0.56, selectivity = 1), ",
        "shown in grey lines in the ",tags$b('Efficacy/selectivity plot',class="hl"),
        "we defined 2,492 genes are esential, 733 of which are selectively essential ",
        "(See ", tags$a(href="https://doi.org/10.1101/2019.12.13.874776","our paper")," for details)."),
      p("Here are two use cases that the ",tags$b("Gene essentiality app")," may be helpful.") %>% 
        yonder::margin(l = "auto", r = "auto", b = 2),
 
      h4("Case 1. Which cyclin-dependent kinases (CDKs) are selectively essential?"),
      p("The ", tags$b("Gene essentiality app")," consists of two panels: an input panel on the left and two output panels on the right. ",
        "The left output panel is ",tags$b('Efficacy/selectivity plot',class="hl")," which plots the efficacy and selectivity of 15,847 protein-encoding genes. ",
        "The right panel is empty at first."),

      img(src = "assets/img/ess1.png") %>% 
        yonder::margin(l = "auto", r = "auto", b = 2),

      p("In shinyDepMap, you always begin by making your query in the input panel. ",
        "In the ",tags$b("Gene essentiality app"),", you will first type in either a full gene symbol ",
        "(e.g., \"cdk4\", case-insensitive) or a prefix (e.g., \"cdk\") in the ",tags$b("Type in a gene name",class="hl"), " textbox. ",
        "In search for \"cdk\", all the gene symbols that begin with CDK (",tags$i("CDK1"),tags$i("CDK2"),", ...) ",
        "are shown on the ",tags$b("Matched genes",class="hl")," on the right panel. ",
        "These genes are also highlighted in orange or red on the ",tags$b("Efficacy/selectivity plot",class="hl"), ".",
        "Every gene symbol on the ",tags$b("Matched genes",class="hl"), " has a link to the gene's entry in ", tags$a(href="https://www.genecards.org/","Gene Cards",target="_blank"), ".",
        "In the ",tags$b("Efficacy/selectivity plot",class="hl"),", ",tags$i("CDK1")," is the most essential but little selective gene, ",
        "while ",tags$i("CDK4"),", is the third most potent and the most selective gene among all the CDKs. This confirms", 
        tags$i("CDK4")," gene as a good drug target for targeted chemotherapy."),

      img(src = "assets/img/ess2.png") %>% 
        yonder::margin(l = "auto", r = "auto", b = 2),
      img(src = "assets/img/ess3.png") %>% 
        yonder::margin(l = "auto", r = "auto", b = 4),        

      h4("Case 2. Which cell line is the most sensitive to the loss of ",tags$i("CDK4"),"?"),
      p("Now assume you want to study the", tags$i("CDK4")," gene in your experiment.",
        "It is crucial to use the right cell lines to use. ",tags$i("CDK4")," is a cell-line selectively essential gene, ",
        "and randomly chosen cell line may not require the gene for survival. ",
        "In shinyDepMap (as well as the ",tags$a(href="https://depmap.org/portal/gene/CDK4?tab=overview","DepMap portal",target="_blank"),
        " for this specific function), you can easily find the right cell line for the experiment.",
        "After you type in in the top-left textbox, you can select one of the matched genes ",tags$i("CDK4")," as one of the matched genes from the ",tags$b("Select a matched gene",class="hl")," dropdown menu. ",
        "So select ",tags$i("CDK4")," and go to the ",tags$b("Dependency scores",class="hl")," tab on the right. ",
        "There you can find the raw dependency scores of the ",tags$i("CDK4")," gene in 423 cell lines, grouped by their lineages. ",
        "By hovering the mouse on the plot, you will find out a triple-negative breast cancer cell line, ",
        "MDA-MB453, has the most negative score, i.e., the cells most strongly depend on the gene."),

      img(src = "assets/img/ess4.png") %>% 
        yonder::margin(l = "auto", r = "auto", b = 2)
    ) %>% 
      font(align = "left") %>%
      display("flex") %>% 
      flex(direction = "column")
  )
}

clusterModal <- function() {
  modal(
    id = "modal_cluster",
    header = h3("Gene cluster app"),
    size = "lg",
    div(
      class="div_modal",
      p("Genes that work together as complexes or pathways should show similar dependency scores across cell lines.",
        "With this assumption, we clustered the 2,492 essential genes based on the similarity of their dependency scores",
        "across 423 cell lines—the resulting clusters group essential genes that likely work together.", 
        "Here, we used acustomized cluster algorithm, which controls the size and tightness",
        "(i.e., strength of the connection) of the clusters",
        "(See ", tags$a(href="https://doi.org/10.1101/2019.12.13.874776","our paper")," for details).",
        " The ",tags$b("Gene cluster app")," allows users to take a look at these clusters."),
      p("Here are two use cases of the ",tags$b("Gene cluster app.")),

      h4("Case 1. What pathway/complex is relevant to ",tags$i("KRAS"),"?"),

      p("The ",tags$b("Gene cluster app")," also consists of an input panel and three output charts: ",
        tags$b("Efficacy/selectivity plot",class="hl")," and ",tags$b("t-SNE plot",class="hl")," in the center, and the right panel is blank at first."),

      img(src = "assets/img/clu1.png") %>% 
        yonder::margin(l = "auto", r = "auto", b = 2),

      p("You will first select a gene of interest (",tags$i("KRAS")," here) from the ",tags$b("Essential gene",class="hl")," dropdown menu, and specify a ",tags$b("Cluster size",class="hl"),
        ", which is set as \"Small\" by default. As soon as you choose the gene, the app finds a cluster that contains ",tags$i("KRAS"),", and show the genes",
        "(",tags$i("RAF1")," and ",tags$i("SHOC2"),") in the ",tags$b("Clustered genes",class="hl")," tab.  The cluster suggests that the three genes have the most similar dependency scores,",
        "so we can expect that ",tags$i("KRAS"),"-dependent cells are also dependent on ",tags$i("RAF1")," and ",tags$i("SHOC2"),"."),

      img(src = "assets/img/clu2.png") %>% 
        yonder::margin(l = "auto", r = "auto", b = 2),

      p("By making the cluster size \"Large,\"",
        "we extend our cluster to incorporate four more genes (",tags$i("BRAF"),", ",tags$i("MAPK1"),", ",tags$i("TCF7L2"),", ",tags$i("CTNNB1"),").",
        "This cluster contains ",tags$i("KRAS"),", ",tags$i("BRAF"),", ",tags$i("RAF1"),", and ",tags$i("MAPK1"),",",
        "which are the members of the MAPK pathway. While the role of ",tags$i("KRAS")," in the MAPK pathway has been well studied and not too surpirsing,", 
        "their close relationship is not immediately discoverable in the ",tags$a(href="https://depmap.org/portal/gene/KRAS?tab=overview","DepMap portal",target="_blank"),
        ". This analysis may add some unique value to the ",tags$b("Gene cluster app"),".",
        "The cluster also contains two other genes, ",tags$i("TCF7L2")," and ",tags$i("CTNNB1")," are the members of the Wnt signaling pathway,",
        "suggesting that ",tags$i("KRAS"),"-dependent cancers also depend on the genes."),

      img(src = "assets/img/clu3.png") %>% 
        yonder::margin(l = "auto", r = "auto", b = 2),

      h4("Case 2. Can we find a better druggable target for ",tags$i("KRAS"),"-dependent cancers?"),

      p("",tags$i("KRAS")," is a notorious cancer driver gene that is frequently mutated in many cancers,",
        "but KRAS protein has a very smooth surface, which makes it difficult to inhibit by a drug.",
        "In such a case, we can think of \"target hop\" strategy that targets other more druggable proteins",
        "instead of trying to inhibit KRAS directly. The ",tags$i("KRAS"),"-containing cluster is an excellent place to look for",
        "alternative targets for the target hop, but one needs to be careful because not all the genes are connected equally."),
      p("The ",tags$b("Connectivity",class="hl")," tab on the right panel would help find out ",tags$i("KRAS"),"-relevant genes.",
        "It shows a network diagram in which nodes represent genes in the cluster,",
        "which is color-coded by their memberships of the smallest clusters.",
        "The nodes are connected with edges when the two genes are strongly correlated (Spearman > 0.1).",
        "In this diagram, ",tags$i("KRAS")," is highly correlated with five genes in the cluster, but not with ",tags$i("BRAF"),".",
        "Thus, ",tags$i("BRAF")," cannot be an alternative target for ",tags$i("KRAS"),"."),

      img(src = "assets/img/clu4.png") %>% 
        yonder::margin(l = "auto", r = "auto", b = 2),

      p("Lastly, the ",tags$b("Correlation",class="hl")," tab shows the Spearman correlation coefficients",
        "between the dependency scores of ",tags$i("KRAS"),"",
        "and the other essential genes. It provides another way to discover a highly correlated gene irrespective of the clusters.",
        "According to this plot, the ",tags$i("DOCK5")," gene is highly correlated.",
        "Thus ",tags$i("DOCK5")," may also serve as an alternative target for ",tags$i("KRAS"),"-dependent cancers."),

      img(src = "assets/img/clu5.png") %>% 
        yonder::margin(l = "auto", r = "auto", b = 2)

    ) %>% 
      font(align = "left") %>%
      display("flex") %>% 
      flex(direction = "column")
  )
}