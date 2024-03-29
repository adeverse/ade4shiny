# Permet de faire une BCA, d'afficher des outputs et de stocker le dudi pour plus tard

bga <- tabItem(tabName = "bga",
               h2("Between-Group Analysis (BGA)"),
               h4("Also called BCA for Between-Class Analysis"),
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   uiOutput("selectizeBGA"),
                   uiOutput("selectDudiBGA"),
                   tags$hr(style="border-color: gray;"),
                   p(markdown("#### Grouping factor")),
                   uiOutput("selectObjectGroupBGA"),
                   uiOutput("selectGroupBGA"),
                   tags$hr(style="border-color: gray;"),
                   
                   numericInput("nfBGA",
                                label = tags$span("Number of dimension to keep", 
                                                  bsButton("helpnfbga", label = "",
                                                           icon = icon("question-circle" )
                                                           , size = "extra-small")),
                                2, 2, 200),
                   
                   bsPopover(id = "helpnfbga",
                             title = "",
                             content = paste0(
                               "Number of axes of variance (dimensions) to keep. See more: ",
                               a("dudi.pca()", href = "http://adeverse.github.io/ade4/reference/bca.html", target="_blank")),
                             placement = "right",
                             trigger = c("hover", "focus", "click"),
                             options = list(container = "body")),
                   
                   actionButton("DoBGA", "Compute BGA", style = "color : white; background-color :  #93bf29")
                 ),
                 mainPanel = mainPanel(
                   tabsetPanel(
                     tabPanel("Summary",
                              verbatimTextOutput("summaryBGA")
                              ),
                     tabPanel("Output",
                              uiOutput("selectoutputBGA"),
                              dataTableOutput("datatableBGA")
                              ),
                     tabPanel("Plot",
                              plotOutput("plotBGA")
                              )  
                   )
                 )
               )
)


bgaServer <- function(input, output, session, projet){
  
  # Permet de donner un nom a la BCA ou de choisir une BCA existante
  output$selectizeBGA <- renderUI({
    all_BGA <- sapply(names(projet$dudi), function(x){
      if ("between" %in% class(projet$dudi[[x]]))
        return(x)
    })
    
    if (length(all_BGA) == 0)
      selectizeInput("NameBGA",
                     label = tags$span("Analysis name ",
                                       popify(el = bsButton("namebga1", label = "", icon = icon("question-circle"), size = "extra-small"),
                                              title = "",
                                              content = "Type in a new name to compute a new BGA or select a previous BGA from the list to display its results",
                                              placement = "right", trigger = c("hover", "focus", "click"),
                                              options = list(container = "body")) 
                     ),
                     choices = all_BGA, options = list(create = TRUE))
    
    else{
      last <- all_BGA[length(all_BGA)]
      selectizeInput("NameBGA",
                     label = tags$span("Analysis name ",
                     popify(el = bsButton("namebga2", label = "", icon = icon("question-circle"), size = "extra-small"),
                            title = "",
                            content = "Type in a new name to compute a new BGA or select a previous BGA from the list to display its results",
                            placement = "right", trigger = c("hover", "focus", "click"),
                            options = list(container = "body"))), 
      choices = all_BGA, 
      options = list(create = TRUE), selected = last)
    }
    
  })
  
  # Permet de choisir le dataframe pour la bca
  output$selectDudiBGA <- renderUI({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    selectInput("DudiBGA",
                label = tags$span("Select a Dudi object",
                                  popify(el = bsButton("dudiobjectbga", label = "", icon = icon("question-circle"), size = "extra-small"),
                                         title = "",
                                         content = paste0("A duality diagram (object of class dudi), outputed by a one table analysis, which is present in the app environment (previously ran or lodaded in the app). See more: ",
                                                          a("bca()", href = "http://adeverse.github.io/ade4/reference/dudi.pca.html", target="_blank")),
                                         placement = "right", trigger = c("hover", "focus", "click"),
                                         options = list(container = "body")) 
                ),
                choices = names(projet$dudi), 
                selected = input$DudiBGA)
  })
  
  # Permet de choisir l'objet contenant le vecteur de group de la bca
  output$selectObjectGroupBGA <-renderUI(
    
    selectInput("ObjectGroupBGA",
                label = tags$span("Select an object",
                                  popify(el = bsButton("objectbga", label = "", icon = icon("question-circle"), size = "extra-small"),
                                         title = "",
                                         content = paste0("An object (dudi or dataframe) loaded in the app in which to select the grouping factor for the between group analysis."),
                                         placement = "right", trigger = c("hover", "focus", "click"),
                                         options = list(container = "body")) 
                ),
                choices = c(names(projet$data), names(projet$dudi)),
                selected = input$ObjectGroupBGA)
    
  )
  
  # Permet de choisir la colonne de l'objet qui contient le grouping factor
  # Si l'objet est un dudi, on choisit une colonne parmi dudi$tab
  # Si l'objet est un dataframe, on choisit une colonne
  output$selectGroupBGA <- renderUI({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    if (is.null(input$ObjectGroupBGA))
      return("")
    
    
    if ("dudi" %in% class(projet$dudi[[input$ObjectGroupBGA]]))
      selectInput("GroupBGA",
                  label = tags$span("Select a grouping column",
                                    popify(el = bsButton("groupbga1", label = "", icon = icon("question-circle"), size = "extra-small"),
                                           title = "",
                                           content = paste0("A factor partitioning the rows of dudi$tab in classes. Dudis$tab is the data frame that was analyzed with the one-table analysis, modified according to the transformation arguments that were used (ie centered and scaled). See more: ",
                                                            a("bca()", href = "http://adeverse.github.io/ade4/reference/bca.html", target="_blank")),
                                           placement = "right", trigger = c("hover", "focus", "click"),
                                           options = list(container = "body")) 
                  ),
                  choices = names(projet$dudi[[input$ObjectGroupBGA]]$tab),
                  selected = input$GroupBGA)
    
    else
      selectInput("GroupBGA",
                  label = tags$span("Select a grouping column",
                                    popify(el = bsButton("groupbga2", label = "", icon = icon("question-circle"), size = "extra-small"),
                                           title = "",
                                           content = paste0("A factor partitioning the rows of dudi$tab in classes. Dudis$tab is the data frame that was analyzed with the one-table analysis, modified according to the transformation arguments that were used (ie centered and scaled). See more: ",
                                                            a("bca()", href = "http://adeverse.github.io/ade4/reference/bca.html", target="_blank")),
                                           placement = "right", trigger = c("hover", "focus", "click"),
                                           options = list(container = "body")) 
                  ),
                  choices = colnames(projet$data[[input$ObjectGroupBGA]]),
                  selected = input$GroupBGA)
  })
  
  
  # Permet de choisir quoi afficher dans l'onglet output
  output$selectoutputBGA <- renderUI({
    if (is.null(input$NameBGA) | !(input$NameBGA %in% names(projet$dudi)))
      return()
    
    
    remove <- c("eig", "rank", "nf", "call")
    name <- names(projet$dudi[[input$NameBGA]])
    keep <- name[! name %in% remove]
    
    selectInput("outputBGA", "select a value to show",
                keep)
  })
  
  # Quand on clique sur le bouton pour faire l'analyse
  observeEvent(input$DoBGA, {
    if (input$NameBGA == ""){
      alert("Please enter a name")
      return(0)
    }
    
    if (input$NameBGA %in% names(projet$dudi)){
      alert("Name already taken, please enter a new one")
      return(0)
    }
    
    dud <- projet$dudi[[input$DudiBGA]]
    if ("dudi" %in% class(projet$dudi[[input$ObjectGroupBGA]]))
      fact <- as.factor(projet$dudi[[input$ObjectGroupBGA]]$tab[,input$GroupBGA])
    else
      fact <- as.factor(projet$data[[input$ObjectGroupBGA]][,input$GroupBGA])
    
    tryCatch({
      
      if ("coinertia" %in% class(dud)) {
	      # temp <- bca.coinertia(x = dud, fac = fact, nf = input$nfBGA, scannf = F)
          alert("Sorry, not yet available ! Please contact the authors")
      } else {
          temp <- bca(x = dud, fac = fact, nf = input$nfBGA, scannf = F)
      }
      
      projet$dudi[[input$NameBGA]] <<- temp
      
      # Ecrit le code pour faire l'analyse dans projet$code
      if ("dudi" %in% class(projet$dudi[[input$ObjectGroupBGA]]))
        string <- paste(input$NameBGA, " <- bca(x = ", input$DudiBGA, ", fac = ", 
                        "as.factor(", input$ObjectGroupBGA, "$tab[,", '"',input$GroupBGA,'"',"])",
                        ", nf = ", input$nfBGA, ", scannf = F"
                        ,")", sep = "")
      
      else
        string <- paste(input$NameBGA, " <- bca(x = ", input$DudiBGA, ", fac = ", 
                        "as.factor(", input$ObjectGroupBGA, "[,", "'",input$GroupBGA,"'","])",
                        ", nf = ", input$nfBGA, ", scannf = F"
                        ,")", sep = "")
      
      projet$code <<- paste(projet$code, string, sep = "\n\n# Computing BCA\n")
      
      projet$dudi[[input$NameBGA]]$call <<- str2lang(substring(string, nchar(input$NameBGA) + 5))
      
    }, error = function(e){
      alert("There has been an error (printed in R console)")
      print(e)
      return(0)
    })
    
    
  })
  
  
  output$summaryBGA <- renderPrint({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    if (is.null(input$NameBGA))
      return("No dudi object with this name in the project")
    
    if (!(input$NameBGA %in% names(projet$dudi)))
      return("No dudi object with this name in the project")
    
    #if ("betcoi" %in% class(projet$dudi[[input$NameBGA]]))
      #ade4:::summary.dudi(projet$dudi[[input$NameBGA]])
    #else 
    ade4:::summary.dudi(projet$dudi[[input$NameBGA]])
  })
  
  
  output$datatableBGA <- renderDataTable({
    if (is.null(projet$dudi[[input$NameBGA]]))
      return(data.frame(list()))
    
    dt <- projet$dudi[[input$NameBGA]]
    
    if (is.null(input$outputBGA))
      return(data.frame(list()))
    
    dt$cw <- list(col.weight = dt$cw)
    dt$lw <- list(row.weight = dt$lw)
    dt$ratio <- list(ratio = dt$ratio)
    
    datatable(as.data.frame(dt[[input$outputBGA]]), extensions = c("Buttons"),
              options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
  }, server = F)
  
  
  
  output$plotBGA <- renderPlot({
    if (is.null(projet$dudi[[input$NameBGA]]))
      return(0)
    
    dud <- projet$dudi[[input$DudiBGA]]
    
    if ("dudi" %in% class(projet$dudi[[input$ObjectGroupBGA]]))
      fact <- as.factor(projet$dudi[[input$ObjectGroupBGA]]$tab[,input$GroupBGA])
    
    else
      fact <- as.factor(projet$data[[input$ObjectGroupBGA]][,input$GroupBGA])
    
    temp <- bca(dud, fac = fact,nf = input$nfBGA, scannf = F)

    if ("betcoi" %in% class(dud))
        ade4:::plot.betcoi(temp, fac = fact)
	else
        ade4:::plot.between(temp)
  })
  
  
  
}