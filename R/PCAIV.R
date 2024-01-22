# Permet de faire une PCAIV, d'afficher des outputs et de stocker le dudi pour plus tard

pcaIV <- tabItem(tabName = "pcaiv",
                 h2("Principal Component Analysis with respect to Instrumental Variables (PCAIV)"),
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   uiOutput("selectizePCAIV"),
                   uiOutput("selectDudiPCAIV"),
                   uiOutput("selectDfPCAIV"),
                   numericInput(
                     inputId = "nfPCAIV",
                     label = tags$span("Number of dimension to keep", 
                                       bsButton("helpnfpcaiv", label = "",
                                                icon = icon("question-circle" )
                                                , size = "extra-small")),
                     value = 2,
                     min = 2,
                     max = 200
                   ),
                   bsPopover(id = "helpnfpcaiv",
                             title = "",
                             content = paste0(
                               "Number of axes of variance (dimensions) to keep. See more: ",
                               a("pcaiv()", href = "http://adeverse.github.io/ade4/reference/pcaiv.html", target="_blank")),
                             placement = "right",
                             trigger = c('focus', 'hover'),
                             options = list(container = "body")),
                   actionButton("DoPCAIV", "Compute PCAIV", style = "color : white; background-color : #93bf29")
                 ),
                 mainPanel = mainPanel(
                   tabsetPanel(
                     tabPanel("Summary",
                              verbatimTextOutput("summaryPCAIV")
                     ), 
                     
                     tabPanel("Output",
                              uiOutput("selectdatatablePCAIV"),
                              dataTableOutput("datatablePCAIV")
                     ),
                     tabPanel("Plot",
                              plotOutput("plotPCAIV")
                     )
                   )
                 )
               )
)


pcaIVserver <- function(input, output, session, projet){
  
  # Permet de donner un nom à la PCAIV ou de choisir une PCAIV existante
  output$selectizePCAIV <- renderUI({
    all_PCAIV <- sapply(names(projet$dudi), function(x){
      if ("pcaiv" %in% class(projet$dudi[[x]]))
        return(x)
    })
    
    if (length(all_PCAIV) == 0)
      selectizeInput("NamePCAIV",
                     label = tags$span("Analysis name",
                                       popify(el = bsButton("hpcivaname2", label = "", icon = icon("question-circle"), size = "extra-small"),
                                              title = "",
                                              content = "Type in a new name to compute a new PCAIV or select a previous one from the list to display its results.",
                                              placement = "right", trigger = c('focus', 'hover'),
                                              options = list(container = "body")) 
                     ), 
                     choices = all_PCAIV, options = list(create = TRUE))
    
    else{
      last <- all_PCAIV[length(all_PCAIV)]
      selectizeInput("NamePCAIV",
                     label = tags$span("Analysis name",
                                       popify(el = bsButton("hpcivaname1", label = "", icon = icon("question-circle"), size = "extra-small"),
                                              title = "",
                                              content = "Type in a new name to compute a new PCAIV or select a previous one from the list to display its results.",
                                              placement = "right", trigger = c('focus', 'hover'),
                                              options = list(container = "body")) 
                     ), 
                     choices = all_PCAIV, 
                     options = list(create = TRUE), selected = last)
    }
  })
  
  # Permet de choisir l'objet dudi pour faire l'analyse
  output$selectDudiPCAIV <- renderUI({
    if (length(projet$dudi) == 0)
      return("No dudi object present in the project")
    
    selectInput("DudiPCAIV",
                label = tags$span("Dudi object",
                                  popify(el = bsButton("Hdudipcaiv", label = "", icon = icon("question-circle"), size = "extra-small"),
                                         title = "",
                                         content = paste0("A dudi object outputed by a one table-analysis, previously ran or loaded in the app. See more: ",
                                                          a("pcaiv()", href = "http://adeverse.github.io/ade4/reference/pcaiv.html", target="_blank")),
                                         placement = "right", trigger = c('focus', 'hover'),
                                         options = list(container = "body")) 
                ),
                choices = names(projet$dudi),
                selected = input$DudiPCAIV)
  })
  
  # Permet de choisir le dataframe pour faire l'analyse
  output$selectDfPCAIV <- renderUI({
    if (length(projet$data) == 0)
      return()
    
    selectInput("DfPCAIV",
                label = tags$span("Dataframe with the same rows",
                                  popify(el = bsButton("Hdfpcaiv", label = "", icon = icon("question-circle"), size = "extra-small"),
                                         title = "",
                                         content = paste0("A dataframe withe the same rows than the dudi object previously selected. See more: ",
                                                          a("pcaiv()", href = "http://adeverse.github.io/ade4/reference/pcaiv.html", target="_blank")),
                                         placement = "right", trigger = c('focus', 'hover'),
                                         options = list(container = "body")) 
                ),
                choices = names(projet$data),
                selected = input$DfPCAIV)
  })
  
  # Permet de choisir quoi afficher dans l'onglet output
  output$selectdatatablePCAIV <- renderUI({
    if (length(projet$dudi) < 2)
      return()
    
    if (is.null(input$NamePCAIV))
      return()
    
    if (!(input$NamePCAIV %in% names(projet$dudi)))
      return()
    
    remove <- c("eig", "rank", "nf", "call")
    name <- names(projet$dudi[[input$NamePCAIV]])
    keep <- name[! name %in% remove]
    
    selectInput("dataframePCAIV", "select a value to show",
                keep)
    
  })
  
  # Quand on clique sur le bouton pour faire l'analyse
  observeEvent(input$DoPCAIV, {
    if (input$NamePCAIV == ""){
      alert("Please enter a name")
      return(0)
    }
    
    if (input$NamePCAIV %in% names(projet$dudi)){
      alert("Name already taken, please enter a new one")
      return(0)
    }
    
    dudi <- projet$dudi[[input$DudiPCAIV]]
    df <- projet$data[[input$DfPCAIV]]
    
    tryCatch({
      
      temp <- pcaiv(dudi, df ,nf = input$nfPCAIV, scannf = F)
      projet$dudi[[input$NamePCAIV]] <- temp
      
      # Rajoute le code pour faire l'analsye dans projet$code
      string <- paste(input$NamePCAIV, " <- pcaiv(", input$DudiPCAIV, 
                      ", ", input$DfPCAIV, ", nf = ", input$nfPCAIV, 
                      ", scannf = ", F,")", sep = "")
      
      projet$code <- paste(projet$code, string, sep = "\n\n# Computing PCAIV\n")
      
      projet$dudi[[input$NamePCAIV]]$call <- substring(string, nchar(input$NamePCAIV) + 5)
      
    }, error = function(e){
      alert("There has been an error (printed in the R console)")
      print(e)
      return(0)
    })
    
  })
  
  
  output$summaryPCAIV <- renderPrint({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    if (is.null(input$NamePCAIV))
      return("No dudi object with this name in the project")
    
    if (!(input$NamePCAIV %in% names(projet$dudi)))
      return("No dudi object with this name in the project")
    
    pcai <- projet$dudi[[input$NamePCAIV]]
    
    ade4:::summary.dudi(pcai)
  })
  
  
  output$plotPCAIV <- renderPlot({
    if (length(projet$dudi) == 0)
      return(0)
    
    if (is.null(projet$dudi[[input$NamePCAIV]]))
      return(0)
    
    pcai <- projet$dudi[[input$NamePCAIV]]
    
    ade4:::plot.pcaiv(pcai)
  })
  
  output$datatablePCAIV <- renderDataTable({
    if (length(projet$dudi) == 0)
      return(0)
    
    if (is.null(projet$dudi[[input$NamePCAIV]]))
      return(data.frame(list()))
    
    dt <- projet$dudi[[input$NamePCAIV]]
    
    if (is.null(input$dataframePCAIV))
      return(data.frame(list()))
    
    # Pour afficher plus joliement les row et col weight
    dt$cw <- list(col.weight = dt$cw)
    dt$lw <- list(row.weight = dt$lw)
    
    
    datatable(as.data.frame(dt[[input$dataframePCAIV]]), 
              extensions = c("Buttons"),
              options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
  }, server = F)
  
  
}
