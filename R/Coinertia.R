# Permet de faire une Coinertie, d'afficher des outputs et de stocker le dudi pour plus tard

coinertie <- tabItem(tabName = "coinertia",
                     h2("Coinertia Analysis (CA)"),
                     h4("Perform a double inertia analysis of two tables."),
                     sidebarLayout(
                       sidebarPanel = sidebarPanel(
                         uiOutput("selectizeCoinertia"),
                         uiOutput("selectDudiCoinertia1"),
                         uiOutput("selectDudiCoinertia2"),
                         
                         numericInput("nfCoinertia",
                                      label = tags$span("Number of dimension to keep", 
                                                        bsButton("helpnfcoin", label = "",
                                                                 icon = icon("question-circle" )
                                                                 , size = "extra-small")),
                                      2, 2, 200),
                         bsPopover(id = "helpnfcoin",
                                   title = "",
                                   content = paste0(
                                     "Number of axes of variance (dimensions) to keep. See more: ",
                                     a("coinertia()", href = "http://adeverse.github.io/ade4/reference/coinertia.html", target="_blank")),
                                   placement = "right",
                                   trigger = c("hover", "focus"),
                                   options = list(container = "body")),
                         actionButton("DoCoinertia", "Compute coinertia", style = "color : white; background-color :  #93bf29")
                       ),
                       mainPanel = mainPanel(
                         tabsetPanel(
                          tabPanel("Summary",
                                   verbatimTextOutput("summaryCoinertia")
                                   ), 
                           
                          tabPanel("Output",
                                   uiOutput("selectdatatableCoinertia"),
                                   dataTableOutput("datatableCoinertia")
                                   ),
                           tabPanel("Plot",
                                    plotOutput("plotCoinertia")
                                    )
                         )
                       )
                     )
)


coinertiaserver <- function(input, output, session, projet){
  
  # Permet de donner un nom à la coinertie ou de choisir une coinertie existante
  output$selectizeCoinertia <- renderUI({
    all_Coinertia <- sapply(names(projet$dudi), function(x){
      if ("coinertia" %in% class(projet$dudi[[x]]))
        return(x)
    })
    
    if (length(all_Coinertia) == 0)
      selectizeInput("NameCoinertia",
                     label = tags$span("Analysis name ",
                                       popify(el = bsButton("help_name_coin1", label = "", icon = icon("question-circle"), size = "extra-small"),
                                              title = "",
                                              content = "Type in a new name to compute a new coinertia analysis or select a previous one from the list to display its results",
                                              placement = "right", trigger = c("hover", "focus"),
                                              options = list(container = "body"))
                     ),
                     choices = all_Coinertia, options = list(create = TRUE))
    
    else{
      last <- all_Coinertia[length(all_Coinertia)]
      selectizeInput("NameCoinertia",
                     label = tags$span("Analysis name ",
                                       popify(el = bsButton("help_name_coin2", label = "", icon = icon("question-circle"), size = "extra-small"),
                                              title = "",
                                              content = "Type in a new name to compute a new coinertia analysis or select a previous one from the list to display its results",
                                              placement = "right", trigger = c("hover", "focus"),
                                              options = list(container = "body"))
                                       ),
                                       choices = all_Coinertia, 
                     options = list(create = TRUE), selected = last)
    }
  })
  
  # Permet de choisir le premier objet dudi pour l'analyse
  output$selectDudiCoinertia1 <- renderUI({
    if (length(projet$dudi) == 0)
      return("No dudi present in the project")
    
    selectInput("DudiCoinertia1",
                label = tags$span("First dudi object",
                                  popify(el = bsButton("coin_dudi_1", label = "", icon = icon("question-circle"), size = "extra-small"),
                                         title = "",
                                         content = paste0("A duality diagram (object of class dudi) outputed by a one-table analysis. See more: ",
                                                          a("coinertia()", href = "http://adeverse.github.io/ade4/reference/coinertia.html", target="_blank")),
                                        placement = "right", trigger = c("hover", "focus"),
                                         options = list(container = "body"))
                ),
                choices = names(projet$dudi),
                selected = input$DudiCoinertia1)
  })
  
  # Permet de choisir le 2e objet dudi pour l'analyse
  output$selectDudiCoinertia2 <- renderUI({
    if (length(projet$dudi) == 0)
      return()
    
    if (length(projet$dudi) == 1)
      return("Only one dudi present in the project")
    
    selectInput("DudiCoinertia2",
                label = tags$span("Second dudi object",
                                  popify(el = bsButton("coin_dudi_2", label = "", icon = icon("question-circle"), size = "extra-small"),
                                         title = "",
                                         content = paste0("A duality diagram (object of class dudi) outputed by a one-table analysis. See more: ",
                                                          a("coinertia()", href = "http://adeverse.github.io/ade4/reference/coinertia.html", target="_blank")),
                                         placement = "right", trigger = c("hover", "focus"),
                                         options = list(container = "body"))
                ),
                choices = names(projet$dudi),
                selected = input$DudiCoinertia2)
  })
  
  # Permet de choisir quoi afficher dans l'onglet output
  output$selectdatatableCoinertia <- renderUI({
    if (length(projet$dudi) < 2)
      return()
    
    if (is.null(input$NameCoinertia))
      return()
    
    if (!(input$NameCoinertia %in% names(projet$dudi)))
      return()
    
    remove <- c("eig", "rank", "nf", "RV", "call")
    name <- names(projet$dudi[[input$NameCoinertia]])
    keep <- name[! name %in% remove]
    
    selectInput("dataframeCoinertia", "select a value to show",
                keep)
    
  })
  
  # Quand on clique sur le bouton pour lancer l'analyse
  observeEvent(input$DoCoinertia, {
    if (is.null(input$NameCoinertia)){
      alert("Please enter a name")
      return(0)
    }
    
    if (input$NameCoinertia %in% names(projet$dudi)){
      alert("Name already taken, please enter a new one")
      return(0)
    }
    
    dud1 <- projet$dudi[[input$DudiCoinertia1]]
    dud2 <- projet$dudi[[input$DudiCoinertia2]]
    
    if (identical(dud1$lw, dud2$lw) == FALSE) # Si les row weights sont différents
      alert("Be careful, the row weight are different")
    
    tryCatch({
      
      temp <- coinertia(dud1, dud2 ,nf = input$nfCoinertia, scannf = F)
      projet$dudi[[input$NameCoinertia]] <- temp
      
      # Rajoute le code pour faire l'analyse dans projet$code
      string <- paste(input$NameCoinertia, " <- coinertia(", input$DudiCoinertia1, 
                      ", ", input$DudiCoinertia2, ", nf = ", input$nfCoinertia, 
                      ", scannf = ", F,")", sep = "")
      
      projet$code <- paste(projet$code, string, sep = "\n\n# Computing Coinertia\n")
      
      projet$dudi[[input$NameCoinertia]]$call <- substring(string, nchar(input$NameCoinertia) + 5)
      
    }, error = function(e){
      alert("There has been an error (printed in the R console)")
      print(e)
      return(0)
    })
    
  })
  
  
  output$summaryCoinertia <- renderPrint({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    if (is.null(input$NameCoinertia))
      return("No dudi object with this name in the project")
    
    if (!(input$NameCoinertia %in% names(projet$dudi)))
      return("No dudi object with this name in the project")
    
    ade4:::summary.dudi(projet$dudi[[input$NameCoinertia]])
  })
  
  
  output$plotCoinertia <- renderPlot({
    if (length(projet$dudi) == 0)
      return(0)
    
    if (is.null(projet$dudi[[input$NameCoinertia]]))
      return(0)
    
    coin <- projet$dudi[[input$NameCoinertia]]
    
    ade4:::plot.coinertia(coin)
  })
  
  output$datatableCoinertia <- renderDataTable({
    if (length(projet$dudi) == 0)
      return(0)
    
    if (is.null(projet$dudi[[input$NameCoinertia]]))
      return(data.frame(list()))
    
    dt <- projet$dudi[[input$NameCoinertia]]
    
    if (is.null(input$dataframeCoinertia))
      return(data.frame(list()))
    
    dt$cw <- list(col.weight = dt$cw)
    dt$lw <- list(row.weight = dt$lw)
    
    datatable(as.data.frame(dt[[input$dataframeCoinertia]]), 
              extensions = c("Buttons"),
              options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
  }, server = F)
  
  
}