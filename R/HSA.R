# Permet de faire une analyse de Hill et SMith, d'afficher des outputs et de stocker le dudi pour plus tard

hsa <- tabItem(tabName = "hsa",
               h2("Hill and Smith Analysis (HSA)"),
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   uiOutput("selectizeHSA"),
                   uiOutput("SelectDataframeHSA"),
                    ## input and hovertext number of dimensions
                   numericInput(
                     inputId = "nfHSA",
                     label = tags$span("Number of dimension to keep", 
                                        bsButton("helpnfhsa", label = "",
                                        icon = icon("question-circle" )
                                        , size = "extra-small")),
                     value = 2,
                     min = 2,
                     max = 200
                   ),
                   bsPopover(id = "helpnfhsa",
                                      title = "",
                                       content = paste0(
                                         "Number of axes of variance (dimensions) to keep. See more: ",
                                         a("dudi.hillsmith()", href = "http://adeverse.github.io/ade4/reference/dudi.hillsmith.html", target="_blank")),
                                       placement = "right",
                                       trigger = c('focus', 'hover'),
                                       options = list(container = "body")),
                   ## checkbox and hovertext row weights hsa     
                   uiOutput("selectDfRwHSA"),
                   uiOutput("selectColumnRwHSA"),

                   actionButton("DoHSA", "Compute HSA", style = "color : white; background-color : #93bf29")
                 ),
                 mainPanel = mainPanel(
                   tabsetPanel(
                     tabPanel("Summary",
                              verbatimTextOutput("SummaryHSA")
                     ),
                     
                     tabPanel("Output",
                              selectInput("selectoutputHSA", "", 
                                          choices = c("Eigenvalues", "Variables", "Individuals", "Dudi object")),
                              uiOutput("selectoutputHSA2"),
                              dataTableOutput("outputHSA")),
                     
                     tabPanel("Plot",
                              plotOutput("screeplotHSA"))
                     
                   )
                   
                 )
               )
)

hsaServer <- function(input, output, session, projet){

  # Permet de donner un nom à la HSA ou de choisir une HSA existante
  output$selectizeHSA <- renderUI({
    all_HSA <- sapply(names(projet$dudi), function(x){
      if ("mix" %in% class(projet$dudi[[x]]))
        return(x)
    })
    
    if (length(all_HSA) == 0)
      selectizeInput("NameHSA",
                     label = tags$span("Analysis name ",
                                       popify(el = bsButton("hhsaname1", label = "", icon = icon("question-circle"), size = "extra-small"),
                                              title = "",
                                              content = "Type in a new name to compute a new HSA or select a previous HSA from the list to display its results.",
                                              placement = "right", trigger = c('focus', 'hover'),
                                              options = list(container = "body")) 
                     ),
                     choices = all_HSA, 
                     options = list(create = TRUE))
    
    else{
      last <- all_HSA[length(all_HSA)]
      selectizeInput("NameHSA", 
                     label = tags$span("Analysis name ",
                               popify(el = bsButton("hhsaname2", label = "", icon = icon("question-circle"), size = "extra-small"),
                                      title = "",
                                      content = "Type in a new name to compute a new HSA or select a previous HSA from the list to display its results.",
                                      placement = "right", trigger = c('focus', 'hover'),
                                      options = list(container = "body")) 
                     ),
                     choices = all_HSA, 
                     options = list(create = TRUE), selected = last)
    }
    
  })
  
  # Permet de choisir le dataframe qui contient les row weights
  output$selectDfRwHSA <- renderUI({
    if (length(projet$data) == 0 & length(projet$dudi) == 0)
      return("")
    
    selectInput("DfRw",
                label = tags$span("Select an object which contains the row weights",
                          popify(el = bsButton("hdfrw", label = "", icon = icon("question-circle"), size = "extra-small"),
                                 title = "",
                                 content = "None for default uniform row weights. Custom weights are useful for two table analyses: select an object in this box and one of its column in the box that will appear below.",
                                 placement = "right", trigger = c('focus', 'hover'),
                                 options = list(container = "body")) 
                ),
                choices = c("None", names(projet$data), names(projet$dudi)))
    
  })
  
  # Permet de choisir la colonne du dataframe qui contient les row weights
  output$selectColumnRwHSA <- renderUI({
    if (is.null(input$DfRw))
      return("")
    
    if (input$DfRw == "None")
      return("")
    
    if (input$DfRw %in% names(projet$data)){
      selectInput("ColumnRw", "Choose the row weigth column",
                  colnames(projet$data[[input$DfRw]]))
    }
    
    else{
      selectInput("ColumnRw", "Choose the row weigth column",
                  names(projet$dudi[[input$DfRw]]), selected = "lw")
    }
    
  })
  
  # Permet de choisir le dataframe pour faire la HSA
  output$SelectDataframeHSA <- renderUI({
    selectInput("DataframeHSA",
                label = tags$span("Select a dataframe ",
                                  popify(el = bsButton("HdfHSA", label = "", icon = icon("question-circle"), size = "extra-small"),
                                         title = "",
                                         content = paste0("A dataframe with n rows (individuals) and p columns (numeric variables) previously loaded in the app. See more: ",
                                           a("dudi.hillsmith()", href = "http://sdray.github.io/ade4/reference/dudi.hillsmith.html", target="_blank")),
                                         placement = "right", trigger = c('focus', 'hover'),
                                         options = list(container = "body")) 
                                  ),
                choices = names(projet$data), selected = input$DataframeHSA)
  })
  
  # Permet de choisir quoi afficher dans l'onglet output
  output$selectoutputHSA2 <- renderUI({
    if(input$selectoutputHSA == "Eigenvalues")
      return()
    
    else if (input$selectoutputHSA == "Variables")
      selectInput("outputHSA2", "Select a value to show", 
                  c("coord", "cor", "cos2", "contrib"))
    
    else if (input$selectoutputHSA == "Individuals")
      selectInput("outputHSA2", "Select a value to show", 
                  c("coord", "cos2", "contrib"))
    
    else {
      if (is.null(input$selectoutputHSA))
        return("")
      
      if (is.null(input$NameHSA))
        return("")
      
      if (!(input$NameHSA %in% names(projet$dudi)))
        return("")
      
      remove <- c("eig", "rank", "nf", "cent", "norm", "call")
      name <- names(projet$dudi[[input$NameHSA]])
      keep <- name[! name %in% remove]
      
      selectInput("outputHSA2", "select a value to show",
                  keep)
    }
  })
  
  # Quand on clique sur le bouton pour faire la HSA
  observeEvent(input$DoHSA, {
    if (input$NameHSA == ""){
      alert("Please enter a name")
      return(0)
    }
    
    if (input$NameHSA %in% names(projet$dudi)){
      alert("Name already taken, please enter a new one")
      return(0)
    }
    
    df <- projet$data[[input$DataframeHSA]]
    
    tryCatch({ # Essaie de faire la HSA
      
      if (input$DfRw == "None"){
        row.weight <- rep(1, nrow(df))/nrow(df)
        
        string_rw <- paste("rep(1, nrow(", input$DataframeHSA, "))/nrow("
                           , input$DataframeHSA, ")",sep = "")
      }
      
      else{
        
        if (input$DfRw %in% names(projet$data)){
          row.weight <- projet$data[[input$DfRw]][,input$ColumnRw]
          string_rw <- paste(input$DfRw, "[, '", input$ColumnRw, "']",sep = "")
        }
        
        else{
          row.weight <- projet$dudi[[input$DfRw]][[input$ColumnRw]]
          string_rw <- paste(input$DfRw, "[['", input$ColumnRw, "']]",sep = "")
          
        }
        
        
      }
      
    temp <- dudi.hillsmith(df, nf = input$nfHSA, scannf = F, 
                     row.w = row.weight)
    
    projet$dudi[[input$NameHSA]] <- temp
    
    # Ecrit le code pour faire la hsa dans projet$code
    string <- paste(input$NameHSA, " <- dudi.hillsmith(", input$DataframeHSA, ", nf = ", input$nfHSA, ", scannf = ", F,
                    ", row.w = ", string_rw, sep = "")
    
    projet$code <- paste(projet$code, string, sep = "\n\n# Computing HSA\n")
    
    projet$dudi[[input$NameHSA]]$call <- substring(string, nchar(input$NameHSA) + 5)
    
    }, error = function(e){
      alert("There has been an error (printed in R console)")
      print(e)
      return(0)
    })
    
  })
  
  output$SummaryHSA <- renderPrint({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    if (is.null(input$NameHSA))
      return("No dudi object with this name in the project")
    
    if (!(input$NameHSA %in% names(projet$dudi)))
      return("No dudi object with this name in the project")
    
    ade4:::summary.dudi(projet$dudi[[input$NameHSA]])
    
  })
  
  
  output$outputHSA <- renderDataTable({
    if (length(projet$dudi) == 0)
      return(data.frame(list()))
    
    if (!(input$NameHSA %in% names(projet$dudi)))
      return(data.frame(list()))
    
    
    if (input$selectoutputHSA == "Eigenvalues"){
      return(datatable(data.frame(list(values = projet$dudi[[input$NameHSA]]$eig))))
    }
    
    else if (input$selectoutputHSA == "Variables")
      dt <- get_pca_var(projet$dudi[[input$NameHSA]])
    
    else if (input$selectoutputHSA == "Individuals")
      dt <- get_pca_ind(projet$dudi[[input$NameHSA]])
    
    else {
      dt <- projet$dudi[[input$NameHSA]]
      dt$cw <- list(col.weight = dt$cw)
      dt$lw <- list(row.weight = dt$lw)
    }
    if (is.null(input$outputHSA2))
      return(data.frame(list()))
    
    datatable(as.data.frame(dt[[input$outputHSA2]]), extensions = c("Buttons"),
              options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
  }, server = F)
  
  
  output$screeplotHSA <- renderPlot({
    if (is.null(projet$dudi[[input$NameHSA]]))
      return(0)
    
    ade4:::screeplot.dudi(projet$dudi[[input$NameHSA]], main = input$NameHSA)
  })
  
}

