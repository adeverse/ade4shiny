# Permet de faire une ACP, d'afficher des outputs et de stocker le dudi pour plus tard

acp <- tabItem(tabName = "pca",
               h2("Principal component analysis (PCA)"),
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   uiOutput("selectizeACP"),
                   uiOutput("SelectDataframeACP"),
                    ## input and hovertext number of dimensions
                   numericInput(
                     inputId = "nfPCA",
                     label = tags$span("Number of dimension to keep", 
                                        bsButton("helpnfpca", label = "",
                                        icon = icon("question-circle" )
                                        , size = "extra-small")),
                     value = 2,
                     min = 2,
                     max = 200
                   ),
                   bsPopover(id = "helpnfpca",
                                      title = "",
                                       content = paste0(
                                         "Number of axes of variance (dimensions) to keep. See more: ",
                                         a("dudi.pca()", href = "http://adeverse.github.io/ade4/reference/dudi.pca.html", target="_blank")),
                                       placement = "right",
                                       trigger = c('focus', 'hover', 'click'),
                                       options = list(container = "body")),
                    ## checkbox and hover text center pca
                   checkboxInput(
                     inputId = "docenterACP",
                     label = tags$span("Center", 
                                       bsButton("helpcenterpca", label = "",
                                                icon = icon("question-circle" )
                                                , size = "extra-small")),
                     value = TRUE
                   ),
                   bsPopover(id = "helpcenterpca",
                             title = "",
                             content = paste0(
                               "If checked, centering by the mean, if uncheck, no centring. Default : centering by mean. See more: ",
                               a("dudi.pca()", href = "http://adeverse.github.io/ade4/reference/dudi.pca.html", target="_blank")),
                             placement = "right",
                             # trigger = c('focus', 'hover', 'click'),
                             trigger = c('focus', 'hover', 'click'),
                             options = list(container = "body")),
                    ## checkbox and hovertext scale pca     
                   checkboxInput(
                     inputId = "doscaleACP",
                     label = tags$span("Scale ", 
                                       bsButton("helpscalepca", label = "",
                                                          icon = icon("question-circle"),
                                                          size = "extra-small")),
                    value = TRUE
                   ),
                    bsPopover(id = "helpscalepca",
                        title = "",
                        content = paste0(
                          "Should column vectors be normed for the row.w weighting ? Yes if checked, no if unchecked. Default : yes. See more: ",
                          a("dudi.pca()", href = "http://adeverse.github.io/ade4/reference/dudi.pca.html", target="_blank")),
                        placement = "right",
                        trigger = c('focus', 'hover', 'click'),
                        options = list(container = "body")),
                   hr(),
                   p(markdown("**Row weights**")),
                   ## checkbox and hovertext row weights pca     
                   uiOutput("selectDfRw"),
                   uiOutput("selectColumnRw"),
                 hr(),
                 p(markdown("**Column weights**")),
                 uiOutput("selectDfCw"),
                 uiOutput("selectColumnCw"),

                   actionButton("DoACP", "Compute PCA", style = "color : white; background-color : #93bf29")
                 ),
                 mainPanel = mainPanel(
                   tabsetPanel(
                     tabPanel("Summary",
                              verbatimTextOutput("SummaryPCA")
                     ),
                     
                     tabPanel("Output",
                              selectInput("selectoutputPCA", "", 
                                          choices = c("Eigenvalues", "Variables", "Individuals", "Dudi object")),
                              uiOutput("selectoutputPCA2"),
                              dataTableOutput("outputPCA")),
                     
                     tabPanel("Plot",
                              plotOutput("screeplotPCA"))
                     
                   )
                   
                 )
               )
)



acpServer <- function(input, output, session, projet){
  
  # Permet de donner un nom à l'ACP ou de choisir une ACP existante
  output$selectizeACP <- renderUI({
    all_PCA <- sapply(names(projet$dudi), function(x){
      if ("pca" %in% class(projet$dudi[[x]]))
        return(x)
    })
    
    if (length(all_PCA) == 0)
      selectizeInput("NameACP",
                     label = tags$span("Analysis name ",
                                       popify(el = bsButton("hpcaname1", label = "", icon = icon("question-circle"), size = "extra-small"),
                                              title = "",
                                              content = "Type in a new name to compute a new PCA or select a previous PCA from the list to display its results.",
                                              placement = "right", trigger = c('focus', 'hover', 'click'),
                                              options = list(container = "body")) 
                     ),
                     choices = all_PCA, 
                     options = list(create = TRUE))
    
    else{
      last <- all_PCA[length(all_PCA)]
      selectizeInput("NameACP", 
                     label = tags$span("Analysis name ",
                               popify(el = bsButton("hpcaname2", label = "", icon = icon("question-circle"), size = "extra-small"),
                                      title = "",
                                      content = "Type in a new name to compute a new PCA or select a previous PCA from the list to display its results.",
                                      placement = "right", trigger = c('focus', 'hover', 'click'),
                                      options = list(container = "body")) 
                     ),
                     choices = all_PCA, 
                     options = list(create = TRUE), selected = last)
    }
    
  })
  
  
  # Permet de choisir le dataframe qui contient les row weights
  output$selectDfRw <- renderUI({
    if (length(projet$data) == 0 & length(projet$dudi) == 0)
      return("")
    
    selectInput("DfRw",
                label = tags$span("Select an object which contains the row weights",
                          popify(el = bsButton("hdfrw", label = "", icon = icon("question-circle"), size = "extra-small"),
                                 title = "",
                                 content = "None for default uniform row weights. Custom weights are useful for two table analyses: select an object in this box and one of its column in the box that will appear below.",
                                 placement = "right", trigger = c('focus', 'hover', 'click'),
                                 options = list(container = "body")) 
                ),
                choices = c("None", names(projet$data), names(projet$dudi)))
    
  })
  
  # Permet de choisir le dataframe qui contient les col weights
  output$selectDfCw <- renderUI({
    if (length(projet$data) == 0 & length(projet$dudi) == 0)
      return("")
    
    selectInput("DfCw",
                label = tags$span("Select an object which contains the column weights",
                                  popify(el = bsButton("hdfcw", label = "", icon = icon("question-circle"), size = "extra-small"),
                                         title = "",
                                         content = "None for default uniform column weights. Custom weights are useful for two table analyses: select an object in this box and one of its column in the box that will appear below.",
                                         placement = "right", trigger = c('focus', 'hover', 'click'),
                                         options = list(container = "body")) 
                ),
                choices = c("None", names(projet$data), names(projet$dudi)))
    
  })
  
  # Permet de choisir la colonne du dataframe qui contient les row weights
  output$selectColumnRw <- renderUI({
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
  
  # Permet de choisir la colonne du dataframe qui contient les col weights
  output$selectColumnCw <- renderUI({
    if (is.null(input$DfCw))
      return("")
    
    if (input$DfCw == "None")
      return("")
    
    if (input$DfCw %in% names(projet$data)){
      selectInput("ColumnCw", "Choose the col weigth column",
                  colnames(projet$data[[input$DfCw]]))
    }
    
    else{
      selectInput("ColumnCw", "Choose the col weigth column",
                  names(projet$dudi[[input$DfCw]]), selected = "cw")
    }
    
  })

  
  # trying to add popover with input generated by renderui does not work. help text for select dataframe
  #delay(400,
  # addPopover(session, id = "HdfPCA", title = "", content = paste0(
  #   "A dataframe with n rows (individuals) and p columns (numeric variables) previously loaded in the app. See more: ",
  #   a("dudi.pca()", href = "http://adeverse.github.io/ade4/reference/dudi.pca.html", target="_blank")),
  #   placement = "right", trigger = c('focus', 'hover', 'click'),
  #   options = list(container = "body"))
  #)
  
  # Permet de choisir le dataframe pour faire l'acp
  output$SelectDataframeACP <- renderUI({
    selectInput("DataframeACP",
                label = tags$span("Select a dataframe ",
                                  popify(el = bsButton("HdfPCA", label = "", icon = icon("question-circle"), size = "extra-small"),
                                         title = "",
                                         content = paste0("A dataframe with n rows (individuals) and p columns (numeric variables) previously loaded in the app. See more: ",
                                           a("dudi.pca()", href = "http://adeverse.github.io/ade4/reference/dudi.pca.html", target="_blank")),
                                         placement = "right", trigger = c('focus', 'hover', 'click'),
                                         options = list(container = "body")) 
                                  ),
                choices = names(projet$data), selected = input$DataframeACP)
  })
  
  # Permet de choisir quoi afficher dans l'onglet output
  output$selectoutputPCA2 <- renderUI({
    if(input$selectoutputPCA == "Eigenvalues")
      return()
    
    else if (input$selectoutputPCA == "Variables")
      selectInput("outputPCA2", "Select a value to show", 
                  c("coord", "cor", "cos2", "contrib"))
    
    else if (input$selectoutputPCA == "Individuals")
      selectInput("outputPCA2", "Select a value to show", 
                  c("coord", "cos2", "contrib"))
    
    else {
      if (is.null(input$selectoutputPCA))
        return("")
      
      if (is.null(input$NameACP))
        return("")
      
      if (!(input$NameACP %in% names(projet$dudi)))
        return("")
      
      remove <- c("eig", "rank", "nf", "cent", "norm", "call")
      name <- names(projet$dudi[[input$NameACP]])
      keep <- name[! name %in% remove]
      
      selectInput("outputPCA2", "select a value to show",
                  keep)
    }
  })
  
  
  # Quand on clique sur le bouton pour faire l'ACP
  observeEvent(input$DoACP, {
    if (input$NameACP == ""){
      alert("Please enter a name")
      return(0)
    }
    
    if (input$NameACP %in% names(projet$dudi)){
      alert("Name already taken, please enter a new one")
      return(0)
    }
    
    df <- projet$data[[input$DataframeACP]]
    
    tryCatch({ # Essaie de faire l'ACP
      
      if (input$DfRw == "None"){
        row.weight <- rep(1, nrow(df))/nrow(df)
        
        string_rw <- paste("rep(1, nrow(", input$DataframeACP, "))/nrow("
                           , input$DataframeACP, ")",sep = "")
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
      
      if (input$DfCw == "None"){
        col.weight <- rep(1, ncol(df))
        string_cw <- paste("rep(1, ncol(", input$DataframeACP, "))", sep = "")
      }
      
      else{
        if (input$DfCw %in% names(projet$data)){
          col.weight <- projet$data[[input$DfCw]][,input$ColumnCw]
          string_cw <- paste(input$DfCw, "[, '", input$ColumnCw, "']",sep = "")
        }
        
        else{
          col.weight <- projet$dudi[[input$DfCw]][[input$ColumnCw]]
          string_cw <- paste(input$DfCw, "[['", input$ColumnCw, "']]",sep = "")
          
        }
        
      }
    
    
    temp <- dudi.pca(df, nf = input$nfPCA, scannf = F, 
                     center = input$docenterACP, scale = input$doscaleACP, 
                     row.w = row.weight, col.w = col.weight)
    
    projet$dudi[[input$NameACP]] <<- temp
    
    # Ecrit le code pour faire l'acp dans projet$code
    string <- paste(input$NameACP, " <- dudi.pca(", input$DataframeACP, ", nf = ", input$nfPCA, ", scannf = ", F,
                    ", center = ",input$docenterACP, ", scale = ", input$doscaleACP, 
                    ", row.w = ", string_rw, ", col.w = ", string_cw,")", sep = "")
    
    projet$code <<- paste(projet$code, string, sep = "\n\n# Computing PCA\n")
    
    projet$dudi[[input$NameACP]]$call <<- str2lang(substring(string, nchar(input$NameACP) + 5))
    
    }, error = function(e){
      alert("There has been an error (printed in R console)")
      print(e)
      return(0)
    })
    
  })
  
  output$SummaryPCA <- renderPrint({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    if (is.null(input$NameACP))
      return("No dudi object with this name in the project")
    
    if (!(input$NameACP %in% names(projet$dudi)))
      return("No dudi object with this name in the project")
    
    ade4:::summary.dudi(projet$dudi[[input$NameACP]])
    
  })
  
  
  output$outputPCA <- renderDataTable({
    if (length(projet$dudi) == 0)
      return(data.frame(list()))
    
    if (!(input$NameACP %in% names(projet$dudi)))
      return(data.frame(list()))
    
    
    if (input$selectoutputPCA == "Eigenvalues"){
      return(datatable(data.frame(list(values = projet$dudi[[input$NameACP]]$eig))))
    }
    
    else if (input$selectoutputPCA == "Variables")
      dt <- get_pca_var(projet$dudi[[input$NameACP]])
    
    else if (input$selectoutputPCA == "Individuals")
      dt <- get_pca_ind(projet$dudi[[input$NameACP]])
    
    else {
      dt <- projet$dudi[[input$NameACP]]
      dt$cw <- list(col.weight = dt$cw)
      dt$lw <- list(row.weight = dt$lw)
    }
    if (is.null(input$outputPCA2))
      return(data.frame(list()))
    
    datatable(as.data.frame(dt[[input$outputPCA2]]), extensions = c("Buttons"),
              options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
  }, server = F)
  
  
  output$screeplotPCA <- renderPlot({
    if (is.null(projet$dudi[[input$NameACP]]))
      return(0)
    
    ade4:::screeplot.dudi(projet$dudi[[input$NameACP]], main = input$NameACP)
  })
  
}

