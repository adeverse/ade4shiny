mca <- tabItem(tabName = "mca",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   uiOutput("selectizeMCA"),
                   uiOutput("SelectDataframeMCA"),
                   numericInput("nfMCA",
                                label = tags$span("Number of dimension to keep", 
                                                  bsButton("helpnfmca", label = "",
                                                           icon = icon("question-circle" )
                                                           , size = "extra-small")),
                                5, 2, 200),
                   bsPopover(id = "nfMCA",
                             title = "",
                             content = paste0(
                               "Number of axes of variance (dimensions) to keep. See more: ",
                               a("dudi.acm()", href = "http://sdray.github.io/ade4/reference/dudi.acm.html", target="_blank")),
                             placement = "right",
                             trigger = "click",
                             options = list(container = "body")),
                   actionButton("DoMCA", "Compute MCA", style = "color : white; background-color :  #93bf29")
                 ),
                 mainPanel = mainPanel(
                   tabsetPanel(
                     tabPanel("Summary",
                              verbatimTextOutput("SummaryMCA")
                     ),
                     
                     tabPanel("Output",
                              selectInput("selectoutputMCA", "", 
                                          choices = c("Eigenvalues", "Variables", "Individuals", "Dudi Object")),
                              uiOutput("selectoutputMCA2"),
                              dataTableOutput("outputMCA")),
                     
                     tabPanel("Plot",
                              plotOutput("plotMCA"))
                     
                   )
                   
                 )
               )
)



mcaServer <- function(input, output, session, projet){
  
  output$selectizeMCA <- renderUI({
    all_MCA <- sapply(names(projet$dudi), function(x){
      if ("acm" %in% class(projet$dudi[[x]]))
        return(x)
    })
    
    if (length(all_MCA) == 0)
      selectizeInput("NameMCA",
                     label = tags$span("MCA name ",
                                       popify(el = bsButton("help_name_mca1", label = "", icon = icon("question-circle"), size = "extra-small"),
                                              title = "",
                                              content = "Type in a new name to compute a new MCA or select a previous one from the list to display its results",
                                              placement = "right", trigger = "click",
                                              options = list(container = "body"))
                     ), 
                     choices = all_MCA, options = list(create = TRUE))
    
    else{
      last <- all_MCA[length(all_MCA)]
      selectizeInput("NameMCA",
                     label = tags$span("MCA name ",
                                       popify(el = bsButton("help_name_mca", label = "", icon = icon("question-circle"), size = "extra-small"),
                                              title = "",
                                              content = "Type in a new name to compute a new MCA or select a previous one from the list to display its results",
                                              placement = "right", trigger = "click",
                                              options = list(container = "body"))
                     ), 
                     choices = all_MCA, 
                     options = list(create = TRUE), selected = last)
    }
  })
  
  output$SelectDataframeMCA <- renderUI(
    selectInput("DataframeMCA",
                label = tags$span("Select a dataframe ",
                                  shinyBS::popify(el = bsButton("help_dfmca", label = "", icon = icon("question-circle"), size = "extra-small"),
                                                  title = "",
                                                  content = paste0("A dataframe (previously loaded in the app) containing only factors. See more: ",
                                                                   a("dudi.acm()", href = "http://sdray.github.io/ade4/reference/dudi.acm.html", target="_blank")),
                                                  placement = "right", trigger = "click",
                                                  options = list(container = "body"))
                ),
                choices = names(projet$data), selected = input$DataframeMCA)
  )
  
  output$selectoutputMCA2 <- renderUI({
    if(input$selectoutputMCA == "Eigenvalues")
      return()
    
    else if (input$selectoutputMCA == "Variables")
      selectInput("outputMCA2", "Select a value to show", 
                  c("coord", "cos2", "contrib"))
    
    else if (input$selectoutputMCA == "Individuals")
      selectInput("outputMCA2", "Select a value to show", 
                  c("coord", "cos2", "contrib"))
    
    else {
      if (is.null(input$selectoutputMCA))
        return("")
      
      if (is.null(input$NameMCA))
        return("")
      
      if (!(input$NameMCA %in% names(projet$dudi)))
        return("")
      
      remove <- c("eig", "rank", "nf", "call")
      name <- names(projet$dudi[[input$NameMCA]])
      keep <- name[! name %in% remove]
      
      selectInput("outputMCA2", "select a value to show",
                  keep)
    }
      
    
  })
  
  observeEvent(input$DoMCA, {
    if (input$NameMCA == ""){
      alert("Please enter a name")
      return(0)
    }
    
    if (input$NameMCA %in% names(projet$dudi)){
      alert("Name already taken, please enter a new one")
      return(0)
    }
    
    df <- projet$data[[input$DataframeMCA]]
    
    tryCatch({
      
      temp <- dudi.acm(df, nf = input$nfMCA, scannf = FALSE)
      
      projet$dudi[[input$NameMCA]] <- temp
      
      string <- paste(input$NameMCA, " <- dudi.acm(", input$DataframeMCA, 
                      ", nf = ", input$nfMCA, ", scannf = ", F,")", sep = "")
      
      projet$code <- paste(projet$code, string, sep = "\n\n#Computing MCA\n")
      
      projet$dudi[[input$NameMCA]]$call <- substring(string, nchar(input$NameMCA) + 5)
      
    }, error = function(e){
      alert("There has been an error (printed in R console)")
      print(e)
      return(0)
    })
    
  })
  
  output$SummaryMCA <- renderPrint({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    if (is.null(input$NameMCA))
      return("No dudi object with this name in the project")
    
    if (!(input$NameMCA %in% names(projet$dudi)))
      return("No dudi object with this name in the project")
    
    ade4:::summary.dudi(projet$dudi[[input$NameMCA]])
    
  })
  
  
  output$outputMCA <- renderDataTable({
    if (is.null(projet$dudi[[input$NameMCA]]))
      return(data.frame(list()))
    
    if (input$selectoutputMCA == "Eigenvalues"){
      return(datatable(data.frame(list(values = projet$dudi[[input$NameMCA]]$eig))))
    }
    
    else if (input$selectoutputMCA == "Variables")
      dt <- get_mca_var(projet$dudi[[input$NameMCA]])
    
    else if (input$selectoutputMCA == "Individuals")
      dt <- get_mca_ind(projet$dudi[[input$NameMCA]])
    
    
    else{
      dt <- projet$dudi[[input$NameMCA]]
      dt$cw <- list(col.weight = dt$cw)
      dt$lw <- list(row.weight = dt$lw)
    }
    
    if (is.null(input$outputMCA2))
      return(data.frame(list()))
    
    datatable(as.data.frame(dt[[input$outputMCA2]]), extensions = c("Buttons"),
              options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
  }, server = F)
  
  
  output$plotMCA <- renderPlot({
    if (is.null(projet$dudi[[input$NameMCA]]))
      return(0)
    
    ade4:::screeplot.dudi(projet$dudi[[input$NameMCA]], main = input$NameMCA)
  })
   
}
