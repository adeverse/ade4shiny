coa <- tabItem(tabName = "coa",
               h2("Correspondence Analysis (COA)"),
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   uiOutput("selectizeCOA"),
                   uiOutput("SelectDataframeCOA"),
                   ## input and help text number of dimensions
                   numericInput("nfCOA",
                                label = tags$span("Number of dimension to keep", 
                                                  bsButton("helpnfcoa", label = "",
                                                           icon = icon("question-circle" )
                                                           , size = "extra-small")),
                                2, 2, 200),
                   bsPopover(id = "helpnfcoa",
                             title = "",
                             content = paste0(
                               "Number of axes of variance (dimensions) to keep. See more: ",
                               a("dudi.coa()", href = "http://sdray.github.io/ade4/reference/dudi.coa.html", target="_blank")),
                             placement = "right",
                             trigger = c("hover", "focus"),
                             options = list(container = "body")),
                   actionButton("DoCOA", "Compute COA", style = "color : white; background-color :  #93bf29")
                 ),
                 mainPanel = mainPanel(
                   tabsetPanel(
                     tabPanel("Summary",
                              verbatimTextOutput("summaryCOA")
                     ),
                     
                     tabPanel("Output",
                              selectInput("selectoutputCOA", "", 
                                          choices = c("Eigenvalues", "Variables", "Individuals", "Dudi object")),
                              uiOutput("selectoutputCOA2"),
                              dataTableOutput("outputCOA")),
                     
                     tabPanel("Screeplot",
                              plotOutput("screeplotCOA"))
                     
                   )
                   
                 )
               )
)


coaServer <- function(input, output, session, projet){
  
  output$selectizeCOA <- renderUI({
    all_COA <- sapply(names(projet$dudi), function(x){
      if ("coa" %in% class(projet$dudi[[x]]))
        return(x)
    })
    
    if (length(all_COA) == 0)
      selectizeInput("NameCOA",
                     label = tags$span("Analysis name ",
                                       popify(el = bsButton("help_name_coa", label = "", icon = icon("question-circle"), size = "extra-small"),
                                              title = "",
                                              content = "Type in a new name to compute a new COA or select a previous COA from the list to display its results.",
                                              placement = "right", trigger = c("hover", "focus"),
                                              options = list(container = "body"))
                                       ), 
                     choices = all_COA, options = list(create = TRUE))
    
    else{
      last <- all_COA[length(all_COA)]
      selectizeInput("NameCOA",
                     label = tags$span("Analysis name ",
                                       popify(el = bsButton("help_name_coa2", label = "", icon = icon("question-circle"), size = "extra-small"),
                                              title = "",
                                              content = "Type in a new name to compute a new COA or select a previous COA from the list to display its results.",
                                              placement = "right", trigger = c("hover", "focus"),
                                              options = list(container = "body"))
                     ), 
                     choices = all_COA, 
                     options = list(create = TRUE), selected = last)
    }
    
  })
  
  output$SelectDataframeCOA <- renderUI({
    selectInput("DataframeCOA",
                #  label = "Select a dataframe ",
                label = tags$span("Select a dataframe ",
                                  # helpPopup(title = NULL, content = paste0("A dataframe previously loaded in the app via 'Manage data' containing positive or null values. See more: ",
                                  #                          a("dudi.coa()", href = "http://sdray.github.io/ade4/reference/dudi.coa.html", target="_blank")),
                                  #           trigger = c("hover", "focus"))
                                  shinyBS::popify(el = bsButton("HdfCOA", label = "", icon = icon("question-circle"), size = "extra-small"),
                                                  title = "",
                                                  content = paste0("A dataframe previously loaded in the app containing positive or null values. See more: ",
                                                                   a("dudi.coa()", href = "http://sdray.github.io/ade4/reference/dudi.coa.html", target="_blank")),
                                                  placement = "right", trigger = c("hover", "focus"),
                                                  options = list(container = "body"))
                                  ),
                                  choices = names(projet$data), selected = input$DataframeCOA)
  })
  
  output$selectoutputCOA2 <- renderUI({
    if (is.null(input$selectoutputCOA))
      return("")
    
    if(input$selectoutputCOA == "Eigenvalues")
      return()
    
    else if (input$selectoutputCOA == "Dudi object"){
      if (is.null(input$NameCOA))
        return("")
      
      if (!(input$NameCOA %in% names(projet$dudi)))
        return("")
      
      remove <- c("eig", "nf", "call", "N", "rank")
      name <- names(projet$dudi[[input$NameCOA]])
      keep <- name[! name %in% remove]
      
      selectInput("outputCOA2", "select a value to show",
                  choices = keep)
      
    }
    
    else
      selectInput("outputCOA2", "Select a value to show", 
                  c("coord", "cos2", "contrib", "inertia"))
    
  })
  
  observeEvent(input$DoCOA, {
    if (input$NameCOA == ""){
      alert("Please enter a name")
      return(0)
    }
    
    if (input$NameCOA %in% names(projet$dudi)){
      alert("Name already taken, please enter a new one")
      return(0)
    }
    
    df <- projet$data[[input$DataframeCOA]]
    
    tryCatch({
      
      temp <- dudi.coa(df, nf = input$nfCOA, scannf = F)
      
      projet$dudi[[input$NameCOA]] <- temp
      
      string <- paste(input$NameCOA, " <- dudi.coa(", input$DataframeCOA, 
                      ", nf = ", input$nfCOA, ", scannf = ", F,")", sep = "")
      
      projet$code <- paste(projet$code, string, sep = "\n\n# Computing COA\n")
      
      projet$dudi[[input$NameCOA]]$call <- substring(string, nchar(input$NameCOA) + 5)
      
    }, error = function(e){
      alert("There has been an error (printed in R console)")
      return(0)
    })
    
  })
  
  output$summaryCOA <- renderPrint({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    if (is.null(input$NameCOA))
      return("No dudi object with this name in the project")
    
    if (!(input$NameCOA %in% names(projet$dudi)))
      return("No dudi object with this name in the project")
    
    ade4:::summary.dudi(projet$dudi[[input$NameCOA]])
    
  })
  
  output$outputCOA <- renderDataTable({
    if (length(projet$dudi) == 0)
      return(data.frame(list()))
    
    if (!(input$NameCOA %in% names(projet$dudi)))
      return(data.frame(list()))
    
    if (input$selectoutputCOA == "Eigenvalues")
      return(datatable(data.frame(list(values = projet$dudi[[input$NameCOA]]$eig))))
    
    else if (input$selectoutputCOA == "Variables")
      dt <- get_ca_col(projet$dudi[[input$NameCOA]])
    
    else if (input$selectoutputCOA == "Individuals")
      dt <- get_ca_row(projet$dudi[[input$NameCOA]])
    
    else{
      dt <- projet$dudi[[input$NameCOA]]
      dt$cw <- list(col.weight = dt$cw)
      dt$lw <- list(row.weight = dt$lw)
    }
    
    if (is.null(input$outputCOA2))
      return(data.frame(list()))
    
    datatable(as.data.frame(dt[[input$outputCOA2]]), extensions = c("Buttons"),
            options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
    
  }, server = F)
  
  
  output$screeplotCOA <- renderPlot({
    if (is.null(projet$dudi[[input$NameCOA]]))
      return(0)
    
    ade4:::screeplot.dudi(projet$dudi[[input$NameCOA]], main = input$NameCOA)
  })
  
  
  # output$screeplotCOA <- renderPlot({
  #   if (is.null(projet$dudi[[input$NameCOA]]))
  #     return(0)
  #   
  #   color_bar <- c(rep("black", projet$dudi[[input$NameCOA]]$nf), 
  #                  rep("grey", length(projet$dudi[[input$NameCOA]]$eig) - projet$dudi[[input$NameCOA]]$nf))
  #   
  #   
  #   barplot(projet$dudi[[input$NameCOA]]$eig, 
  #           main = "Screeplot - Eigenvalues", 
  #           names.arg = 1:length(projet$dudi[[input$NameCOA]]$eig), 
  #           col = color_bar)
  #   
  # })
  
  
}


