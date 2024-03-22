pco <- tabItem(tabName = "pco",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   uiOutput("selectizePCO"),
                   uiOutput("SelectDataframePCO"),
                   numericInput("nfPCO", "Number of dimension to keep", 2, 2, 200),
                   actionButton("DoPCO", "Compute PCO", style = "color : white; background-color : #58d68d")
                 ),
                 mainPanel = mainPanel(
                   tabsetPanel(
                     tabPanel("Summary",
                              verbatimTextOutput("summaryPCO")
                     ),
                     
                     tabPanel("Output",
                              selectInput("selectoutputPCO", "", 
                                          choices = c("Eigenvalues", "Items")),
                              uiOutput("selectoutputPCO2"),
                              dataTableOutput("outputPCO")),
                     
                     tabPanel("Screeplot",
                              plotOutput("screeplotPCO"))
                     
                   )
                   
                 )
               )
)



pcoServer <- function(input, output, session, projet){
  
  output$selectizePCO <- renderUI({
    all_PCO <- sapply(names(projet$dudi), function(x){
      if ("pco" %in% class(projet$dudi[[x]]))
        return(x)
    })
    
    if (length(all_PCO) == 0)
      selectizeInput("NamePCO", "Name to refer the PCO later", 
                     choices = all_PCO, options = list(create = TRUE))
    
    else{
      last <- all_PCO[length(all_PCO)]
      selectizeInput("NamePCO", "Name to refer the PCO later", choices = all_PCO, 
                     options = list(create = TRUE), selected = last)
    }

  })
  
  output$SelectDataframePCO <- renderUI(
    selectInput("DataframePCO", "Select a dataframe", choices = names(projet$data), selected = input$DataframePCO)
  )
  
  output$selectoutputPCO2 <- renderUI({
    if(input$selectoutputPCO == "Eigenvalues")
      return()
    
    else if (input$selectoutputPCO == "Items")
      selectInput("outputPCO2", "Select a value to show", 
                  c("coord"))
    
  })
  
  observeEvent(input$DoPCO, {
    if (input$NamePCO == ""){
      alert("Please enter a name")
      return(0)
    }
    
    if (input$NamePCO %in% names(projet$dudi)){
      alert("Name already taken, please enter a new one")
      return(0)
    }
    
    tryCatch({
	  dm <- as.dist(projet$data[[input$DataframePCO]])
    }, error = function(e){
      alert("There has been an error (printed in R console)")
      print(e)
      return(0)
    })
    
    if (!is.euclid(dm)) {
      alert("Dist matrix is not Euclidean, trying quasieuclid")
      dm <- quasieuclid(dm)
    }

    tryCatch({
      
      temp <- dudi.pco(d = dm, nf = input$nfPCO, scannf = F)
      projet$dudi[[input$NamePCO]] <<- temp
      
      string <- paste(input$NamePCO, " <- dudi.pco(d = dm, nf = ", input$nfPCO, ", scannf = ", F, ")", sep = "")
      
      projet$code <- paste(projet$code, string, sep = "\n\n# Computing PCO\n")
      
      projet$dudi[[input$NamePCO]]$call <- str2lang(substring(string, nchar(input$NamePCO) + 5))
      
    }, error = function(e){
      alert("There has been an error (printed in R console)")
      print(e)
      return(0)
    })
    
  })
  
  output$summaryPCO <- renderPrint({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    if (is.null(input$NamePCO))
      return("No dudi object with this name in the project")
    
    if (!(input$NamePCO %in% names(projet$dudi)))
      return("No dudi object with this name in the project")
    
    ade4:::summary.dudi(projet$dudi[[input$NamePCO]])
    
  })
  
  
  output$outputPCO <- renderDataTable({
    if (is.null(projet$dudi[[input$NamePCO]]))
      return(data.frame(list()))
    
    if (input$selectoutputPCO == "Eigenvalues")
      return(datatable(data.frame(list(values = projet$dudi[[input$NamePCO]]$eig))))
    
    else if (input$selectoutputPCO == "Items")
      return(datatable(data.frame(list(values = projet$dudi[[input$NamePCO]]$li))))
    
  }, server = F)
  
  
  output$screeplotPCO <- renderPlot({
    if (is.null(projet$dudi[[input$NamePCO]]))
      return(0)
    
    ade4:::screeplot.dudi(projet$dudi[[input$NamePCO]], main = input$NamePCO)
  })
  
}