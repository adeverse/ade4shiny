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
                                          choices = c("Eigenvalues", "Variables", "Individuals")),
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
    
    else if (input$selectoutputPCO == "Variables")
      selectInput("outputPCO2", "Select a value to show", 
                  c("coord", "cos2", "contrib"))
    
    else
      selectInput("outputPCO2", "Select a value to show", 
                  c("coord", "cos2", "contrib"))
    
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
    
    df <- projet$data[[input$DataframePCO]]
    
    tryCatch({
      
      temp <- dudi.pco(df, nf = input$nfPCO, scannf = F, 
                       center = input$docenterPCO, scale = input$doscalePCO)
      projet$dudi[[input$NamePCO]] <- temp
      
      string <- paste(input$NamePCO, " <- dudi.pco(", input$DataframePCO, ", nf = ", input$nfPCO, ", scannf = ", F,
                      ", center = ",input$docenterPCO, ", scale = ", input$doscalePCO, ")", sep = "")
      
      projet$code <- paste(projet$code, string, sep = "\n\n# Computing PCO\n")
      
      projet$dudi[[input$NamePCO]]$call <- substring(string, nchar(input$NamePCO) + 5)
      
    }, error = function(e){
      alert("There has been an error (printed in R console)")
      print(e)
      return(0)
    })
    
  })
  
  output$SummaryPCO <- renderPrint({
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
    
    if (input$selectoutputPCO == "Eigenvalues"){
      return(datatable(data.frame(list(values = projet$dudi[[input$NamePCO]]$eig))))
    }
    
    else if (input$selectoutputPCO == "Variables")
      dt <- get_pco_var(projet$dudi[[input$NamePCO]])
    
    else
      dt <- get_pco_ind(projet$dudi[[input$NamePCO]])
    
    if (is.null(input$outputPC02))
      return(data.frame(list()))
    
    datatable(dt[[input$outputPCO2]], extensions = c("Buttons"),
              options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
  }, server = F)
  
  
  output$screeplotPCO <- renderPlot({
    if (is.null(projet$dudi[[input$NamePCO]]))
      return(0)
    
    ade4:::screeplot.dudi(projet$dudi[[input$NamePCO]], main = input$NamePCO)
  })
  
}