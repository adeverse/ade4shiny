mca <- tabItem(tabName = "mca",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   textInput("NameMCA", "Name to refer the MCA later"),
                   uiOutput("SelectDataframeMCA"),
                   numericInput("nfMCA", "Number of dimension to keep", 5, 2, 200),
                   actionButton("DoMCA", "Compute MCA", style = "color : white; background-color : #58d68d")
                 ),
                 mainPanel = mainPanel(
                   tabsetPanel(
                     tabPanel("Summary",
                              verbatimTextOutput("SummaryMCA")
                     ),
                     
                     tabPanel("Output",
                              selectInput("selectoutputMCA", "", 
                                          choices = c("Eigenvalues", "Variables", "Individuals")),
                              uiOutput("selectoutputMCA2"),
                              dataTableOutput("outputMCA")),
                     
                     tabPanel("Plot",
                              plotOutput("plotMCA"))
                     
                   )
                   
                 )
               )
)



mcaServer <- function(input, output, session, projet){
  
  output$SelectDataframeMCA <- renderUI(
    selectInput("DataframeMCA", "Select a dataframe", choices = names(projet$data), selected = input$DataframeMCA)
  )
  
  output$selectoutputMCA2 <- renderUI({
    if(input$selectoutputMCA == "Eigenvalues")
      return()
    
    else if (input$selectoutputMCA == "Variables")
      selectInput("outputMCA2", "Select a value to show", 
                  c("coord", "cos2", "contrib"))
    
    else
      selectInput("outputMCA2", "Select a value to show", 
                  c("coord", "cos2", "contrib"))
    
  })
  
  observeEvent(input$DoMCA, {
    if (input$NameMCA == ""){
      alert("Please enter a name")
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
      alert("The dataframe is not suited for a mca analysis")
      print(e)
      return(0)
    })
    
  })
  
  output$SummaryMCA <- renderPrint({
    if (is.null(projet$dudi[[input$NameMCA]]))
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
    
    else
      dt <- get_mca_ind(projet$dudi[[input$NameMCA]])
    
    datatable(dt[[input$outputMCA2]], extensions = c("Buttons"),
              options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
  }, server = F)
  
  
  output$plotMCA <- renderPlot({
    if (is.null(projet$dudi[[input$NameMCA]]))
      return(0)
    
    ade4:::screeplot.dudi(projet$dudi[[input$NameMCA]], main = input$NameMCA)
  })
   
}