cca <- tabItem(tabName = "cca",
                     sidebarLayout(
                       sidebarPanel = sidebarPanel(
                         textInput("NameCCA", "Name to refer the CCA later"),
                         uiOutput("selectDfCCA1"),
                         uiOutput("selectDfCCA2"),
                         numericInput("nfCCA", "Nomber of dimension to keep", 5, 2, 200),
                         actionButton("DoCCA", "Compute CCA", style = "color : white; background-color : #58d68d")
                       ),
                       mainPanel = mainPanel(
                         tabsetPanel(
                           tabPanel("Summary",
                                    verbatimTextOutput("summaryCCA")
                           ), 
                           
                           tabPanel("Output",
                                    uiOutput("selectdatatableCCA"),
                                    dataTableOutput("datatableCCA")
                           ),
                           tabPanel("Plot",
                                    plotOutput("plotCCA")
                           )
                         )
                       )
                     )
)


ccaserver <- function(input, output, session, projet){
  
  output$selectDfCCA1 <- renderUI({
    if (length(projet$data) == 0)
      return("No dataframe present in the project")
    
    selectInput("DfCCA1", "First dataframe (site x species)", 
                choices = names(projet$data),
                selected = input$DfCCA1)
  })
  
  output$selectDfCCA2 <- renderUI({
    if (length(projet$data) == 0)
      return()
    
    if (length(projet$data) == 1)
      return("Only one dataframe present in the project")
    
    selectInput("DfCCA2", "Second dataframe (sites x environmental variables)", 
                choices = names(projet$data),
                selected = input$DfCCA2)
  })
  
  output$selectdatatableCCA <- renderUI({
    if (length(projet$dudi) < 2)
      return()
    
    if (is.null(projet$dudi[[input$NameCCA]]))
      return()
    
    x <- lapply(projet$dudi[[input$NameCCA]], is.data.frame)
    y <- x[which(x == T)]
    
    selectInput("dataframeCCA", "Select a variable", choices = names(y))
    
    
  })
  
  observeEvent(input$DoCCA, {
    if (input$NameCCA == ""){
      alert("Please enter a name")
      return(0)
    }
    
    df1 <- projet$data[[input$DfCCA1]]
    df2 <- projet$data[[input$DfCCA2]]
    
    tryCatch({
      
      temp <- cca(df1, df2 ,nf = input$nfCCA, scannf = F)
      projet$dudi[[input$NameCCA]] <- temp
      
    }, error = function(e){
      alert("There has been an error (printed in the R console)")
      print(e)
      return(0)
    })
    
  })
  
  
  output$summaryCCA <- renderPrint({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    if (is.null(projet$dudi[[input$NameCCA]]))
      return("No dudi object with this name in the project")
    
    ade4:::summary.dudi(projet$dudi[[input$NameCCA]])
  })
  
  
  output$plotCCA <- renderPlot({
    if (length(projet$dudi) == 0)
      return(0)
    
    if (is.null(projet$dudi[[input$NameCCA]]))
      return(0)
    
    corca <- projet$dudi[[input$NameCCA]]
    
    ade4:::plot.pcaiv(corca)
  })
  
  output$datatableCCA <- renderDataTable({
    if (length(projet$dudi) == 0)
      return(0)
    
    if (is.null(projet$dudi[[input$NameCCA]]))
      return(data.frame(list()))
    
    dt <- projet$dudi[[input$NameCCA]]
    
    if (is.null(dt[[input$dataframeCCA]]))
      return(data.frame(list()))
    
    datatable(dt[[input$dataframeCCA]], 
              extensions = c("Buttons"),
              options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
  }, server = F)
  
  
}