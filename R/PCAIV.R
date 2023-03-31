pcaIV <- tabItem(tabName = "pcaiv",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   textInput("NamePCAIV", "Name to refer the PCAIV later"),
                   uiOutput("selectDudiPCAIV"),
                   uiOutput("selectDfPCAIV"),
                   numericInput("nfPCAIV", "Nomber of dimension to keep", 5, 2, 200),
                   actionButton("DoPCAIV", "Compute PCAIV", style = "color : white; background-color : #58d68d")
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
  
  output$selectDudiPCAIV <- renderUI({
    if (length(projet$dudi) == 0)
      return("No dudi object present in the project")
    
    selectInput("DudiPCAIV", "Dudi object", 
                choices = names(projet$dudi),
                selected = input$DudiPCAIV)
  })
  
  output$selectDfPCAIV <- renderUI({
    if (length(projet$data) == 0)
      return()
    
    selectInput("DfPCAIV", "Dataframe with the same rows", 
                choices = names(projet$data),
                selected = input$DfPCAIV)
  })
  
  output$selectdatatablePCAIV <- renderUI({
    if (length(projet$dudi) < 2)
      return()
    
    if (is.null(projet$dudi[[input$NamePCAIV]]))
      return()
    
    x <- lapply(projet$dudi[[input$NamePCAIV]], is.data.frame)
    y <- x[which(x == T)]
    
    selectInput("dataframePCAIV", "Select a variable", choices = names(y))
    
    
  })
  
  observeEvent(input$DoPCAIV, {
    if (input$NamePCAIV == ""){
      alert("Please enter a name")
      return(0)
    }
    
    dudi <- projet$dudi[[input$DudiPCAIV]]
    df <- projet$data[[input$DfPCAIV]]
    
    tryCatch({
      
      temp <- pcaiv(dudi, df ,nf = input$nfPCAIV, scannf = F)
      projet$dudi[[input$NamePCAIV]] <- temp
      
    }, error = function(e){
      alert("There has been an error (printed in the R console)")
      print(e)
      return(0)
    })
    
  })
  
  
  output$summaryPCAIV <- renderPrint({
    if (is.null(projet$dudi[[input$NamePCAIV]]))
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
    
    if (is.null(dt[[input$dataframePCAIV]]))
      return(data.frame(list()))
    
    datatable(dt[[input$dataframePCAIV]], 
              extensions = c("Buttons"),
              options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
  }, server = F)
  
  
}
