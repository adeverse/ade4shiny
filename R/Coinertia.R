coinertie <- tabItem(tabName = "coinertia",
                     sidebarLayout(
                       sidebarPanel = sidebarPanel(
                         textInput("NameCoinertia", "Name to refer the coinertia later"),
                         uiOutput("selectDudiCoinertia1"),
                         uiOutput("selectDudiCoinertia2"),
                         numericInput("nfCoinertia", "Nomber of dimension to keep", 5, 2, 200),
                         actionButton("DoCoinertia", "Compute coinertia", style = "color : white; background-color : #58d68d")
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
  
  output$selectDudiCoinertia1 <- renderUI({
    if (length(projet$dudi) == 0)
      return("No dudi present in the project")
    
    selectInput("DudiCoinertia1", "First dudi object", 
                choices = names(projet$dudi),
                selected = input$DudiCoinertia1)
  })
  
  output$selectDudiCoinertia2 <- renderUI({
    if (length(projet$dudi) == 0)
      return()
    
    if (length(projet$dudi) == 1)
      return("Only one dudi present in the project")
    
    selectInput("DudiCoinertia2", "Second dudi object", 
                choices = names(projet$dudi),
                selected = input$DudiCoinertia2)
  })
  
  output$selectdatatableCoinertia <- renderUI({
    if (length(projet$dudi) < 2)
      return()
    
    if (is.null(projet$dudi[[input$NameCoinertia]]))
      return()
    
    x <- lapply(projet$dudi[[input$NameCoinertia]], is.data.frame)
    y <- x[which(x == T)]
    
    selectInput("dataframeCoinertia", "Select a variable", choices = names(y))
    
    
  })
  
  observeEvent(input$DoCoinertia, {
    if (input$NameCoinertia == ""){
      alert("Please enter a name")
      return(0)
    }
    
    dud1 <- projet$dudi[[input$DudiCoinertia1]]
    dud2 <- projet$dudi[[input$DudiCoinertia2]]
    
    if (identical(dud1$lw, dud2$lw) == FALSE)
      alert("Be careful, the row weight are different")
    
    tryCatch({
      
      temp <- coinertia(dud1, dud2 ,nf = input$nfCoinertia, scannf = F)
      projet$dudi[[input$NameCoinertia]] <- temp
      
    }, error = function(e){
      alert("There has been an error (printed in the R console)")
      print(e)
      return(0)
    })
    
  })
  
  
  output$summaryCoinertia <- renderPrint({
    if (is.null(projet$dudi[[input$NameCoinertia]]))
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
    
    if (is.null(dt[[input$dataframeCoinertia]]))
      return(data.frame(list()))
    
    datatable(dt[[input$dataframeCoinertia]], 
              extensions = c("Buttons"),
              options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
  }, server = F)
  
  
}