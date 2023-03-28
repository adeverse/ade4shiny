coinertie <- tabItem(tabName = "coinertia",
                     sidebarLayout(
                       sidebarPanel = sidebarPanel(
                         textInput("NameCoinertia", "Name to refer the coinertia later"),
                         uiOutput("selectDudiCoinertia1"),
                         uiOutput("selectDudiCoinertia2"),
                         numericInput("nfCoinertia", "Nomber of dimension to keep", 5, 2, 200),
                         actionButton("DoCoinertia", "Compute coinertia")
                       ),
                       mainPanel = mainPanel(
                         tabsetPanel(
                          tabPanel("Summary",
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
    
    x <- lapply(coin1, is.data.frame)
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
  
  
  output$plotCoinertia <- renderPlot({
    if (length(projet$dudi) == 0)
      return(0)
    
    if (is.null(projet$dudi[[input$NameCoinertia]]))
      return(0)
    
    coin <- projet$dudi[[input$NameCoinertia]]
    
    g1 <- s.arrow(coin$l1, plab.cex = 0.7, plot = FALSE)
    g2 <- s.arrow(coin$c1, plab.cex = 0.7, plot = FALSE)
    g3 <- s.corcircle(coin$aX, plot = FALSE)
    g4 <- s.corcircle(coin$aY, plot = FALSE)
    ADEgS(list(g1, g2, g3, g4), layout = c(2, 2))
    
  })
  
  output$datatableCoinertia <- renderDataTable({
    if (length(projet$dudi) == 0)
      return(0)
    
    if (is.null(projet$dudi[[input$NameCoinertia]]))
      return(data.frame(list()))
    
    datatable(projet$dudi[[input$NameCoinertia]][[input$dataframeCoinertia]], 
              extensions = c("Buttons"),
              options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
  }, server = F)
  
  
}