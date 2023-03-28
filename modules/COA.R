coa <- tabItem(tabName = "coa",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   textInput("NameCOA", "Name to refer the COA later"),
                   uiOutput("SelectDataframeCOA"),
                   numericInput("nfCOA", "Nomber of dimension to keep", 5, 2, 200),
                   actionButton("DoCOA", "Compute COA")
                 ),
                 mainPanel = mainPanel(
                   tabsetPanel(
                     tabPanel("Screeplot",
                              plotOutput("screeplotCOA")
                     ),
                     
                     tabPanel("Summary",
                              selectInput("selectsummaryCOA", "", 
                                          choices = c("Eigenvalues", "Variables", "Individuals")),
                              uiOutput("selectsummaryCOA2"),
                              dataTableOutput("summaryCOA")),
                     
                     tabPanel("Biplot",
                              plotOutput("biplotCOA"))
                     
                   )
                   
                 )
               )
)


coaServer <- function(input, output, session, projet){
  
  output$SelectDataframeCOA <- renderUI(
    selectInput("DataframeCOA", "Select a dataframe", choices = names(projet$data), selected = input$DataframeCOA)
  )
  
  output$selectsummaryCOA2 <- renderUI({
    if(input$selectsummaryCOA == "Eigenvalues")
      return()
    
    else
      selectInput("summaryCOA2", "Select a value to show", 
                  c("coord", "cos2", "contrib"), 
                  selected = "coord")
    
  })
  
  observeEvent(input$DoCOA, {
    if (input$NameCOA == ""){
      alert("Please enter a name")
      return(0)
    }
    
    df <- projet$data[[input$DataframeCOA]]
    
    tryCatch({
      
      temp <- dudi.coa(df, nf = input$nfCOA, scannf = F)
      projet$dudi[[input$NameCOA]] <- temp
      
    }, error = function(e){
      alert("The dataframe is not suited for a coa analysis")
      return(0)
    })
    
  })
  
  
  output$screeplotCOA <- renderPlot({
    if (is.null(projet$dudi[[input$NameCOA]]))
      return(0)
    
    color_bar <- c(rep("black", projet$dudi[[input$NameCOA]]$nf), 
                   rep("grey", length(projet$dudi[[input$NameCOA]]$eig) - projet$dudi[[input$NameCOA]]$nf))
    
    
    barplot(projet$dudi[[input$NameCOA]]$eig, 
            main = "Screeplot - Eigenvalues", 
            names.arg = 1:length(projet$dudi[[input$NameCOA]]$eig), 
            col = color_bar)
    
  })
  
  
  
  output$summaryCOA <- renderDataTable({
    if (is.null(projet$dudi[[input$NameCOA]]))
      return(0)
    
    if (input$selectsummaryCOA == "Eigenvalues"){
      return(datatable(data.frame(list(values = projet$dudi[[input$NameCOA]]$eig))))
    }
    
    else if (input$selectsummaryCOA == "Variables")
      dt <- get_ca_col(projet$dudi[[input$NameCOA]])
    
    else
      dt <- get_ca_row(projet$dudi[[input$NameCOA]])
    
    datatable(dt[[input$summaryCOA2]], extensions = c("Buttons"),
            options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
    
  }, server = F)
  
  
  output$biplotCOA <- renderPlot({
    if (is.null(projet$dudi[[input$NameCOA]]))
      return(0)
    
    ade4:::biplot.dudi(projet$dudi[[input$NameCOA]])
  })
  
  
}
