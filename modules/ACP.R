acp <- tabItem(tabName = "pca",
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      textInput("NameACP", "Name to refer the PCA later"),
      uiOutput("SelectDataframeACP"),
      numericInput("nfPCA", "Nomber of dimension to keep", 5, 2, 200),
      checkboxInput("docenterACP", "Center", value = T),
      checkboxInput("doscaleACP", "Scale", value = T),
      actionButton("DoACP", "Compute ACP")
    ),
    mainPanel = mainPanel(
      tabsetPanel(
        tabPanel("Screeplot",
          plotOutput("screeplotPCA")
        ),
        
        tabPanel("Summary",
                 selectInput("selectsummaryPCA", "", 
                             choices = c("Eigenvalues", "Variables", "Individuals")),
                 uiOutput("selectsummaryPCA2"),
                 dataTableOutput("summaryPCA")),
        
        tabPanel("Corcircle",
          plotOutput("corcirclePCA")
        ),
        
        tabPanel("Graph of individuals",
                 plotOutput("goiPCA")),
        
        tabPanel("Biplot",
                 plotOutput("biplotPCA"))
        
      )
    
    )
  )
)



acpServer <- function(input, output, session, projet){
  
  output$SelectDataframeACP <- renderUI(
    selectInput("DataframeACP", "Select a dataframe", choices = names(projet$data), selected = input$DataframeACP)
  )
  
  output$selectsummaryPCA2 <- renderUI({
    if(input$selectsummaryPCA == "Eigenvalues")
      return()
    
    else if (input$selectsummaryPCA == "Variables")
      selectInput("summaryPCA2", "Select a value to show", 
                  c("coord", "cor", "cos2", "contrib"))
    
    else
      selectInput("summaryPCA2", "Select a value to show", 
                  c("coord", "cos2", "contrib"))
    
  })
  
  observeEvent(input$DoACP, {
    if (input$NameACP == ""){
      alert("Please enter a name")
      return(0)
    }
    
    df <- projet$data[[input$DataframeACP]]
    
    tryCatch({
    
    temp <- dudi.pca(df, nf = input$nfPCA, scannf = F, 
                     center = input$docenterACP, scale = input$doscaleACP)
    projet$dudi[[input$NameACP]] <- temp
    
    }, error = function(e){
      alert("The dataframe is not suited for a pca analysis")
      print(e)
      return(0)
    })
    
  })
  
  
  output$screeplotPCA <- renderPlot({
    if (is.null(projet$dudi[[input$NameACP]]))
      return(0)
    
    color_bar <- c(rep("black", projet$dudi[[input$NameACP]]$nf), 
                   rep("grey", length(projet$dudi[[input$NameACP]]$eig) - projet$dudi[[input$NameACP]]$nf))
    
    #adegraphics:::screeplot.dudi(projet$dudi[[input$NameACP]])
    
    barplot(projet$dudi[[input$NameACP]]$eig, 
            main = "Screeplot - Eigenvalues", 
            names.arg = 1:length(projet$dudi[[input$NameACP]]$eig), 
             col = color_bar)
    
  })
  
  
  
  output$summaryPCA <- renderDataTable({
    if (is.null(projet$dudi[[input$NameACP]]))
      return(0)
    
    if (input$selectsummaryPCA == "Eigenvalues"){
      return(datatable(data.frame(list(values = projet$dudi[[input$NameACP]]$eig))))
      }
      
    else if (input$selectsummaryPCA == "Variables")
      dt <- get_pca_var(projet$dudi[[input$NameACP]])
    
    else
      dt <- get_pca_ind(projet$dudi[[input$NameACP]])
    
    datatable(dt[[input$summaryPCA2]], extensions = c("Buttons"),
              options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
  }, server = F)
  
  
  output$corcirclePCA <- renderPlot({
    if (is.null(projet$dudi[[input$NameACP]]))
      return(0)
    
    if (identical(projet$dudi[[input$NameACP]]$norm[1:length(projet$dudi[[input$NameACP]]$norm)], 
                  rep(1, length(projet$dudi[[input$NameACP]]$norm))))
      return(s.arrow(projet$dudi[[input$NameACP]]$co))
    
    else
      return(s.corcircle(projet$dudi[[input$NameACP]]$co))
    
  })
  
  output$goiPCA <- renderPlot({
    if (is.null(projet$dudi[[input$NameACP]]))
      return(0)
    
    s.label(projet$dudi[[input$NameACP]]$li, 
            xax = 1,     # Dimension 1
            yax = 2)     # Dimension 2
    
  })
  
  
  output$biplotPCA <- renderPlot({
    if (is.null(projet$dudi[[input$NameACP]]))
      return(0)
    
    ade4:::biplot.dudi(projet$dudi[[input$NameACP]])
  })
  
  
}