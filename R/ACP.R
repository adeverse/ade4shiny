acp <- tabItem(tabName = "pca",
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      uiOutput("selectizeACP"),
      uiOutput("SelectDataframeACP"),
      numericInput("nfPCA", "Nomber of dimension to keep", 5, 2, 200),
      checkboxInput("docenterACP", "Center", value = T),
      checkboxInput("doscaleACP", "Scale", value = T),
      checkboxInput("uniformrw", "Uniform row weight", value = T),
      checkboxInput("uniformcw", "Uniform col weight", value = T),
      uiOutput("FileUniRW"),
      uiOutput("FileUniCW"),
      actionButton("DoACP", "Compute ACP", style = "color : white; background-color : #58d68d")
    ),
    mainPanel = mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 verbatimTextOutput("SummaryPCA")
                 ),
        
        tabPanel("Output",
                 selectInput("selectoutputPCA", "", 
                             choices = c("Eigenvalues", "Variables", "Individuals")),
                 uiOutput("selectoutputPCA2"),
                 dataTableOutput("outputPCA")),
        
        tabPanel("Plot",
                 plotOutput("screeplotPCA"))
        
      )
    
    )
  )
)



acpServer <- function(input, output, session, projet){
  
  output$selectizeACP <- renderUI({
    all_PCA <- sapply(names(projet$dudi), function(x){
      if ("pca" %in% class(projet$dudi[[x]]))
        return(x)
    })
    
    if (length(all_PCA) == 0)
      selectizeInput("NameACP", "Name to refer the PCA later", choices = all_PCA, 
                     options = list(create = TRUE))
    
    else{
      last <- all_PCA[length(all_PCA)]
      selectizeInput("NameACP", "Name to refer the PCA later", choices = all_PCA, 
                   options = list(create = TRUE), selected = last)
    }
    
  })
  
  
  output$FileUniRW <- renderUI({
    if (input$uniformrw)
      return("")
    
    fileInput("rw", label = "Row weight file")
    
  })
  
  output$FileUniCW <- renderUI({
    if (input$uniformcw == F)
      return("")
    
    fileInput("cw", label = "Col weight file")
    
  })
  
  
  output$SelectDataframeACP <- renderUI(
    selectInput("DataframeACP", "Select a dataframe", choices = names(projet$data), selected = input$DataframeACP)
  )
  
  output$selectoutputPCA2 <- renderUI({
    if(input$selectoutputPCA == "Eigenvalues")
      return()
    
    else if (input$selectoutputPCA == "Variables")
      selectInput("outputPCA2", "Select a value to show", 
                  c("coord", "cor", "cos2", "contrib"))
    
    else
      selectInput("outputPCA2", "Select a value to show", 
                  c("coord", "cos2", "contrib"))
    
  })
  
  observeEvent(input$DoACP, {
    if (input$NameACP == ""){
      alert("Please enter a name")
      return(0)
    }
    
    if (input$NameACP %in% names(projet$dudi)){
      alert("Name already taken, please enter a new one")
      return(0)
    }
    
    df <- projet$data[[input$DataframeACP]]
    
    tryCatch({
    
    temp <- dudi.pca(df, nf = input$nfPCA, scannf = F, 
                     center = input$docenterACP, scale = input$doscaleACP)
    
    projet$dudi[[input$NameACP]] <- temp
    
    string <- paste(input$NameACP, " <- dudi.pca(", input$DataframeACP, ", nf = ", input$nfPCA, ", scannf = ", F,
                    ", center = ",input$docenterACP, ", scale = ", input$doscaleACP, ")", sep = "")
    
    projet$code <- paste(projet$code, string, sep = "\n\n# Computing PCA\n")
    
    projet$dudi[[input$NameACP]]$call <- substring(string, nchar(input$NameACP) + 5)
    
    }, error = function(e){
      alert("The dataframe is not suited for a pca analysis")
      print(e)
      return(0)
    })
    
  })
  
  output$SummaryPCA <- renderPrint({
    if (is.null(projet$dudi[[input$NameACP]]))
      return("No dudi object with this name in the project")
    
    ade4:::summary.dudi(projet$dudi[[input$NameACP]])
    
  })
  
  
  output$outputPCA <- renderDataTable({
    if (is.null(projet$dudi[[input$NameACP]]))
      return(data.frame(list()))
    
    if (input$selectoutputPCA == "Eigenvalues"){
      return(datatable(data.frame(list(values = projet$dudi[[input$NameACP]]$eig))))
    }
    
    else if (input$selectoutputPCA == "Variables")
      dt <- get_pca_var(projet$dudi[[input$NameACP]])
    
    else
      dt <- get_pca_ind(projet$dudi[[input$NameACP]])
    
    datatable(dt[[input$outputPCA2]], extensions = c("Buttons"),
              options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
  }, server = F)
  
  
  output$screeplotPCA <- renderPlot({
    if (is.null(projet$dudi[[input$NameACP]]))
      return(0)
    
    ade4:::screeplot.dudi(projet$dudi[[input$NameACP]], main = input$NameACP)
  })
}

