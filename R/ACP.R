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
                             choices = c("Eigenvalues", "Variables", "Individuals", "Dudi object")),
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
    if (input$uniformcw)
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
    
    else if (input$selectoutputPCA == "Individuals")
      selectInput("outputPCA2", "Select a value to show", 
                  c("coord", "cos2", "contrib"))
    
    else {
      if (is.null(input$selectoutputPCA))
        return("")
      
      if (is.null(input$NameACP))
        return("")
      
      if (!(input$NameACP %in% names(projet$dudi)))
        return("")
      
      remove <- c("eig", "rank", "nf", "cent", "norm", "call")
      name <- names(projet$dudi[[input$NameACP]])
      keep <- name[! name %in% remove]
      
      selectInput("outputPCA2", "select a value to show",
                  keep)
    }
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
    
    if (!(input$uniformrw) & is.null(input$rw)){
      alert("Please enter a row weight file")
      return(0)
    }
    
    
    if (!(input$uniformcw) & is.null(input$cw)){
      alert("Please enter a col weight file")
      return(0)
    }
    
    df <- projet$data[[input$DataframeACP]]
    
    tryCatch({
    
    if (input$uniformrw){
      row.weight <- rep(1, nrow(df))/nrow(df)
      
      string_rw <- paste("rep(1, nrow(", input$DataframeACP, "))/nrow("
                         , input$DataframeACP, ")",sep = "")
      }
    
    else{
      row.w1 <- read.table(input$rw$datapath, header = TRUE, row.names = 1)
      
      # Adding import table to automatic code
      little_string <- "row.w1 <- read.table(<path_to_row_weight_file>, header = T, row.names = 1)"
      projet$code <- paste(projet$code, little_string, sep = "\n\n# Importing row weight\n")
      
      row.weight <- row.w1[,1]
      
      string_rw <- "row.w1[,1]"
      
    }
    
    if (input$uniformcw){
      col.weight <- rep(1, ncol(df))
      string_cw <- paste("rep(1, ncol(", input$DataframeACP, "))", sep = "")
    }
    
    else{
      col.w1 <- read.table(input$cw$datapath, header = TRUE, row.names = 1)
      
      # Adding import table to automatic code
      little_string <- "col.w1 <- read.table(<path_to_col_weight_file>, header = T, row.names = 1)"
      projet$code <- paste(projet$code, little_string, sep = "\n\n# Importing col weight\n")
      
      col.weight <- col.w1[,1]
      
      string_cw <- "col.w1[,1]"
    }
    
    
    temp <- dudi.pca(df, nf = input$nfPCA, scannf = F, 
                     center = input$docenterACP, scale = input$doscaleACP, 
                     row.w = row.weight, col.w = col.weight)
    
    temp$cw <- list(col.weight = temp$cw)
    temp$lw <- list(row.weight = temp$lw)
    
    projet$dudi[[input$NameACP]] <- temp
    
    string <- paste(input$NameACP, " <- dudi.pca(", input$DataframeACP, ", nf = ", input$nfPCA, ", scannf = ", F,
                    ", center = ",input$docenterACP, ", scale = ", input$doscaleACP, 
                    ", row.w = ", string_rw, ", col.w = ", string_cw,")", sep = "")
    
    projet$code <- paste(projet$code, string, sep = "\n\n# Computing PCA\n")
    
    projet$dudi[[input$NameACP]]$call <- substring(string, nchar(input$NameACP) + 5)
    
    }, error = function(e){
      alert("There has been an error (printed in R console)")
      print(e)
      return(0)
    })
    
  })
  
  output$SummaryPCA <- renderPrint({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    if (is.null(input$NameACP))
      return("No dudi object with this name in the project")
    
    if (!(input$NameACP %in% names(projet$dudi)))
      return("No dudi object with this name in the project")
    
    ade4:::summary.dudi(projet$dudi[[input$NameACP]])
    
  })
  
  
  output$outputPCA <- renderDataTable({
    if (length(projet$dudi) == 0)
      return(data.frame(list()))
    
    if (!(input$NameACP %in% names(projet$dudi)))
      return(data.frame(list()))
    
    
    if (input$selectoutputPCA == "Eigenvalues"){
      return(datatable(data.frame(list(values = projet$dudi[[input$NameACP]]$eig))))
    }
    
    else if (input$selectoutputPCA == "Variables")
      dt <- get_pca_var(projet$dudi[[input$NameACP]])
    
    else if (input$selectoutputPCA == "Individuals")
      dt <- get_pca_ind(projet$dudi[[input$NameACP]])
    
    else
      dt <- projet$dudi[[input$NameACP]]
    
    if (is.null(input$outputPCA2))
      return(data.frame(list()))
    
    datatable(as.data.frame(dt[[input$outputPCA2]]), extensions = c("Buttons"),
              options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
  }, server = F)
  
  
  output$screeplotPCA <- renderPlot({
    if (is.null(projet$dudi[[input$NameACP]]))
      return(0)
    
    ade4:::screeplot.dudi(projet$dudi[[input$NameACP]], main = input$NameACP)
  })
}

