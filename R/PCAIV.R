pcaIV <- tabItem(tabName = "pcaiv",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   uiOutput("selectizePCAIV"),
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
  
  output$selectizePCAIV <- renderUI({
    all_PCAIV <- sapply(names(projet$dudi), function(x){
      if ("pcaiv" %in% class(projet$dudi[[x]]))
        return(x)
    })
    
    if (length(all_PCAIV) == 0)
      selectizeInput("NamePCAIV", "Name to refer the PCAIV later", 
                     choices = all_PCAIV, options = list(create = TRUE))
    
    else{
      last <- all_PCAIV[length(all_PCAIV)]
      selectizeInput("NamePCAIV", "Name to refer the PCAIV later", choices = all_PCAIV, 
                     options = list(create = TRUE), selected = last)
    }
  })
  
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
    
    if (is.null(input$NamePCAIV))
      return()
    
    if (!(input$NamePCAIV %in% names(projet$dudi)))
      return()
    
    remove <- c("eig", "rank", "nf", "call")
    name <- names(projet$dudi[[input$NamePCAIV]])
    keep <- name[! name %in% remove]
    
    selectInput("dataframePCAIV", "select a value to show",
                keep)
    
  })
  
  observeEvent(input$DoPCAIV, {
    if (input$NamePCAIV == ""){
      alert("Please enter a name")
      return(0)
    }
    
    if (input$NamePCAIV %in% names(projet$dudi)){
      alert("Name already taken, please enter a new one")
      return(0)
    }
    
    dudi <- projet$dudi[[input$DudiPCAIV]]
    df <- projet$data[[input$DfPCAIV]]
    
    tryCatch({
      
      temp <- pcaiv(dudi, df ,nf = input$nfPCAIV, scannf = F)
      projet$dudi[[input$NamePCAIV]] <- temp
      
      string <- paste(input$NamePCAIV, " <- pcaiv(", input$DudiPCAIV, 
                      ", ", input$DfPCAIV, ", nf = ", input$nfPCAIV, 
                      ", scannf = ", F,")", sep = "")
      
      projet$code <- paste(projet$code, string, sep = "\n\n# Computing PCAIV\n")
      
      projet$dudi[[input$NamePCAIV]]$call <- substring(string, nchar(input$NamePCAIV) + 5)
      
    }, error = function(e){
      alert("There has been an error (printed in the R console)")
      print(e)
      return(0)
    })
    
  })
  
  
  output$summaryPCAIV <- renderPrint({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    if (is.null(input$NamePCAIV))
      return("No dudi object with this name in the project")
    
    if (!(input$NamePCAIV %in% names(projet$dudi)))
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
    
    if (is.null(input$dataframePCAIV))
      return(data.frame(list()))
    
    dt$cw <- list(col.weight = dt$cw)
    dt$lw <- list(row.weight = dt$lw)
    
    
    datatable(as.data.frame(dt[[input$dataframePCAIV]]), 
              extensions = c("Buttons"),
              options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
  }, server = F)
  
  
}
