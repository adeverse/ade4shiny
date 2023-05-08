subsetdata <- tabItem(tabName = "subsetdata",
                      sidebarLayout(
                        sidebarPanel = sidebarPanel(
                          textInput("NameSubsetData", "Name of the created dataframe"),
                          uiOutput("selectDataframeSubsetData"),
                          uiOutput("SelectColumnSubsetData"),
                          actionButton("DoSubsetData", "Subset data", style = "color : white; background-color : #93bf29")
                        ),
                        mainPanel = mainPanel(
                          dataTableOutput("DatatableSubsetData")
                        )
                      ))


SubsetDataServer <- function(input, output, session, projet){
  
  output$selectDataframeSubsetData <- renderUI({
    if (length(projet$data) == 0)
      return("")
    
    selectInput("DataframeSubsetData", "Choose a dataframe to subset",
                names(projet$data))
  })
  
  
  output$SelectColumnSubsetData <- renderUI({
    if (length(projet$data) == 0)
      return("")
    
    if (is.null(input$DataframeSubsetData))
      return("")
    
    selectizeInput("ColumnSubsetData", "Select the columns to remove",
                   colnames(projet$data[[input$DataframeSubsetData]]), multiple = T)
    
  })
  
  
  
  output$DatatableSubsetData <- renderDataTable({
    if (length(projet$data) == 0)
      return(data.frame(list()))
    
    if (is.null(input$DataframeSubsetData))
      return(data.frame(list()))
    
    datatable(projet$data[[input$DataframeSubsetData]], options = list(scrollX = T))
    
    })
  
  observeEvent(input$DoSubsetData, {
    if (input$NameSubsetData == ""){
      alert("Please enter a name")
      return(0)
    }
    
    if (input$NameSubsetData %in% names(projet$data)){
      alert("Name already taken, please enter a new one")
      return(0)
    }
    
    df <- projet$data[[input$DataframeSubsetData]]
    keep <- colnames(df)[!colnames(df) %in% input$ColumnSubsetData]
    
    df <- df[,keep]
    keep_string <- paste0(keep, sep = "','", collapse = "")
    keep_string <- substring(keep_string, 1, last = nchar(keep_string)-2)

    
    projet$data[[input$NameSubsetData]] <- df
    
    
    string <- paste(input$NameSubsetData, " <- ", input$DataframeSubsetData, 
                    "[, c('", keep_string, ")]",sep = "")
    
    projet$code <- paste(projet$code, string, sep = "\n\n# Subset dataframe\n")
    
    
  })
  
}

