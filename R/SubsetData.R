subsetdata <- tabItem(tabName = "subsetdata",
                      h2("Subsetting dataframes"),
                      sidebarLayout(
                        sidebarPanel = sidebarPanel(
                          textInput("NameSubsetData", 
                                    label = tags$span("Name of the created dataframe", 
                                                      bsButton("helpnamesubset", label = "",
                                                               icon = icon("question-circle" )
                                                               , size = "extra-small"))
                                    ),
                          bsPopover(id = "helpnamesubset",
                                  title = "",
                                  content = "Name for the new dataframe that will be created. You cannot ovrewrite an existing dataframe.",
                                  placement = "right",
                                  trigger = c("hover", "focus"),
                                  options = list(container = "body")),
                        
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
    
    selectInput("DataframeSubsetData",
                label = tags$span("Choose a dataframe to subset ",
                                  popify(el = bsButton("helpsusbsetdf", label = "", icon = icon("question-circle"), size = "extra-small"),
                                         title = "",
                                         content = "A dataframe, already loaded in the app, that you want to modify. Modifications will be saved in the new object and not inplace.",
                                         placement = "right", trigger = c("hover", "focus"),
                                         options = list(container = "body"))
                ), 
                names(projet$data))
  })
  
  
  output$SelectColumnSubsetData <- renderUI({
    if (length(projet$data) == 0)
      return("")
    
    if (is.null(input$DataframeSubsetData))
      return("")
    
    selectizeInput("ColumnSubsetData",
                   label = tags$span("Select the columns to remove ",
                                     popify(el = bsButton("helpsusbsetcols", label = "", icon = icon("question-circle"), size = "extra-small"),
                                            title = "",
                                            content = "The unselected columns will be saved in the new object. The original dataframe will not be deleted or modified.",
                                            placement = "right", trigger = c("hover", "focus"),
                                            options = list(container = "body"))
                   ), 
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

