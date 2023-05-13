LoadData <- tabItem(tabName = "managedata",
                    sidebarLayout(
                      sidebarPanel = sidebarPanel(
                        selectInput("LoaddataType",
                                    label = tags$span("Loading method", 
                                                      bsButton("helploading", label = "",
                                                               icon = icon("question-circle" )
                                                               , size = "extra-small")),
                                    choices = c("ADE4 example", "Saved project", "New dataframe")),
                        
                        bsPopover(id = "helploading",
                                title = "",
                                content = paste0("Use already existing examples, your own data, or a previous project downloaded from the app."),
                                placement = "right",
                                trigger = c("hover", "focus"),
                                options = list(container = "body")),
                        uiOutput("LoadData_listoptions")
                      ),
                      mainPanel = mainPanel(
                        tabsetPanel(
                          tabPanel("Dataframe",
                                   uiOutput("selectdataframenameLoadData"),
                                   dataTableOutput("LoadDataDatatable")
                          ),
                          tabPanel("Dudi",
                                   uiOutput("selectdudinameLoadData"),
                                   verbatimTextOutput("textdudiLoadData"))
                        )
                      )
                      )
                    )


LoadDataServer <- function(input, output, session, projet){
  
  output$LoadData_listoptions <- renderUI({
    
    switch(input$LoaddataType,
           
           "ADE4 example" = tagList(
             selectInput("examples", "Choose an example", choices = 
                           list(PCA = c("aravo", "baran95"), 
                                COA = c("aminoacyl", "microsatt"),
                                MCA = c("ours", "banque"),
                                BCA = c("meaudret", "avimedi"),
                                Coinertia = c("doubs", "aviurba"),
                                PCAIV = c("rhone", "avimedi")),
                         selected = input$examples),
             if (length(input$examples) != 0) 
               p(description[[input$examples]]),
             actionButton("DoLoadExample", "Load example", style = "color : white; background-color : #93bf29")
           ),
           "Saved project" = tagList(
             fileInput("LoadProjectFile", "Choose a rds file")
           ),
           "New dataframe" = tagList(
             textInput("LoadDataName", "Choose a name to refer the data frame later"),
             fileInput("LoadDataFile", "Choose a rds, csv or txt file"),
             selectInput("LoadDataSep", "separator", c(",", ";", "tab" = "\t", "space" = " ")),
             checkboxInput("LoadDataCheckHeader", "Header"),
             checkboxInput("LoadDataCheckRownames", "Rownames in first column"),
             actionButton("DoLoadData", "Load data", style = "color : white; background-color : #93bf29")
           )
          )
    
    
  })

  
  
  observeEvent(input$LoadProjectFile,{
    
    object <- readRDS(input$LoadProjectFile$datapath)
    projet$data <- object$data
    projet$dudi <- object$dudi
    
  })
  
  output$selectdataframenameLoadData <- renderUI({
    if (length(projet$data) == 0)
      return(NULL)
    
    selectInput("DataframenameLoadData", 
                "Dataframe to show", 
                choices = names(projet$data), 
                selected = input$DataframenameLoadData)
    })
  
  
  output$selectdudinameLoadData <- renderUI({
    if (length(projet$dudi) == 0)
      return(NULL)
    selectInput("DudinameLoadData", 
                "Dataframe to show", 
                choices = names(projet$dudi), 
                selected = input$DudinameLoadData)
    
  })
  
  observeEvent(input$DoLoadData, {
    
    if (length(input$LoadDataFile$datapath) == 0){
      alert("Please choose a rds or csv file")
      return(0)
    }
    
    if(input$LoadDataName == ""){
      alert("Please enter a name")
      return(0)
    }
    
    
    if (file_ext(input$LoadDataFile$datapath) == "rds"){
      
      projet$data[[input$LoadDataName]] <- readRDS(input$LoadDataFile$datapath)
      
      string <- paste(input$LoadDataName, " <- readRDS(", 
                      input$LoadDataFile$datapath,")", sep = "")
      
      projet$code <- paste(projet$code, string, sep = "\n")
      
    }
    
    else if (file_ext(input$LoadDataFile$datapath) == "csv" | 
             file_ext(input$LoadDataFile$datapath) == "txt"){
      isrownames <- NULL
      if (input$LoadDataCheckRownames)
        isrownames <- 1
      
      tryCatch(
      projet$data[[input$LoadDataName]] <- read.table(input$LoadDataFile$datapath, 
                                                    header = input$LoadDataCheckHeader,
                                                    sep = input$LoadDataSep,
                                                    row.names = isrownames)
      , error = function(e){
        alert("There has been an error (printed in R console)")
        print(e)
        return(0)
        
      })
      
      if (is.null(isrownames))
        string <- paste(input$LoadDataName, " <- read.table(<path_to_your_dataframe>, header = ", 
                        input$LoadDataCheckHeader, ", sep = ","'", 
                       input$LoadDataSep,"'", ", row.names = NULL)", sep = "")
      else
        string <- paste(input$LoadDataName, " <- read.table(<path_to_your_dataframe>, header = ", 
                        input$LoadDataCheckHeader, ", sep = ","'", 
                        input$LoadDataSep,"'", ", row.names = ", 
                        isrownames,")", sep = "")
      
      projet$code <- paste(projet$code, string, sep = "\n\n# Load data from a new dataframe\n")
      
    }
    else {
      alert("Please enter a rds or csv file") 
      return(0)
    }
  })
  
  output$LoadDataDatatable <- renderDataTable({
    if (length(projet$data) == 0)
      return(data.frame(list()))
    
    if (is.null(input$DataframenameLoadData))
      return(data.frame(list()))
    
    if (!(input$DataframenameLoadData %in% names(projet$data)))
      return(data.frame(list()))
    
    datatable(projet$data[[input$DataframenameLoadData]],options = list(scrollX = T))
    })
  
  
  observeEvent(input$DoLoadExample,{
    projet$data <- list()
    projet$dudi <- list()
    
    temp <- get(data(list = c(input$examples)))
    
    if (class(temp) == "data.frame" & "dudi" %in% class(temp) == FALSE)
      projet$data[[input$examples]] <- temp
    
    if (class(temp) == "list")
      for(i in names(temp)){
        
        if("dudi" %in% class(temp[[i]]))
          projet$dudi[[paste(input$examples, "$", i, sep = "")]] <- temp[[i]]
        
        else if (class(temp[[i]]) == "data.frame")
          projet$data[[paste(input$examples, "$", i, sep = "")]] <- temp[[i]]
        
        else if (class(temp[[i]]) == "character" | class(temp[[i]]) == "factor") {
          temp[[i]] <- list(X = temp[[i]])
          projet$data[[paste(input$examples, "$", i, sep = "")]] <- as.data.frame(temp[[i]])
        }
      }
    
    string <- paste("data(", input$examples,")", sep = "")
    
    projet$code <- paste(projet$code, string, sep = "\n\n# Load data from ade4 examples\n")
  })
  
  output$textdudiLoadData <- renderPrint({
    if (length(projet$dudi) == 0)
      return("No dudi object in the current project")
    
    if (is.null(input$DudinameLoadData))
      return("No dudi object in the current project")
    
    if (!(input$DudinameLoadData %in% names(projet$dudi)))
      return("No dudi object in the current project")
    
    return(projet$dudi[[input$DudinameLoadData]])
    
  })
  
  
}

