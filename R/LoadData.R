LoadData <- tabItem(tabName = "managedata",
                    sidebarLayout(
                      sidebarPanel = sidebarPanel(
                        selectInput("LoaddataType", "Loading method", 
                                    choices = c("ADE4 example", "Saved project", "New dataframe")),
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
             actionButton("DoLoadExample", "Load example", style = "color : white; background-color : #58d68d")
           ),
           "Saved project" = tagList(
             fileInput("LoadProjectFile", "Choose a rds file")
           ),
           "New dataframe" = tagList(
             textInput("LoadDataName", "Choose a name to refer the data frame later"),
             fileInput("LoadDataFile", "Choose a rds or csv file"),
             textInput("LoadDataSep", "separator", ","),
             checkboxInput("LoadDataCheckHeader", "Header"),
             checkboxInput("LoadDataCheckRownames", "Rownames in first column"),
             actionButton("DoLoadData", "Load data", style = "color : white; background-color : #58d68d")
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
    
    if (file_ext(input$LoadDataFile$datapath) == "rds")
      projet$data[[input$LoadDataName]] <- readRDS(input$LoadDataFile$datapath)
    
    
    else if (file_ext(input$LoadDataFile$datapath) == "csv"){
      isrownames <- NULL
      if (input$LoadDataCheckRownames)
        isrownames <- 1
      
      
      projet$data[[input$LoadDataName]] <- read.csv(input$LoadDataFile$datapath, 
                                                    header = input$LoadDataCheckHeader,
                                                    sep = input$LoadDataSep,
                                                    row.names = isrownames)
    }
    else {
      alert("Please enter a rds or csv file") 
      return(0)
    }
  })
  
  output$LoadDataDatatable <- renderDataTable({
    if (length(projet$data) == 0)
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
          projet$dudi[[i]] <- temp[[i]]
        
        else if (class(temp[[i]]) == "data.frame")
          projet$data[[i]] <- temp[[i]]
      }
  })
  
  output$textdudiLoadData <- renderPrint({
    if (length(projet$dudi) == 0)
      return("No dudi object in the current project")
    
    return(projet$dudi[[input$DudinameLoadData]])
    
  })
  
  
}
