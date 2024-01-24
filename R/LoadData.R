## Permet d'importer des jeux de données de 3 manières :
### Depuis les exemples d'ADE4
### Depuis un ancien projet créé par l'appli
### Depuis un dataframe ou un vecteur au format csv-like.


LoadData <- tabItem(tabName = "managedata",
                    h2("Loading in data"),
                    sidebarLayout(
                      sidebarPanel = sidebarPanel(
                        selectInput("LoaddataType",
                                    label = tags$span("Loading method", 
                                                      bsButton("helploading", label = "",
                                                               icon = icon("question-circle" )
                                                               , size = "extra-small")),
                                    choices = c("ADE4 example", "Saved project", "New dataframe", "ADE4 data set")),
                        
                        bsPopover(id = "helploading",
                                title = "",
                                content = paste0("Use basic ade4 examples, your own data, a previously downloaded project, or any ade4 data set."),
                                placement = "right",
                                trigger = c("hover", "focus", "click"),
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
  
  
  ## Change l'UI in se basant sur la valeur de input$LoaddataType
  ### qui donne le choix entre les 3 façons d'importer les données
  output$LoadData_listoptions <- renderUI({

    switch(input$LoaddataType,
           "ADE4 example" = tagList(
             selectInput("examples", "Choose an example", choices = 
                           list('PCA' = c("aravo", "baran95"), 
                                'COA' = c("aminoacyl", "microsatt"),
                                'MCA' = c("ours", "banque"),
                                'BCA' = c("meaudret", "avimedi"),
                                'Coinertia' = c("doubs", "aviurba"),
                                'PCAIV' = c("rhone", "avimedi")),
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
           ),
           "ADE4 data set" = tagList(
             textInput("LoadDataSetName", "Choose the data set name"),
             # actionButton("DoHelpDataSet", "Data set help", style = "color : white; background-color : #93bf29"),
             actionButton("DoLoadDataSet", "Load data set", style = "color : white; background-color : #93bf29")
           )
          )
  })

  
  
  # Quand on clique sur le bouton pour charger un projet existant
  # On peut créer un projet dans R en enregistrant une liste au format RDS:
  #	dune4 <- list(data=dunedata)
  # saveRDS(dune4, "dune4.rds")
  #
  observeEvent(input$LoadProjectFile,{
    
    object <- readRDS(input$LoadProjectFile$datapath)
    projet$data <- object$data
    projet$dudi <- object$dudi
    projet$code <- object$code
    
  })
  
  # Select input qui permet de choisir quel dataframe afficher dans l'onglet Dataframe
  output$selectdataframenameLoadData <- renderUI({
    if (length(projet$data) == 0)
      return(NULL)
    
    selectInput("DataframenameLoadData", 
                "Dataframe to show", 
                choices = names(projet$data), 
                selected = input$DataframenameLoadData)
    })
  
  
  # Select input qui permet de choisir quel dudi afficher dans l'onglet Dudi
  output$selectdudinameLoadData <- renderUI({
    if (length(projet$dudi) == 0)
      return(NULL)
    selectInput("DudinameLoadData", 
                "Dataframe to show", 
                choices = names(projet$dudi), 
                selected = input$DudinameLoadData)
    
  })
  
  
  # Quand on clique sur le bouton pour charger un dataframe depuis un fichier
  observeEvent(input$"DoLoadData", {
    
    if (length(input$LoadDataFile$datapath) == 0){ # Si aucun fichier en entrée
      alert("Please choose a rds or csv file")
      return(0)
    }
    
    if(input$LoadDataName == ""){
      alert("Please enter a name")
      return(0)
    }
    
    
    if (file_ext(input$LoadDataFile$datapath) == "rds"){ # Si le fichier est un rds
      
      projet$data[[input$LoadDataName]] <- readRDS(input$LoadDataFile$datapath)
      
      string <- paste(input$LoadDataName, " <- readRDS(", 
                      input$LoadDataFile$datapath,")", sep = "")
      
      projet$code <- paste(projet$code, string, sep = "\n")
      
    }
    
    else {
      isrownames <- NULL
      if (input$LoadDataCheckRownames)
        isrownames <- 1
      
      tryCatch( # Essaie de read le fichier en entrée
      projet$data[[input$LoadDataName]] <- read.table(input$LoadDataFile$datapath, 
                                                    header = input$LoadDataCheckHeader,
                                                    sep = input$LoadDataSep,
                                                    row.names = isrownames)
      , error = function(e){
        alert("There has been an error (printed in R console)")
        print(e)
        return(0)
        
      })
      
      # Rajoute le code d'importation du fichier dans projet$code
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
  })
  
  # Affiche le dataframe dans l'onglet Dataframe
  output$LoadDataDatatable <- renderDataTable({
    if (length(projet$data) == 0)
      return(data.frame(list()))
    
    if (is.null(input$DataframenameLoadData))
      return(data.frame(list()))
    
    if (!(input$DataframenameLoadData %in% names(projet$data)))
      return(data.frame(list()))
    
    datatable(projet$data[[input$DataframenameLoadData]],options = list(scrollX = T))
    })
  
  
  # Quand on clique sur le bouton pour charger un example d'ADE4
  observeEvent(input$DoLoadExample,{
    
    temp <- get(data(list = c(input$examples)))
    
    # Si l'example est juste un data frame
    if (class(temp) == "data.frame" & "dudi" %in% class(temp) == FALSE)
      projet$data[[input$examples]] <- temp
    
    # Si l'example est une liste d'objets
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
    
    # Rajoute le code pour load les data dans projet$code
    string <- paste("data(", input$examples,")", sep = "")
    
    projet$code <- paste(projet$code, string, sep = "\n\n# Load data from ade4 examples\n")
  })
  
  # Affiche le dudi selectionné dans l'onglet
  output$textdudiLoadData <- renderPrint({
    if (length(projet$dudi) == 0)
      return("No dudi object in the current project")
    
    if (is.null(input$DudinameLoadData))
      return("No dudi object in the current project")
    
    if (!(input$DudinameLoadData %in% names(projet$dudi)))
      return("No dudi object in the current project")
    
    return(projet$dudi[[input$DudinameLoadData]])
    
  })
  
#   # Quand on clique sur le bouton pour l'aide sur un jeu de données d'ADE4
#  observeEvent(input$DoHelpDataSet,{
#    lsd1 <- isolate(data(package="ade4")$results[,3])
#
#    if (!(input$LoadDataSetName %in% lsd1)) {
#      alert("No such data set in ade4")
#      return(0)
#    }
#
#    temp <- get(data(list = c(input$LoadDataSetName)))
#    
#    browseURL(paste0("http://127.0.0.1:31770/library/ade4/html/", input$LoadDataSetName, ".html"))
#
#    # Rajoute le code pour load les data dans projet$code
#    string <- paste0("help(", input$LoadDataSetName,")")
#    
#    projet$code <- paste(projet$code, string, sep = "\n\n# help about ade4 data set\n")
#  })


    # Quand on clique sur le bouton pour charger un jeu de données d'ADE4
  observeEvent(input$DoLoadDataSet,{
    lsd1 <- isolate(data(package="ade4")$results[,3])

    if (!(input$LoadDataSetName %in% lsd1)) {
      alert("No such data set in ade4")
      return(0)
    }

    temp <- get(data(list = c(input$LoadDataSetName)))
    
    # Si l'example est juste un data frame
    if (class(temp) == "data.frame" & "dudi" %in% class(temp) == FALSE)
      projet$data[[input$LoadDataSetName]] <- temp
    
    # Si l'example est une liste d'objets
    if (class(temp) == "list")
      for(i in names(temp)){
        
        if("dudi" %in% class(temp[[i]]))
          projet$dudi[[paste(input$LoadDataSetName, "$", i, sep = "")]] <- temp[[i]]
        
        else if (class(temp[[i]]) == "data.frame")
          projet$data[[paste(input$LoadDataSetName, "$", i, sep = "")]] <- temp[[i]]
        
        else if (class(temp[[i]]) == "character" | class(temp[[i]]) == "factor") {
          temp[[i]] <- list(X = temp[[i]])
          projet$data[[paste(input$LoadDataSetName, "$", i, sep = "")]] <- as.data.frame(temp[[i]])
        }
      }
    
    # Rajoute le code pour load les data dans projet$code
    string <- paste("data(", input$LoadDataSetName,")", sep = "")
    
    projet$code <- paste(projet$code, string, sep = "\n\n# Load data from ade4 data set\n")
  })
  

  
  
}

