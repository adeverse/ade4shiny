# Permet de faire une WCA, d'afficher des outputs et de stocker le dudi pour plus tard

wga <- tabItem(tabName = "wga",
               h2("Within-Group Analysis (WGA)"),
               h4("Also called WCA for Within-Class Analysis"),
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   uiOutput("selectizeWGA"),
                   uiOutput("selectDudiWGA"),
                   tags$hr(style="border-color: gray;"),
                   p(markdown("#### Grouping factor")),
                   uiOutput("selectObjectGroupWGA"),
                   uiOutput("selectGroupWGA"),
                   tags$hr(style="border-color: gray;"),
                   
                   numericInput("nfWGA",
                                label = tags$span("Number of dimension to keep", 
                                                  bsButton("helpnfwga", label = "",
                                                           icon = icon("question-circle" )
                                                           , size = "extra-small")),
                                2, 2, 200),
                   
                   bsPopover(id = "helpnfwga",
                             title = "",
                             content = paste0(
                               "Number of axes of variance (dimensions) to keep. See more: ",
                               a("dudi.pca()", href = "http://adeverse.github.io/ade4/reference/wca.html", target="_blank")),
                             placement = "right",
                             trigger = c("hover", "focus", "click"),
                             options = list(container = "body")),
                   
                   actionButton("DoWGA", "Compute WGA", style = "color : white; background-color :  #93bf29")
                 ),
                 mainPanel = mainPanel(
                   tabsetPanel(
                     tabPanel("Summary",
                              verbatimTextOutput("summaryWGA")
                              ),
                     tabPanel("Output",
                              uiOutput("selectoutputWGA"),
                              dataTableOutput("datatableWGA")
                              ),
                     tabPanel("Plot",
                              plotOutput("plotWGA")
                              )  
                   )
                 )
               )
)


wgaServer <- function(input, output, session, projet){
  
  # Permet de donner un nom a la WCA ou de choisir une WCA existante
  output$selectizeWGA <- renderUI({
    all_WGA <- sapply(names(projet$dudi), function(x){
      if ("within" %in% class(projet$dudi[[x]]))
        return(x)
    })
    
    if (length(all_WGA) == 0)
      selectizeInput("NameWGA",
                     label = tags$span("Analysis name ",
                                       popify(el = bsButton("namewga1", label = "", icon = icon("question-circle"), size = "extra-small"),
                                              title = "",
                                              content = "Type in a new name to compute a new WGA or select a previous WGA from the list to display its results",
                                              placement = "right", trigger = c("hover", "focus", "click"),
                                              options = list(container = "body")) 
                     ),
                     choices = all_WGA, options = list(create = TRUE))
    
    else{
      last <- all_WGA[length(all_WGA)]
      selectizeInput("NameWGA",
                     label = tags$span("Analysis name ",
                     popify(el = bsButton("namewga2", label = "", icon = icon("question-circle"), size = "extra-small"),
                            title = "",
                            content = "Type in a new name to compute a new WGA or select a previous WGA from the list to display its results",
                            placement = "right", trigger = c("hover", "focus", "click"),
                            options = list(container = "body"))), 
      choices = all_WGA, 
      options = list(create = TRUE), selected = last)
    }
    
  })
  
  # Permet de choisir le dataframe pour la wca
  output$selectDudiWGA <- renderUI({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    selectInput("DudiWGA",
                label = tags$span("Select a Dudi object",
                                  popify(el = bsButton("dudiobjectwga", label = "", icon = icon("question-circle"), size = "extra-small"),
                                         title = "",
                                         content = paste0("A duality diagram (object of class dudi), outputed by a one table analysis, which is present in the app environment (previously ran or lodaded in the app). See more: ",
                                                          a("wca()", href = "http://adeverse.github.io/ade4/reference/dudi.pca.html", target="_blank")),
                                         placement = "right", trigger = c("hover", "focus", "click"),
                                         options = list(container = "body")) 
                ),
                choices = names(projet$dudi), 
                selected = input$DudiWGA)
  })
  
  # Permet de choisir l'objet contenant le vecteur de group de la wca
  output$selectObjectGroupWGA <-renderUI(
    
    selectInput("ObjectGroupWGA",
                label = tags$span("Select an object",
                                  popify(el = bsButton("objectwga", label = "", icon = icon("question-circle"), size = "extra-small"),
                                         title = "",
                                         content = paste0("An object (dudi or dataframe) loaded in the app in which to select the grouping factor for the within group analysis."),
                                         placement = "right", trigger = c("hover", "focus", "click"),
                                         options = list(container = "body")) 
                ),
                choices = c(names(projet$data), names(projet$dudi)),
                selected = input$ObjectGroupWGA)
    
  )
  
  # Permet de choisir la colonne de l'objet qui contient le grouping factor
  # Si l'objet est un dudi, on choisit une colonne parmi dudi$tab
  # Si l'objet est un dataframe, on choisit une colonne
  output$selectGroupWGA <- renderUI({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    if (is.null(input$ObjectGroupWGA))
      return("")
    
    
    if ("dudi" %in% class(projet$dudi[[input$ObjectGroupWGA]]))
      selectInput("GroupWGA",
                  label = tags$span("Select a grouping column",
                                    popify(el = bsButton("groupwga1", label = "", icon = icon("question-circle"), size = "extra-small"),
                                           title = "",
                                           content = paste0("A factor partitioning the rows of dudi$tab in classes. Dudis$tab is the data frame that was analyzed with the one-table analysis, modified according to the transformation arguments that were used (ie centered and scaled). See more: ",
                                                            a("wca()", href = "http://adeverse.github.io/ade4/reference/wca.html", target="_blank")),
                                           placement = "right", trigger = c("hover", "focus", "click"),
                                           options = list(container = "body")) 
                  ),
                  choices = names(projet$dudi[[input$ObjectGroupWGA]]$tab),
                  selected = input$GroupWGA)
    
    else
      selectInput("GroupWGA",
                  label = tags$span("Select a grouping column",
                                    popify(el = bsButton("groupwga2", label = "", icon = icon("question-circle"), size = "extra-small"),
                                           title = "",
                                           content = paste0("A factor partitioning the rows of dudi$tab in classes. Dudis$tab is the data frame that was analyzed with the one-table analysis, modified according to the transformation arguments that were used (ie centered and scaled). See more: ",
                                                            a("wca()", href = "http://adeverse.github.io/ade4/reference/wca.html", target="_blank")),
                                           placement = "right", trigger = c("hover", "focus", "click"),
                                           options = list(container = "body")) 
                  ),
                  choices = colnames(projet$data[[input$ObjectGroupWGA]]),
                  selected = input$GroupWGA)
  })
  
  
  # Permet de choisir quoi afficher dans l'onglet output
  output$selectoutputWGA <- renderUI({
    if (is.null(input$NameWGA) | !(input$NameWGA %in% names(projet$dudi)))
      return()
    
    
    remove <- c("eig", "rank", "nf", "call")
    name <- names(projet$dudi[[input$NameWGA]])
    keep <- name[! name %in% remove]
    
    selectInput("outputWGA", "select a value to show",
                keep)
  })
  
  # Quand on clique sur le bouton pour faire l'analyse
  observeEvent(input$DoWGA, {
    if (input$NameWGA == ""){
      alert("Please enter a name")
      return(0)
    }
    
    if (input$NameWGA %in% names(projet$dudi)){
      alert("Name already taken, please enter a new one")
      return(0)
    }
    
    dud <- projet$dudi[[input$DudiWGA]]
    if ("dudi" %in% class(projet$dudi[[input$ObjectGroupWGA]]))
      fact <- as.factor(projet$dudi[[input$ObjectGroupWGA]]$tab[,input$GroupWGA])
    else
      fact <- as.factor(projet$data[[input$ObjectGroupWGA]][,input$GroupWGA])
    
    tryCatch({
      
      temp <- wca(x = dud, fac = fact, nf = input$nfWGA, scannf = F)
      
      projet$dudi[[input$NameWGA]] <<- temp
      
      # Ecrit le code pour faire l'analyse dans projet$code
      if ("dudi" %in% class(projet$dudi[[input$ObjectGroupWGA]]))
        string <- paste(input$NameWGA, " <- wca(x = ", input$DudiWGA, ", fac = ", 
                        "as.factor(", input$ObjectGroupWGA, "$tab[,", '"',input$GroupWGA,'"',"])",
                        ", nf = ", input$nfWGA, ", scannf = F"
                        ,")", sep = "")
      
      else
        string <- paste(input$NameWGA, " <- wca(x = ", input$DudiWGA, ", fac = ", 
                        "as.factor(", input$ObjectGroupWGA, "[,", "'",input$GroupWGA,"'","])",
                        ", nf = ", input$nfWGA, ", scannf = F"
                        ,")", sep = "")
      
      projet$code <<- paste(projet$code, string, sep = "\n\n# Computing WCA\n")
      
      projet$dudi[[input$NameWGA]]$call <<- str2lang(substring(string, nchar(input$NameWGA) + 5))
      
    }, error = function(e){
      alert("There has been an error (printed in R console)")
      print(e)
      return(0)
    })
    
    
  })
  
  
  output$summaryWGA <- renderPrint({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    if (is.null(input$NameWGA))
      return("No dudi object with this name in the project")
    
    if (!(input$NameWGA %in% names(projet$dudi)))
      return("No dudi object with this name in the project")
    
    ade4:::summary.dudi(projet$dudi[[input$NameWGA]])
  })
  
  
  output$datatableWGA <- renderDataTable({
    if (is.null(projet$dudi[[input$NameWGA]]))
      return(data.frame(list()))
    
    dt <- projet$dudi[[input$NameWGA]]
    
    if (is.null(input$outputWGA))
      return(data.frame(list()))
    
    dt$cw <- list(col.weight = dt$cw)
    dt$lw <- list(row.weight = dt$lw)
    dt$ratio <- list(ratio = dt$ratio)
    
    datatable(as.data.frame(dt[[input$outputWGA]]), extensions = c("Buttons"),
              options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
  }, server = F)
  
  
  
  output$plotWGA <- renderPlot({
    if (is.null(projet$dudi[[input$NameWGA]]))
      return(0)
    
    dud <- projet$dudi[[input$DudiWGA]]
    
    if ("dudi" %in% class(projet$dudi[[input$ObjectGroupWGA]]))
      fact <- as.factor(projet$dudi[[input$ObjectGroupWGA]]$tab[,input$GroupWGA])
    
    else
      fact <- as.factor(projet$data[[input$ObjectGroupWGA]][,input$GroupWGA])
    
    temp <- wca(dud, fac = fact,nf = input$nfWGA, scannf = F)

    ade4:::plot.within(temp)
  })
  
  
  
}