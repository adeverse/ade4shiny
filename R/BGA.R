bga <- tabItem(tabName = "bga",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   uiOutput("selectizeBGA"),
                   uiOutput("selectDudiBGA"),
                   tags$hr(style="border-color: gray;"),
                   p(markdown("#### Grouping factor")),
                   uiOutput("selectObjectGroupBGA"),
                   uiOutput("selectGroupBGA"),
                   tags$hr(style="border-color: gray;"),
                   numericInput("nfBGA", "Nomber of dimension to keep", 5, 2, 200),
                   actionButton("DoBGA", "Compute BGA", style = "color : white; background-color : #58d68d")
                 ),
                 mainPanel = mainPanel(
                   tabsetPanel(
                     tabPanel("Summary",
                              verbatimTextOutput("summaryBGA")
                              ),
                     tabPanel("Output",
                              uiOutput("selectoutputBGA"),
                              dataTableOutput("datatableBGA")
                              ),
                     tabPanel("Plot",
                              plotOutput("plotBGA")
                              )  
                   )
                 )
               )
)


bgaServer <- function(input, output, session, projet){
  
  output$selectizeBGA <- renderUI({
    all_BGA <- sapply(names(projet$dudi), function(x){
      if ("between" %in% class(projet$dudi[[x]]))
        return(x)
    })
    
    if (length(all_BGA) == 0)
      selectizeInput("NameBGA", "Name to refer the BGA later", 
                     choices = all_BGA, options = list(create = TRUE))
    
    else{
      last <- all_BGA[length(all_BGA)]
      selectizeInput("NameBGA", "Name to refer the BGA later", choices = all_BGA, 
                     options = list(create = TRUE), selected = last)
    }
  })
  
  output$selectDudiBGA <- renderUI({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    selectInput("DudiBGA", "Select a Dudi object", 
                choices = names(projet$dudi), 
                selected = input$DudiBGA)
  })
  
  output$selectObjectGroupBGA <-renderUI(
    
    selectInput("ObjectGroupBGA", "Select an object", 
                choices = c(names(projet$data), names(projet$dudi)),
                selected = input$ObjectGroupBGA)
    
  )
  
  
  output$selectGroupBGA <- renderUI({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    if (is.null(input$ObjectGroupBGA))
      return("")
    
    
    if ("dudi" %in% class(projet$dudi[[input$ObjectGroupBGA]]))
      selectInput("GroupBGA", "Select a grouping column",
                  choices = names(projet$dudi[[input$ObjectGroupBGA]]$tab),
                  selected = input$GroupBGA)
    
    else
      selectInput("GroupBGA", "Select a grouping column",
                  choices = colnames(projet$data[[input$ObjectGroupBGA]]),
                  selected = input$GroupBGA)
  })
  
  
  output$selectoutputBGA <- renderUI({
    if (is.null(input$NameBGA) | !(input$NameBGA %in% names(projet$dudi)))
      return(0)
    
    x <- lapply(projet$dudi[[input$NameBGA]], is.data.frame)
    y <- x[which(x == T)]
    
    selectInput("outputBGA", "Select a variable", choices = names(y))
  })
  
  
  observeEvent(input$DoBGA, {
    if (input$NameBGA == ""){
      alert("Please enter a name")
      return(0)
    }
    
    if (input$NameBGA %in% names(projet$dudi)){
      alert("Name already taken, please enter a new one")
      return(0)
    }
    
    dud <- projet$dudi[[input$DudiBGA]]
    
    if ("dudi" %in% class(projet$dudi[[input$ObjectGroupBGA]]))
      fact <- as.factor(projet$dudi[[input$ObjectGroupBGA]]$tab[,input$GroupBGA])
    
    else
      fact <- as.factor(projet$data[[input$ObjectGroupBGA]][,input$GroupBGA])
    
    tryCatch({
      
      temp <- bca(dud, fac = fact,nf = input$nfBGA, scannf = F)
      projet$dudi[[input$NameBGA]] <- temp
      
      if ("dudi" %in% class(projet$dudi[[input$ObjectGroupBGA]]))
        string <- paste(input$NameBGA, " <- bca(", input$DudiBGA, ", fac = ", 
                        "as.factor(", input$ObjectGroupBGA, "$tab[,", '"',input$GroupBGA,'"',"])",
                        ", nf = ", input$nfBGA, ", scannf = F"
                        ,")", sep = "")
      
      else
        string <- paste(input$NameBGA, " <- bca(", input$DudiBGA, ", fac = ", 
                        "as.factor(", input$ObjectGroupBGA, "[,", "'",input$GroupBGA,"'","])",
                        ", nf = ", input$nfBGA, ", scannf = F"
                        ,")", sep = "")
      
      projet$code <- paste(projet$code, string, sep = "\n\n# Computing BCA\n")
      
      projet$dudi[[input$NameBGA]]$call <- substring(string, nchar(input$NameBGA) + 5)
      
    }, error = function(e){
      alert("There has been an error (printed in R console)")
      print(e)
      return(0)
    })
    
    
  })
  
  
  output$summaryBGA <- renderPrint({
    if (length(projet$dudi) == 0)
      return("No dudi object in the project")
    
    if (is.null(input$NameBGA))
      return("No dudi object with this name in the project")
    
    if (!(input$NameBGA %in% names(projet$dudi)))
      return("No dudi object with this name in the project")
    
    ade4:::summary.dudi(projet$dudi[[input$NameBGA]])
  })
  
  
  output$datatableBGA <- renderDataTable({
    if (is.null(projet$dudi[[input$NameBGA]]))
      return(data.frame(list()))
    
    dt <- projet$dudi[[input$NameBGA]]
    
    if (is.null(input$outputBGA))
      return(data.frame(list()))
    
    datatable(dt[[input$outputBGA]], extensions = c("Buttons"),
              options = list(scrollX = TRUE, buttons = c("csv"), dom = 'Bfrtip'))
  }, server = F)
  
  
  
  output$plotBGA <- renderPlot({
    if (is.null(projet$dudi[[input$NameBGA]]))
      return(0)
    
    dud <- projet$dudi[[input$DudiBGA]]
    
    if ("dudi" %in% class(projet$dudi[[input$ObjectGroupBGA]]))
      fact <- as.factor(projet$dudi[[input$ObjectGroupBGA]]$tab[,input$GroupBGA])
    
    else
      fact <- as.factor(projet$data[[input$ObjectGroupBGA]][,input$GroupBGA])
    
    temp <- bca(dud, fac = fact, nf = input$nfBGA, scannf = F)
    
    
    ade4:::plot.between(temp)
    
    
  })
  
  
  
}