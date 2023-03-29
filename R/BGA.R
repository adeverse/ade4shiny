bga <- tabItem(tabName = "bga",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   textInput("NameBGA", "Name to refer the BGA later"),
                   uiOutput("selectDudiBGA"),
                   tags$hr(style="border-color: gray;"),
                   p(markdown("#### Grouping factor")),
                   uiOutput("selectObjectGroupBGA"),
                   uiOutput("selectGroupBGA"),
                   tags$hr(style="border-color: gray;"),
                   numericInput("nfBGA", "Nomber of dimension to keep", 5, 2, 200),
                   actionButton("DoBGA", "Compute BGA")
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
    if (is.null(projet$dudi[[input$NameBGA]]))
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
    
    dud <- projet$dudi[[input$DudiBGA]]
    
    if ("dudi" %in% class(projet$dudi[[input$ObjectGroupBGA]]))
      fact <- as.factor(projet$dudi[[input$ObjectGroupBGA]]$tab[,input$GroupBGA])
    
    else
      fact <- as.factor(projet$data[[input$ObjectGroupBGA]][,input$GroupBGA])
    
    tryCatch({
      
      temp <- bca(dud, fac = fact,nf = input$nfBGA, scannf = F)
      projet$dudi[[input$NameBGA]] <- temp
      
    }, error = function(e){
      alert("The dataframe is not suited for a bga analysis")
      print(e)
      return(0)
    })
    
    
  })
  
  
  output$summaryBGA <- renderPrint({
    if (is.null(projet$dudi[[input$NameBGA]]))
      return("No dudi object with this name in the project")
    
    ade4:::summary.dudi(projet$dudi[[input$NameBGA]])
  })
  
  
  output$datatableBGA <- renderDataTable({
    if (is.null(projet$dudi[[input$NameBGA]]))
      return(data.frame(list()))
    
    dt <- projet$dudi[[input$NameBGA]]
    
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
    
    ade4:::plot.between(projet$dudi[[input$NameBGA]])
    
  })
  
  
  
}