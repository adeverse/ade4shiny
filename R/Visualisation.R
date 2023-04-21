visu <- tabItem("visualisation",
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      selectInput("plottype", "Plot", 
                  choices = c("Label", "Class", "Value")),
      hr(),
      uiOutput("visualisation_listoption"),
      #checkboxInput("AddPlot", "Add to plot"),
      actionButton("DoRenderPlot", "Compute plot", 
                   style = "color : white; background-color : #58d68d"),
      hr(),
      selectInput("plotformat", "Picture extension", c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg")),
      numericInput("width", "Plot width (cm)", value = 14),
      numericInput("height", "Plot height (cm)", value = 9),
      downloadButton("exportplot", "Export plot", style = "color : white; background-color : #58d68d")
    ),
    mainPanel = mainPanel(
      plotOutput("PlotVisualisation")
    )
  )
)


visuServer <- function(input, output, session, projet){
  
  output$visualisation_listoption <- renderUI({
    
    switch(input$plottype,
           
           "Label" = tagList(
             selectInput("objectLabel", "Object", 
                         choices = c(names(projet$data), names(projet$dudi)),
                         selected = input$objectLabel),
             {
               if (length(input$objectLabel) != 0){
                 if (input$objectLabel %in% names(projet$data))
                  {}
                 
                 else if (input$objectLabel %in% names(projet$dudi))
                   selectInput("xyLabel", "XY coordinates", 
                              choices = names(projet$dudi[[input$objectLabel]]))
               }
             },
             numericInput("xLabel", "x axis column", value = 1),
             numericInput("yLabel", "y axis column", value = 2)
             
           ),
           
           "Class" = tagList(
             selectInput("objectClass", "Object", 
                         choices = c(names(projet$data), names(projet$dudi)),
                         selected = input$objectClass),
             {
               if (length(input$objectClass) != 0){
                 if (input$objectClass %in% names(projet$data))
                 {}
                 
                 else if (input$objectClass %in% names(projet$dudi))
                   selectInput("xyClass", "XY coordinates", 
                               choices = names(projet$dudi[[input$objectClass]]),
                               selected = input$xyClass)
               }
             },
             numericInput("xClass", "x axis column", value = 1),
             numericInput("yClass", "y axis column", value = 2),
             
             tags$hr(style="border-color: gray;"),
             p(markdown("#### Grouping factor")),
             selectInput("dataClassFactor", "Dataframe containing the groups", 
                         names(projet$data), selected = input$dataClassFactor),
             {if (length(input$dataClassFactor) != 0)
               selectInput("ClassGroupingFactor", "Column containing the groups",
                           names(projet$data[[input$dataClassFactor]]),
                           selected = input$ClassGroupingFactor)
               
               },
             
             tags$hr(style="border-color: gray;"),
             p(markdown("#### Row weight")),
             selectInput("dataClassWeight", "Dataframe containing the weights", 
                         c("None" = "none", names(projet$data)),
                         selected = input$dataClassWeight),
             {if (length(input$dataClassWeight) != 0)
               selectInput("RowWeightClass", "Column containing the groups",
                           names(projet$data[[input$dataClassWeight]]),
                           selected = input$RowWeightClass)
             }
             
             
           ))
           
           
    
  })
  
  
  observeEvent(input$DoRenderPlot,
               switch(input$plottype,
                      
                      "Label" = output$PlotVisualisation <- renderPlot({
                        isolate({
                        if (input$objectLabel %in% names(projet$data)){
                          tryCatch({
                            projet$plot <- ade4:::s.label(projet$data[[input$objectLabel]], 
                                                  xax = input$xLabel,
                                                  yax = input$yLabel)
                                                  #add.plot = input$AddPlot)
                            
                          }, error = function(e){
                            alert("The dataframe is not suited for the plot")
                            print(e)
                            return(0)
                            }
                          )
                        }
                        
                        else if (input$objectLabel %in% names(projet$dudi)){
                          tryCatch({
                            projet$plot <- ade4:::s.label(projet$dudi[[input$objectLabel]][[input$xyLabel]],
                                                  xax = input$xLabel,
                                                  yax = input$yLabel) 
                                                  #add.plot = input$AddPlot)
                            
                          }, error = function(e){
                            alert("The dataframe is not suited for the plot")
                            print(e)
                            return(0)
                          }
                          )
                        }
                    })}),
                    
                    
                    "Class" = output$PlotVisualisation <- renderPlot({
                      isolate({
                        
                        if (input$objectClass %in% names(projet$data))
                          df <- projet$data[[input$objectClass]]
                        
                        else
                          df <- projet$dudi[[input$objectClass]][[input$xyClass]]
                        
                        fact <- projet$data[[input$dataClassFactor]][,input$ClassGroupingFactor]
                        
                        if (input$dataClassWeight == "none")
                          rw <- rep(1, length(fact))
                        
                        else
                          rw <- projet$data[[input$dataClassWeight]][,input$RowWeightClass]
                          
                        
                        tryCatch({
                          projet$plot <- ade4:::s.class(dfxy = df, fac = fact, wt = rw,
                                         xax = input$xClass,
                                         yax = input$yClass) 
                          
                        }, error = function(e){
                          alert("The dataframe is not suited for the plot")
                          print(e)
                          return(0)
                        }
                        )
                        
                        
                      })
                    })
                      
                    ))
  
  
  output$exportplot <- downloadHandler(
    filename = function(){
      paste("plot_ade4", input$plotformat, sep = ".")}
    ,
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      tryCatch({
      if(input$plotformat == "pdf")
        pdf(file, width = input$width/2.54, height = input$height/2.54)
      
      else if (input$plotformat == "png")
        png(file, width = input$width, height = input$height, units = 'cm', res = 72 * input$width / 10)
      
      else
        jpeg(file, width = input$width, height = input$height, units = "cm", res = 72 * input$width / 10)
      
      eval(projet$plot)
      dev.off()
      }, error = function(e){
        alert("There has been an error (printed in R console)")
        print(e)
        return(0)
      })
    } 
  )
  
}