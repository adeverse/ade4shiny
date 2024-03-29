# Permet de choisir un plot à faire, de choisir les options correspondantes,
# de l'afficher et de l'exporter au format voulu

visu <- tabItem("visualisation",
                h2("Creating graphical outputs"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      selectInput("plottype", "Plot", 
                  choices = c("Label", "Class")),
      hr(),
      uiOutput("visualisation_listoption"),
      #checkboxInput("AddPlot", "Add to plot"),
      actionButton("DoRenderPlot", "Compute plot", 
                   style = "color : white; background-color : #93bf29"),
      hr(),
      selectInput("plotformat", "Picture extension", c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg")),
      numericInput("width", "Plot width (cm)", value = 14),
      numericInput("height", "Plot height (cm)", value = 9),
      downloadButton("exportplot", "Export plot", style = "color : white; background-color : #93bf29")
    ),
    mainPanel = mainPanel(
      plotOutput("PlotVisualisation")
    )
  )
)


visuServer <- function(input, output, session, projet){
  
  # Permet d'adapter l'ui en fonction du type de plot
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
                              choices = c("Individuals" = "li", "Variables" = "co"),
                              selected = input$xyLabel)
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
                               choices = c("Individuals" = "li", "Variables" = "co"),
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
  
  # Permet d'adapter le calcul du plot en fonction du type de plot
  observeEvent(input$DoRenderPlot,
               switch(input$plottype,
                      
                      "Label" = output$PlotVisualisation <- renderPlot({
                        isolate({
                        if (input$objectLabel %in% names(projet$data)){
                          tryCatch({
                            projet$plot <- ade4:::s.label(projet$data[[input$objectLabel]], 
                                                  xax = input$xLabel,
                                                  yax = input$yLabel)
                            # Ajout du code pour faire le plot dans projet$code
                            string <- paste("s.label(", input$objectLabel, ", xax = ", 
                                            input$xLabel, ", yax = ", input$yLabel, ")", sep = "")
                            
                            projet$code <- paste(projet$code, string, sep = "\n\n# Computing Label plot\n")
                            
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
                            string <- paste("s.label(", input$objectLabel, "$", input$xyLabel, 
                                            ", xax = ", input$xLabel, ", yax = ", input$yLabel, ")", sep = "")
                            
                            projet$code <- paste(projet$code, string, sep = "\n\n# Computing Label plot\n")
                            
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
                        
                        if (input$objectClass %in% names(projet$data)){
                          df <- projet$data[[input$objectClass]]
                          string_df <- input$objectClass
                        }
                        
                        else{
                          df <- projet$dudi[[input$objectClass]][[input$xyClass]]
                          string_df <- paste(input$objectClass, "$", input$xyClass, sep = "")
                          
                          if ("between" %in% class(projet$dudi[[input$objectClass]]) & input$xyClass == "li"){
                            df <- projet$dudi[[input$objectClass]]$ls
                            string_df <- paste(input$objectClass, "$ls", sep = "")
                          }
                        }
                        
                        fact <- as.factor(projet$data[[input$dataClassFactor]][,input$ClassGroupingFactor])
                        string_fact <- paste("as.factor(", input$dataClassFactor, "$", 
                                             input$ClassGroupingFactor, ")", sep = "")
                        
                        if (input$dataClassWeight == "none"){
                          rw <- rep(1, length(fact))
                          string_rw <- paste("rep(1, ",length(fact), ")", sep = "")
                        }
                        
                        else{
                          rw <- projet$data[[input$dataClassWeight]][,input$RowWeightClass]
                          string_rw <- paste(input$dataClassWeight, "$", input$RowWeightClass, sep = "")
                        }
                        
                        
                        tryCatch({
                          projet$plot <- ade4:::s.class(dfxy = df, fac = fact, wt = rw,
                                         xax = input$xClass,
                                         yax = input$yClass) 
                          
                          string <- paste("s.class(dfxy = ", string_df, ", fac = ", string_fact,
                                          ", wt = ", string_rw, ", xax = ", input$xClass,
                                          ", yax = ", input$yClass, ")",sep = "")
                          
                          projet$code <- paste(projet$code, string, sep = "\n\n# Computing Class plot\n")
                          
                        }, error = function(e){
                          alert("The dataframe is not suited for the plot")
                          print(e)
                          return(0)
                        }
                        )
                        
                        
                      })
                    })
                      
                    ))
  
  
  # Permet d'exporter le plot avec les dimensions voulues au format voulu
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