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
      numericInput("width", "Plot width (px)", value = 2048),
      numericInput("height", "Plot height (px)", value = 1024),
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
             
           )
           
           )
    
  })
  
  
  observeEvent(input$DoRenderPlot,
               switch(input$plottype,
                      
                      "Label" = output$PlotVisualisation <- renderPlot({
                        isolate({
                        if (input$objectLabel %in% names(projet$data)){
                          tryCatch({
                            ade4:::s.label(projet$data[[input$objectLabel]], 
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
                            ade4:::s.label(projet$dudi[[input$objectLabel]][[input$xyLabel]],
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
                    })})
                      
                    ))
  
  
  output$exportplot <- downloadHandler(
    filename = function(){
      paste("plot_ade4", input$plotformat, sep = ".")}
    ,
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$plotformat == "pdf")
        pdf(file)
      
      else if (input$plotformat == "png")
        png(file)
      
      else
        jpeg(file)
      
      dev.off()
    } 
  )
  
}