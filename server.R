server <- function(input, output, session) { 
  
  projet <- reactiveValues(data = list(),
                           dudi = list(),
                           plot = character(0))
  
  
  output$savedata <- downloadHandler(
    filename = "ade4_project.rds",
    content = function(file) {
      
      object <- list(data = projet$data, dudi = projet$dudi)
      saveRDS(object, file)
    }
  )
  
  # Load data
  LoadDataServer(input, output, session, projet)
  
  # ACP
  acpServer(input, output, session, projet)
  
  # COA
  coaServer(input, output, session, projet)
  
  # MCA
  mcaServer(input, output, session, projet)
  
  # PCO
  pcoServer(input, output, session, projet)
  
  # BGA
  bgaServer(input, output, session, projet)
  
  # Coinertia
  coinertiaserver(input, output, session, projet)
  
  # CCA
  ccaserver(input, output, session, projet)
  
  # PCAIV
  pcaIVserver(input, output, session, projet)
  
  # Visualisation
  visuServer(input, output, session, projet)
  
  
}