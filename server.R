server <- function(input, output, session) { 
  
  projet <- reactiveValues(data = list(), # List of all dataframe
                           dudi = list(), # List of all dudi objects
                           plot = character(0), # Useful for visualisation
                           code = "library(ade4)\n") # Useful to write and download code
  
  
  # Save data button
  output$savedata <- downloadHandler(
    filename = "ade4_project.rds",
    content = function(file) {
      
      object <- list(data = projet$data, dudi = projet$dudi)
      saveRDS(object, file)
    }
  )
  
  # Save code button
  output$savecode <- downloadHandler(
    filename = "ade4_R_code.txt",
    content = function(file) {
      
      writeLines(projet$code, file)
    }
  )
  
  # R/LoadData.R
  LoadDataServer(input, output, session, projet)
  
  # R/SubsetData.R
  SubsetDataServer(input, output, session, projet)
  
  # R/ACP.R
  acpServer(input, output, session, projet)
  
  # R/COA.R
  coaServer(input, output, session, projet)
  
  # R/MCA.R
  mcaServer(input, output, session, projet)
  
  # R/PCO.R
  ## pcoServer(input, output, session, projet)
  
  # R/BGA.R
  bgaServer(input, output, session, projet)
  
  # R/Coinertia.R
  coinertiaserver(input, output, session, projet)
  
  # R/CCA.R
  ## ccaserver(input, output, session, projet)
  
  # R/PCAIV.R
  pcaIVserver(input, output, session, projet)
  
  # R/Visualisation.R
  visuServer(input, output, session, projet)
  
  
}


