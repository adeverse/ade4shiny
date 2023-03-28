server <- function(input, output, session) { 
  
  projet <- reactiveValues(data = list(),
                           dudi = list())
  
  # Load data
  LoadDataServer(input, output, session, projet)
  
  # ACP
  acpServer(input, output, session, projet)
  
  # COA
  coaServer(input, output, session, projet)
  
  
  # BGA
  bgaServer(input, output, session, projet)
  
  # Coinertia
  coinertiaserver(input, output, session, projet)
  
  
  # Visualisation
  visuServer(input, output, session, projet)
  
}
