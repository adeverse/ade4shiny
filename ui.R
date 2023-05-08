############# Sourcing modules
# source("modules/LoadData.R")
# source("modules/ACP.R")
# source("modules/Visualisation.R")
# source("modules/COA.R")
# source("modules/BGA.R")
# source("modules/Coinertia.R")

header <- dashboardHeader(title = "Dashboard ade4 example", 
                          tags$li(class = "dropdown", 
                                  downloadButton("savedata", "Save data",
                                                 style = "color : white; background-color : #58d68d")),
                          tags$li(class = "dropdown",
                                  downloadButton("savecode", "Save code", 
                                                 style = "color : white; background-color : #58d68d")))

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Manage data",
           menuSubItem("Load data", tabName = "managedata"),
           menuSubItem("Subset data", tabName = "subsetdata")),
  menuItem("One table analyses",
           menuSubItem("PCA", tabName = "pca"),
           menuSubItem("COA", tabName = "coa"),
           menuSubItem("MCA", tabName = "mca")
           # menuSubItem("PCO", tabName = "pco")),
  ),
  
  menuItem("One table with groups",
           menuSubItem("BGA", tabName = "bga")
           # menuSubItem("WGA"),
           # menuSubItem("DA")),
  ),
  menuItem("Two tables analyses",
           menuSubItem("Coinertia", tabName = "coinertia"),
           #menuSubItem("CCA", tabName = "cca"),
           menuSubItem("PCAIV", tabName = "pcaiv")),
  menuItem("Visualisation", tabName = "visualisation")
))


body <- dashboardBody(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    # tags$link(rel = "stylesheet", href = "https://use.fontawesome.com/releases/v5.8.1/css/all.css", 
    #                     integrity = "sha384-50oBUHEmvpQ+86rDHNDJKPXuL8PAOGYrS6b7Lwv+2XN2S6JkleUpGSnLcJvkinD+", 
    #                     crossorigin = "anonymous")
    
  ),
  tabItems(
    LoadData,
    subsetdata,
    acp,
    coa,
    mca,
    pco,
    bga,
    coinertie,
    cca,
    pcaIV,
    visu
))

dashboardPage(
  header,
  sidebar,
  body
)