############# Sourcing modules
# source("modules/LoadData.R")
# source("modules/ACP.R")
# source("modules/Visualisation.R")
# source("modules/COA.R")
# source("modules/BGA.R")
# source("modules/Coinertia.R")

header <- dashboardHeader(title = "Dashboard ade4 example")

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Manage data", tabName = "managedata"),
  menuItem("One table analyses",
           menuSubItem("PCA", tabName = "pca"),
           menuSubItem("COA", tabName = "coa"),
           menuSubItem("MCA", tabName = "mca"),
           menuSubItem("PCO")),
  
  menuItem("One table with groups",
           menuSubItem("BGA", tabName = "bga"),
           menuSubItem("WGA"),
           menuSubItem("DA")),
  menuItem("Two tables analyses",
           menuSubItem("Coinertia", tabName = "coinertia"),
           menuSubItem("CCA"),
           menuSubItem("PCAIV")),
  menuItem("Visualisation", tabName = "visualisation")
))


body <- dashboardBody(
  useShinyjs(),
  tabItems(
    LoadData,
    acp,
    coa,
    mca,
    bga,
    coinertie,
    visu
))

dashboardPage(skin = "green",
  header,
  sidebar,
  body
)


