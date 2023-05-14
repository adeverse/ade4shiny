# Header with buttons to save data and code
header <- dashboardHeader(title = "ADE4", 
                          tags$li(class = "dropdown", 
                                  downloadButton("savedata", "Save data",
                                                 style = "color : white; background-color : #117d89;
                                                 border-color #094d4e")),
                          tags$li(class = "dropdown",
                                  downloadButton("savecode", "Save code", 
                                                 style = "color : white; background-color : #117d89;
                                                 border-color: #094d4e")))

# Side bar with all the tabs
## Commented tabs are tabs done in ade4tkgui but not implemented in this app yet
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Welcome to ade4", tabName = "introtab"),
  menuItem("Manage data", startExpanded = TRUE,
           menuSubItem("Load data", tabName = "managedata"),
           menuSubItem("Subset data", tabName = "subsetdata")),
  menuItem("One table analyses", startExpanded = TRUE,
           menuSubItem("PCA", tabName = "pca"),
           menuSubItem("COA", tabName = "coa"),
           menuSubItem("MCA", tabName = "mca")
           # menuSubItem("PCO", tabName = "pco")),
  ),
  
  menuItem("One table with groups", startExpanded = TRUE,
           menuSubItem("BGA", tabName = "bga")
           # menuSubItem("WGA"),
           # menuSubItem("DA")),
  ),
  menuItem("Two tables analyses", startExpanded = TRUE,
           menuSubItem("Coinertia", tabName = "coinertia"),
           #menuSubItem("CCA", tabName = "cca"),
           menuSubItem("PCAIV", tabName = "pcaiv")),
  menuItem("Visualisation", tabName = "visualisation")
))


body <- dashboardBody(
  useShinyjs(),
  # Add custom css color (lbbe logo colors to header)
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$link(rel = "stylesheet", href = "https://use.fontawesome.com/releases/v5.8.1/css/all.css", 
                        integrity = "sha384-50oBUHEmvpQ+86rDHNDJKPXuL8PAOGYrS6b7Lwv+2XN2S6JkleUpGSnLcJvkinD+", 
                        crossorigin = "anonymous"),
    tags$style(".main-header{background-color: #094d4e}"),
    tags$style(".skin-blue .main-header .navbar{background-color: #094d4e}"),
    tags$style(".skin-blue .main-header .logo {background-color: #094d4e}"),
    tags$style(".skin-blue .main-header .navbar .nav>li>a {color: #094d4e}"),
    tags$style("#savedata {border-color: #094d4e}")
    
  ),
  tabItems(
    # Done in corresponding modules in the R/ folder
    introtab,
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