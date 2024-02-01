## Setting working directory to shiny directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Loading libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ade4)
library(adegraphics)
library(DT)
library(xfun)
library(factoextra)
library(shinyBS)

# Run the shiny app
runApp(".")