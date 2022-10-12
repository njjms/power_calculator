# This Shiny web application uses the Natera "biostatslib" R package
# This package can be downloaded via devtools::install_git("ssh://git@stash.natera.com:7999/~nsun/biostatslib.git")
# Must be connected to the VPN and have SSH keys enabled on Natera stash account

library(shiny)
library(shinyWidgets)
library(biostatslib)
library(tidyverse)
library(plotly)
library(DT)
library(shinydashboard)
library(markdown)

# Source ui and server functions
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)
