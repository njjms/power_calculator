# Loading dependencies
library(shiny)
library(shinyWidgets)
library(biostatslib)
library(tidyverse)
library(plotly)
library(DT)
library(shinydashboard)


# Source in UI for individual tabs
source("ui_files/sample_size_calculator_ui.R")
source("ui_files/effect_size_calculator_ui.R")
source("ui_files/single_size_calculator_ui.R")
source("ui_files/help_page_ui.R")

ui <- dashboardPage(
    dashboardHeader(title = "Power Calculations",
      tags$li(class = "dropdown",
              tags$a(href = "https://www.natera.com/",
                     target = "_blank",
                     tags$img(height = "20px",
                              alt = "Natera logo",
                              src = "https://upload.wikimedia.org/wikipedia/commons/d/db/Natera_logo.png"
                              )
                     )
              )
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Explore Sample Sizes",
                 tabName = "sample_size_calculator",
                 icon = icon("king", lib = "glyphicon", verify_fa = FALSE)),
        menuItem("Explore Effect Sizes",
                 tabName = "effect_size_calculator",
                 icon = icon("queen", lib = "glyphicon", verify_fa = FALSE)),
        menuItem("Visualize CIs",
                 tabName = "single_size_calculator",
                 icon = icon("pawn", lib = "glyphicon", verify_fa = FALSE)),
        menuItem("Help Page",
                 tabName = "help_page",
                 icon = icon("dashboard", verify_fa = FALSE))
      )
    ),
    dashboardBody(
      tags$head(
        includeCSS("www/natera_colorscheme.css")
      ),
      tabItems(
        sample_size_calculator_ui,
        effect_size_calculator_ui,
        single_size_calculator_ui,
        help_page_ui
      )
  )
)