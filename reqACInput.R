#
# Shiny UI Module for requirements and acceptance criteria (AC) setting
# in power calculation app.
#

direction <- function(id) {
    selectInput(
        inputId = ns("direction"),
        label = "Requirement Type",
        choices = list("Greater or equal to" = 1,
                       "Lesser or equal to" = 2),
        selected = 1
    )
}

requirement <- function(id) {
    numericInput(
      inputId = ns("requirement"),
      label = "Specified Requirement",
      value = .95,
      min = 0,
      max = 1,
      step = 0.01
    )
}

test <- function(id) {
    radioButtons(inputId = ns("test"),
                 label = "Hypothesis Test Method",
                 choices = list("Wilson-Score" = 1,
                                "Clopper-Pearson" = 2),
                 selected = 1)
}

AC_type <- function(id) {
    selectInput(inputId = ns("AC_type"),
                label="Acceptance Criteria Risk Level",
                choices = list("Low" = 1,
                               "Medium" = 2,
                               "High" = 3,
                               "High Delta" = 4),
                selected = 2),
    conditionalPanel(
      condition = "input.AC_type == 4",
      numericInput(
        inputId = "prq_delta",
        label = "Specified Requirement \u00B1 \u03B4", # ± δ
        value = .94,
        step = 0.01,
        min = 0,
        max = 1
      )
    )
}
  
