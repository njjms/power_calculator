#
# UI function which defines layout of sample size calculator app
#

sample_size_calculator_ui <- 
        tabItem(tabName = "sample_size_calculator",
          titlePanel("Explore Sample Sizes"),
          sidebarLayout(
              sidebarPanel(
                  #
                  # Select greater than or equal to and the numeric requirement
                  #
                  fluidRow(
                    column(6,
                      selectInput(
                          inputId = "direction",
                          label = "Requirement Type",
                          choices = list("Greater or equal to" = 1,
                                         "Lesser or equal to" = 2),
                          selected = 1)
                    ),
                    column(6,
                      numericInput(
                        inputId = "requirement",
                        label = "Specified Requirement",
                        value = .95,
                        min = 0,
                        max = 1,
                        step = 0.01)
                    )
                  ),
                  selectInput(inputId = "AC_type",
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
                      label = "Specified Requirement \u00B1 \u03B4",
                      value = .94,
                      step = 0.01,
                      min = 0,
                      max = 1
                    )
                  ),
                  conditionalPanel(
                    condition = "input.AC_type != 2",
                    #
                    # Select the confidence interval methods
                    #
                    radioButtons(inputId = "test",
                                 label = "Confidence Interval Method",
                                 choices = list("Wilson-Score" = 1,
                                                "Clopper-Pearson" = 2),
                                 selected = 1),
                    #
                    # Select the significance level
                    #
                    numericInput(
                      inputId = "alpha",
                      label = "Significance Level",
                      min = .0000000001,
                      max = .2,
                      value = .05,
                      step = 0.005
                    ),
                  helpText("Note: This is for 2-sided intervals."),
                  checkboxInput(inputId = "VerboseTitle", label = "Include CI in Title")
                  ),
                  #
                  # Input true probability vector
                  #
                  textInput(
                    inputId = "true_p",
                    label = "True Probabilities",
                    value = "0.95",
                  ),
                  helpText("Can enter comma-separated values (e.g. .95, .96, .97)"),
                  #
                  # Select sample size range
                  #
                  fluidRow(
                    column(4,
                      numericInput(
                        inputId = "n_minimum",
                        label = "Min sample size",
                        value = 5,
                        min = 1,
                        step = 1
                      )
                    ),
                    column(4,
                      numericInput(
                        inputId = "n_maximum",
                        label = "Max sample size",
                        value = 100,
                        min = 1,
                        step = 1
                      )
                    ),
                    column(4,
                      numericInput(
                        inputId = "step",
                        label = "Step size",
                        value = 1,
                        min = 1,
                        step = 1
                      )
                    )
                  ),
                  actionButton(inputId = "run_calculation",
                               label = "Calculate")
              ),
              mainPanel(
                plotlyOutput(outputId = "curvePlot"),
                fluidRow(
                  column(2,
                    h4("Plot Options:")
                  ),
                  column(10,
                    checkboxInput(inputId = "FlipY", label = "Flip Y-Axis")
                  )
                ),
                DT::dataTableOutput(outputId = "curvePlotData")
              )
          )
      )
