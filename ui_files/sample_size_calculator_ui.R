#
# UI function which defines layout of sample size calculator app
#

sample_size_calculator_ui <- 
        tabItem(tabName = "sample_size_calculator",
          titlePanel("Sample Size Calculator"),
          sidebarLayout(
              sidebarPanel(
                  #
                  # Select greater than or equal to and the numeric requirement
                  #
                  selectInput(
                      inputId = "direction",
                      label = "Requirement Type",
                      choices = list("Greater or equal to" = 1,
                                     "Lesser or equal to" = 2),
                      selected = 1
                  ),
                  numericInput(
                    inputId = "requirement",
                    label = "Specified Requirement",
                    value = .95,
                    min = 0,
                    max = 1,
                    step = 0.01
                  ),
                  #
                  # Input true probability vector
                  #
                  textInput(
                    inputId = "true_p",
                    label = "True Probability (comma-separated)",
                    value = "0.95",
                  ),
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
                  #
                  # Select minimum sample size
                  #
                  numericInput(
                    inputId = "n_minimum",
                    label = "Minimum sample size",
                    value = 5,
                    min = 1,
                    step = 1
                  ),
                  #
                  # Select maximum sample size
                  #
                  numericInput(
                    inputId = "n_maximum",
                    label = "Maximum sample size",
                    value = 100,
                    min = 1,
                    step = 1
                  ),
                  #
                  # Select step size
                  #
                  numericInput(
                    inputId = "step",
                    label = "Step size",
                    value = 1,
                    min = 1,
                    step = 1
                  ),
                  #
                  # Select the hypothesis test methods to compare
                  #
                  radioButtons(inputId = "test",
                               label = "Hypothesis Test Method",
                               choices = list("Wilson-Score" = 1,
                                              "Clopper-Pearson" = 2),
                               selected = 1),
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
                  actionButton(inputId = "run_calculation",
                               label = "Calculate"),
                  h5("Plot Options"),
                  checkboxInput(inputId = "VerboseTitle", label = "Include Test Type in Title"),
                  checkboxInput(inputId = "FlipY", label = "Flip Y-Axis")
              ),
              mainPanel(
                 plotlyOutput(outputId = "curvePlot"),
                 DT::dataTableOutput(outputId = "curvePlotData")
              )
          )
      )
