#
# UI function which defines layout of single sample size calculator app
#

single_size_calculator_ui <- 
      tabItem(tabName = "single_size_calculator",
              titlePanel("Single Size Calculator"),
              sidebarLayout(
                sidebarPanel(
                   #
                   # Select greater than or equal to and the numeric requirement
                   #
                   selectInput(
                       inputId = "direction3",
                       label = "Requirement Type",
                       choices = list("Greater or equal to" = 1,
                                      "Lesser or equal to" = 2),
                       selected = 1
                   ),
                   #
                   # Requirement
                   #
                   numericInput(
                         inputId = "requirement3",
                         label = "Specified Requirement",
                         value = .95,
                         min = 0,
                         max = 1,
                         step = 0.01
                   ),
                  #
                  # True probability
                  #
                  numericInput(
                    inputId = "true_probability",
                    label = "True Probability",
                    value = .95,
                    min = 0,
                    max = 1
                  ),
                  #
                  # Select the significance level
                  #
                  numericInput(
                    inputId = "alpha3",
                    label = "Significance Level",
                    min = .0000000001,
                    max = .2,
                    value = .05,
                    step = 0.005
                  ),
                  helpText("Note: This is for 2-sided intervals."),
                  #
                  # Input selected sample sizes
                  #
                  numericInput(
                    inputId = "sample_size3",
                    label = "Enter sample size",
                    value = "100",
                  ),
                  #
                  # Effect size hypothesis test options
                  #
                  radioButtons(inputId = "test3",
                               label = "Hypothesis Test Method",
                               choices = list("Wilson-Score" = 1,
                                              "Clopper-Pearson" = 2),
                               selected = 1),
                  selectInput(inputId = "AC_type3",
                              label="Acceptance Criteria Risk Level",
                              choices = list("Low" = 1,
                                             "Medium" = 2,
                                             "High" = 3,
                                             "High Delta" = 4),
                              selected = 2),
                  conditionalPanel(
                    condition = "input.AC_type3 == 4",
                    numericInput(
                      inputId = "prq_delta3",
                      label = "Specified Requirement \u00B1 \u03B4",
                      value = .94,
                      step = 0.01,
                      min = 0,
                      max = 1
                    )
                  ),
                  actionButton(inputId = "run_calculation3",
                               label = "Calculate"),
                ),
                mainPanel(
                   textOutput("single_sample_size_power"),
                   textOutput("single_sample_size_max_misses"),
                   br(),
                   plotlyOutput(outputId = "curvePlot3"),
                   DT::dataTableOutput(outputId = "single_sample_size_data")
                )
              )
      ) # end of single sample size calculator tab