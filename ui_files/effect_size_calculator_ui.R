#
# UI function which defines layout of effect size calculator app
#

effect_size_calculator_ui <- 
      tabItem(tabName = "effect_size_calculator",
              titlePanel("Effect Size Calculator"),
              sidebarLayout(
                sidebarPanel(
                   #
                   # Select greater than or equal to and the numeric requirement
                   #
                   selectInput(
                       inputId = "direction2",
                       label = "Requirement Type",
                       choices = list("Greater or equal to" = 1,
                                      "Lesser or equal to" = 2),
                       selected = 1
                   ),
                   numericInput(
                         inputId = "requirement2",
                         label = "Specified Requirement",
                         value = .95,
                         min = 0,
                         max = 1,
                         step = 0.01
                   ),
                  #
                  # Select the significance level
                  #
                  numericInput(
                    inputId = "alpha2",
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
                  textInput(
                    inputId = "sample_sizes2",
                    label = "Enter sample sizes (comma-separated)",
                    value = "100",
                  ),
                  #
                  # Effect size
                  #
                  sliderInput(
                    inputId = "effect_size_neighborhood",
                    label = "Select effect size range",
                    value = .1,
                    min = 0,
                    max = .2
                  ),
                  #
                  # Select effect size step
                  #
                  numericInput(
                    inputId = "effect_size_step",
                    label = "Effect size step intervals",
                    value = .01
                  ),
                  #
                  # Effect size hypothesis test options
                  #
                  radioButtons(inputId = "test2",
                               label = "Hypothesis Test Method",
                               choices = list("Wilson-Score" = 1,
                                              "Clopper-Pearson" = 2),
                               selected = 1),
                  selectInput(inputId = "AC_type2",
                              label="Acceptance Criteria Risk Level",
                              choices = list("Low" = 1,
                                             "Medium" = 2,
                                             "High" = 3,
                                             "High Delta" = 4),
                              selected = 2),
                  conditionalPanel(
                    condition = "input.AC_type2 == 4",
                    numericInput(
                      inputId = "prq_delta2",
                      label = "Specified Requirement \u00B1 \u03B4",
                      value = .94,
                      step = 0.01,
                      min = 0,
                      max = 1
                    )
                  ),
                  actionButton(inputId = "run_calculation2",
                               label = "Calculate"),
                  h5("Plot Options"),
                  checkboxInput(inputId = "VerboseTitle2", label = "Include Test Type in Title"),
                  checkboxInput(inputId = "FlipY2", label = "Flip Y-Axis")
                ),
                mainPanel(
                   plotlyOutput(outputId = "curvePlot2"),
                   DT::dataTableOutput(outputId = "curvePlotData2")
                )
              )
      )