# This Shiny web application uses my personal R package "nicks"
# This package can be downloaded via devtools::install_github("njjms/nicks")

library(shiny)
library(shinyWidgets)
library(nicks)
library(tidyverse)
library(plotly)
library(DT)
library(shinydashboard)

ui <- dashboardPage(

    dashboardHeader(title = "Power Calculations"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Sample Size Calculator",
                 tabName = "sample_size_calculator",
                 icon = icon("th")),
        menuItem("Effect Size Calculator",
                 tabName = "effect_size_calculator",
                 icon = icon("th")),
        menuItem("Help Page",
                 tabName = "help_page",
                 icon = icon("dashboard"))
      )
    ),
    dashboardBody(
      tabItems(
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
                      choices = list("≥" = 1,
                                     "≤" = 2),
                      selected = 1
                  ),
                  conditionalPanel(
                    condition = 'input.direction == 1',
                    numericInput(
                        inputId = "gt_requirement",
                        label = "Specified Requirement",
                        value = .95
                    )
                  ),
                  conditionalPanel(
                    condition = 'input.direction == 2',
                    numericInput(
                        inputId = "lt_requirement",
                        label = "Specified Requirement",
                        value = .039
                    )
                  ),
                  #
                  # Input true probability value
                  #
                  numericInput(
                      inputId = "true_p",
                      label = "True Probability",
                      value = .95
                  ),
                  #
                  # Select the significance level
                  #
                  sliderInput(
                    inputId = "alpha",
                    label = "Significance Level",
                    min = .01,
                    max = .2,
                    value = .1
                  ),
                  helpText("Note: This is for 2-sided intervals."),
                  #
                  # Select minimum sample size
                  #
                  numericInput(
                    inputId = "n_minimum",
                    label = "Minimum sample size",
                    value = 5
                  ),
                  #
                  # Select maximum sample size
                  #
                  numericInput(
                    inputId = "n_maximum",
                    label = "Maximum sample size",
                    value = 100
                  ),
                  #
                  # Select step size
                  #
                  numericInput(
                    inputId = "step",
                    label = "Step size",
                    value = 1
                  ),
                  #
                  # Select the hypothesis test methods to compare
                  #
                  checkboxGroupInput(inputId = "tests",
                                     label = "Hypothesis Test Methods",
                                     choices = list("Wilson-Score" = 1,
                                                    "Clopper-Pearson" = 2,
                                                    "RBT" = 3),
                                     selected = c(1)),
                  h5("Confidence Interval Options"),
                  checkboxInput(inputId = "intervalSurpasses",
                                label = "Lower Bound Surpasses",
                                value = FALSE),
                  checkboxInput(inputId = "pointSurpasses",
                                label = "Point Estimate Surpasses",
                                value = FALSE),
                  actionButton(inputId = "run_calculation",
                               label = "Calculate")
              ),
              mainPanel(
                 plotlyOutput(outputId = "curvePlot"),
                 DT::dataTableOutput(outputId = "curvePlotData")
              )
          )
      ), # end of sample_size_calculator tab
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
                       choices = list("≥" = 1,
                                      "≤" = 2),
                       selected = 1
                   ),
                   conditionalPanel(
                     condition = 'input.direction2 == 1',
                     numericInput(
                         inputId = "gt_requirement2",
                         label = "Specified Requirement",
                         value = .95
                     )
                   ),
                   conditionalPanel(
                     condition = 'input.direction2 == 2',
                     numericInput(
                         inputId = "lt_requirement2",
                         label = "Specified Requirement",
                         value = .039
                     )
                   ),
                  #
                  # Select the significance level
                  #
                  sliderInput(
                    inputId = "alpha2",
                    label = "Significance Level",
                    min = .01,
                    max = .2,
                    value = .1
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
                  radioButtons(inputId = "tests2",
                               label = "Hypothesis Test Methods",
                               choices = list("Wilson-Score" = 1,
                                              "Clopper-Pearson" = 2),
                               selected = c(1)),
                  h5("Confidence Interval Options"),
                  checkboxInput(inputId = "intervalSurpasses2",
                                label = "Lower Bound Surpasses",
                                value = FALSE),
                  checkboxInput(inputId = "pointSurpasses2",
                                label = "Point Estimate Surpasses",
                                value = FALSE),
                  actionButton(inputId = "run_calculation2",
                               label = "Calculate")
                ),
                mainPanel(
                   plotlyOutput(outputId = "curvePlot2"),
                   DT::dataTableOutput(outputId = "curvePlotData2")
                )
              )
      ),
      tabItem(tabName = "help_page",
              h3("Help with Power Calculations"),
              headerPanel(
                includeMarkdown("help_page.md")
              )
      ) # end of help_page tab
    )
  )
)

server <- function(input, output, session) {

  #
  # Create blank dataframes to fill in
  #
  curve_plot_data <- reactiveValues(curve_plot_data =
                                      data.frame(
                                        sample_sizes = numeric(),
                                        power = numeric(),
                                        test = character()
                                      ))

  curve_plot_data2 <- reactiveValues(curve_plot_data2 =
                                      data.frame(
                                        requirement = numeric(),
                                        true_probs = numeric(),
                                        power = numeric(),
                                        test = character(),
                                        sample_size = numeric()
                                      ))

  #
  # Sample size calculator server components
  #
  requirement <- reactiveValues(requirement = NA)
  requirement_direction <- reactiveValues(requirement_direction = NA)

  observeEvent(
    input$run_calculation, {
      requirement_direction$requirement_direction <- input$direction
      requirement_type <- if_else(requirement_direction$requirement_direction == 1, "gt", "lt")
      requirement$requirement <- if_else(requirement_type == "gt", input$gt_requirement, input$lt_requirement)
      sample_sizes <- seq(input$n_minimum,
                          input$n_maximum,
                          by = input$step)
      hypothesis_tests <- input$tests
      add_curve_plot_data <- list()
      for (test in hypothesis_tests) {
        if (test == "1") {
          ws_power_calc <- sapply(sample_sizes,
                                  FUN = function(x) {
                                    power_calc(sample_size = x,
                                      true_prob = input$true_p,
                                      requirement = requirement$requirement,
                                      alpha= input$alpha,
                                      requirement_type= requirement_type,
                                      interval_type="ws",
                                      interval_surpasses=input$intervalSurpasses,
                                      point_surpasses = input$pointSurpasses)$power
                                  })
          ws_power_df <- data.frame(
                n = sample_sizes,
                power = ws_power_calc,
                test = rep("Wilson-Score",
                           length(sample_sizes))
          )
          add_curve_plot_data[[length(add_curve_plot_data) + 1]] <- ws_power_df
        }
        if (test == "2") {
          cp_power_calc <- sapply(sample_sizes,
                                  FUN = function(x) {
                                    power_calc(sample_size = x,
                                      true_prob = input$true_p,
                                      requirement = requirement$requirement,
                                      alpha= input$alpha,
                                      requirement_type = requirement_type,
                                      interval_type = "cp",
                                      interval_surpasses = input$intervalSurpasses,
                                      point_surpasses = input$pointSurpasses)$power
                                  })
        cp_power_df <- data.frame(
              n = sample_sizes,
              power = cp_power_calc,
              test = rep("Clopper-Pearson",
                         length(sample_sizes))
        )
          add_curve_plot_data[[length(add_curve_plot_data) + 1]] <- cp_power_df
        }
        if (test == "3") {
          rbt_power_calc <- sapply(sample_sizes,
                                   FUN = function(x) {
                                     rbt_power_calc(
                                       sample_size = x,
                                       true_p = input$true_p,
                                       requirement = requirement$requirement,
                                       alpha= input$alpha,
                                       requirement_type = requirement_type
                                     )$power
                                  })
          rbt_power_df <- data.frame(
                n = sample_sizes,
                power = rbt_power_calc,
                test = rep("RBT", length(sample_sizes))
          )
          add_curve_plot_data[[length(add_curve_plot_data) + 1]] <- rbt_power_df
        }
      }
      add_curve_plot_data <- dplyr::bind_rows(add_curve_plot_data)
      curve_plot_data$curve_plot_data <- add_curve_plot_data
      },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

    output$curvePlot <- renderPlotly({
      validate(
        need(input$n_minimum <= input$n_maximum,
             'Minimum sample size must be less than or equal to the maximum sample size.'),
        need(dplyr::between(input$gt_requirement, 0, 1),
             'Requirement must be between 0 and 1'),
        need(dplyr::between(input$lt_requirement, 0, 1),
             'Requirement must be between 0 and 1'),
        need(dplyr::between(input$true_p, 0, 1),
             'True probability must be between 0 and 1')
      )
      if (nrow(curve_plot_data$curve_plot_data) == 0) {
        p <- ggplot() +
          scale_x_continuous("n",
                             limits = c(input$n_minimum,
                                        input$n_maximum)) +
          scale_y_continuous("Probability of Rejection",
                             limits = c(0, 1)) +
          theme(
            legend.position = "none",
            panel.grid.minor.x = element_blank()
          )
        ggplotly(p)
      } else {
        p<- ggplot(curve_plot_data$curve_plot_data) +
        	    geom_line(mapping = aes(x=n,
        	                            y=power,
        	                            color=test)) +
              labs(y = "Probability of Rejection") +
        	    theme_bw()
        ggplotly(p) %>%
          layout(
            legend = list(
              title = "test",
              orientation = "h",
              x = .4,
              y = -.2
            )
          )
      }
    })
    output$curvePlotData <- DT::renderDataTable({
      if (nrow(curve_plot_data$curve_plot_data) == 0) {
        data.frame(
          sample_sizes = numeric(),
          power = numeric(),
          test = character())
      } else {
        curve_plot_data$curve_plot_data$requirement <- rep(input$requirement,
                                                           nrow(curve_plot_data$curve_plot_data))
        curve_plot_data$curve_plot_data$true_p <- rep(input$true_p,
                                                      nrow(curve_plot_data$curve_plot_data))
        curve_plot_data$curve_plot_data$alpha <- rep(input$alpha,
                                                     nrow(curve_plot_data$curve_plot_data))
        curve_plot_data$curve_plot_data
      }
    },
    extensions = c("Buttons", "Scroller"),
    options = list(
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 400,
      scroller = TRUE,
      buttons = c('copy', 'csv', 'excel')
    )
  )

  #
  # Effect size calculator components
  #
  selected_sample_sizes <- reactiveValues(sizes = 100)
  true_probs <- reactiveValues(true_probs = vector(mode = "numeric"))
  requirement2 <- reactiveValues(requirement2 = .95)
  requirement_type2 <- reactiveValues(requirement_type2 = NA)
  test_type <- reactiveValues(test_type = "ws")

  observeEvent(
    input$run_calculation2, {
      requirement_type2$requirement_type2 <- if_else(input$direction2 == 1, "gt", "lt")
      test_type$test_type <- case_when(input$tests2 == 1 ~ "ws", input$tests2 == 2 ~ "cp")
      selected_sample_sizes$sizes <- as.numeric(trimws(unlist(strsplit(input$sample_sizes2,","))))
      selected_sample_sizes$sizes <- selected_sample_sizes$sizes[!is.na(selected_sample_sizes$sizes)]
      requirement2$requirement2 <- if_else(requirement_type2$requirement_type2 == "gt",
                                           input$gt_requirement2,
                                           input$lt_requirement2)

      if (requirement_type2$requirement_type2 == "gt") {
         true_probs$true_probs <- requirement2$requirement2 - seq(0,
                                                                  input$effect_size_neighborhood,
                                                                  by = input$effect_size_step)
         true_probs$true_probs <- true_probs$true_probs[(true_probs$true_probs >= 0 &
                                                           (true_probs$true_probs <= 1))]
      } else {
         true_probs$true_probs <- requirement2$requirement2 + seq(0,
                                                                  input$effect_size_neighborhood,
                                                                  by = input$effect_size_step)
         true_probs$true_probs <- true_probs$true_probs[(true_probs$true_probs >= 0 &
                                                           (true_probs$true_probs <= 1))]
      }
      hypothesis_tests <- input$tests2
      add_curve_plot_data2 <- list()
      for (sample_size in selected_sample_sizes$sizes) {
          ws_power_calc <- sapply(true_probs$true_probs,
                                  FUN = function(x) {
                                    power_calc(sample_size = sample_size,
                                      true_prob = x,
                                      requirement = requirement2$requirement2,
                                      alpha= input$alpha2,
                                      requirement_type= requirement_type2$requirement_type2,
                                      interval_type=test_type$test_type,
                                      interval_surpasses=input$intervalSurpasses2,
                                      point_surpasses = input$pointSurpasses2)$power
                                  })
          tmp_power_df <- data.frame(
                requirement = rep(requirement2$requirement2,
                                  length(true_probs$true_probs)),
                true_probs = true_probs$true_probs,
                power = ws_power_calc,
                test = rep(test_type$test_type,
                           length(true_probs$true_probs)),
                sample_size = rep(sample_size, length(true_probs$true_probs))
          )
          add_curve_plot_data2$curve_plot_data2[[length(add_curve_plot_data2$curve_plot_data2) + 1]] <- tmp_power_df
    }
    curve_plot_data2$curve_plot_data2 <- dplyr::bind_rows(add_curve_plot_data2)
  },
  ignoreNULL = TRUE,
  ignoreInit = TRUE)

    output$curvePlot2 <- renderPlotly({
      validate(
        need(dplyr::between(requirement2$requirement2, 0, 1),
             'Requirement must be between 0 and 1'),
        need(dplyr::between(input$effect_size_step, 0, input$effect_size_neighborhood),
             'Effect size step must be less than effect size maximum'),
        need(is.numeric(selected_sample_sizes$sizes),
             'Sample sizes must be numeric.')
      )
      if (nrow(curve_plot_data2$curve_plot_data2) == 0) {
        p <- ggplot() +
          scale_x_continuous("Effect Size",
                             limits = c(0, 1)) +
          scale_y_continuous("Probability of Rejection",
                             limits = c(0, 1)) +
          theme(
            legend.position = "none",
            panel.grid.minor.x = element_blank()
          )
        ggplotly(p)
      } else {
        p<- ggplot(curve_plot_data2$curve_plot_data2) +
        	    geom_line(mapping = aes(x=true_probs,
        	                            y=power,
        	                            color=sample_size)) +
              labs(y = "Probability of Rejection",
                   x = "True Probability") +
        	    theme_bw()
        ggplotly(p) %>%
          layout(
            legend = list(
              title = "test",
              orientation = "h",
              x = .4,
              y = -.2
            )
          )
      }
    })
    output$curvePlotData2 <- DT::renderDataTable({
      if (nrow(curve_plot_data2$curve_plot_data2) == 0) {
        data.frame(
          requirement = numeric(),
          true_probs = numeric(),
          power = numeric(),
          test = character(),
          sample_size = numeric()
        )
      } else {
        curve_plot_data2$curve_plot_data2
      }
    },
    extensions = c("Buttons", "Scroller"),
    options = list(
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 400,
      scroller = TRUE,
      buttons = c('copy', 'csv', 'excel')
    )
  )
}

# Run the application
shinyApp(ui = ui, server = server)

