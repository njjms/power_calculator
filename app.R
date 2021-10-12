# This Shiny web application uses my personal R package "nicks"
# This package can be downloaded via devtools::install_github("njjms/nicks")

library(shiny)
library(shinyWidgets)
library(nicks)
library(tidyverse)
library(plotly)
library(DT)

ui <- fluidPage(

    titlePanel("Power Calculator"),

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
                  inputId = "requirement",
                  label = "Specified Requirement",
                  value = .95
              )
            ),
            conditionalPanel(
              condition = 'input.direction == 2',
              numericInput(
                  inputId = "requirement",
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
              max = .1,
              value = .05
            ),
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
)

server <- function(input, output, session) {

  curve_plot_data <- reactiveValues(curve_plot_data =
                                      data.frame(
                                        sample_sizes = numeric(),
                                        power = numeric(),
                                        test = character()
                                      ))

  observeEvent(
    input$run_calculation, {
      requirement_type <- if_else(input$direction == 1,
                                  "gt",
                                  "lt")
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
                                      requirement = input$requirement,
                                      alpha= input$alpha*2,
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
                                      requirement = input$requirement,
                                      alpha= input$alpha*2,
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
                                       requirement = input$requirement,
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
        need(dplyr::between(input$requirement, 0, 1),
             'Requirement must be between 0 and 1'),
        need(dplyr::between(input$true_p, 0, 1),
             'True probability must be between 0 and 1')
      )
      if (nrow(curve_plot_data$curve_plot_data) == 0) {
        p <- ggplot() +
          scale_x_continuous("n",
                             limits = c(input$n_minimum,
                                        input$n_maximum)) +
          scale_y_continuous("power",
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
    ))
}

# Run the application
shinyApp(ui = ui, server = server)

