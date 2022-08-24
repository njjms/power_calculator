#
# Single Size Calculator server components
#
# Initiate reactive values
single_sample_size_data <- reactiveValues(single_sample_size_data =
                                          data.frame(
                                           	obs = numeric(),
                                           	point= numeric(),
                                           	prob= numeric(),
                                           	lower_bound = numeric(),
                                           	upper_bound = numeric(),
                                           	pass = logical()
                                          ),
                                          power = numeric(),
                                          max_misses = numeric())

true_probability <- reactiveValues(true_probability = .90)
sample_size3 <- reactiveValues(sample_size3 = 100)
requirement3 <- reactiveValues(requirement3 = .95)
direction3 <- reactiveValues(direction3 = NA)
test3 <- reactiveValues(test3 = "ws")
AC_type3 <- reactiveValues(AC_type3 = "medium")
prq_delta3 <- reactiveValues(prq_delta3 = 0.94)
alpha3 <- reactiveValues(alpha3 = 0.05)

# Calculate
observeEvent(input$run_calculation3, {
    direction3$direction3 <- if_else(input$direction3 == 1, "gt", "lt")
    requirement3$requirement3 <- input$requirement3
    alpha3$alpha3 <- input$alpha3
    test3$test3 <- case_when(input$test3 == 1 ~ "ws", input$test3 == 2 ~ "cp")
    sample_size3$sample_size3 <- input$sample_size3
    true_probability$true_probability <- input$true_probability
    AC_type3$AC_type3 <- case_when(input$AC_type3 == 1 ~ "low",
                                   input$AC_type3 == 2 ~ "medium",
                                   input$AC_type3 == 3 ~ "high",
                                   input$AC_type3 == 4 ~ "high_delta")
    prq_delta3$prq_delta3 <- input$prq_delta3
    
    single_sample_size_df <- power_calc(sample_size = sample_size3$sample_size3,
                                        true_prob = true_probability$true_probability,
                                        requirement = requirement3$requirement3,
                                        alpha = alpha3$alpha3,
                                        requirement_type = direction3$direction3,
                                        interval_type = test3$test3,
                                        AC_type = AC_type3$AC_type3,
                                        prq_delta = prq_delta3$prq_delta3)
    single_sample_size_data$single_sample_size_data <- single_sample_size_df$df
    single_sample_size_data$single_sample_size_data$cumulative_probability <- cumsum(single_sample_size_df$df$prob)
    single_sample_size_data$power <- single_sample_size_df$power
    single_sample_size_data$max_misses <- nrow(single_sample_size_df$df[single_sample_size_df$df$pass == 1,]) - 1
 })

output$single_sample_size_power <- renderText(
  if (nrow(single_sample_size_data$single_sample_size_data) == 0) {
    "Power: N/A"
  } else {
    paste0("Probability of Failing AC: ", round(single_sample_size_data$power*100, 5), "%")
  }
)

output$single_sample_size_max_misses <- renderText(
  if (nrow(single_sample_size_data$single_sample_size_data) == 0) {
    "Maximum Incorrect Samples Before Failure: N/A"
  } else if (single_sample_size_data$max_misses == -1) {
    paste0("Maximum Incorrect Samples Before Failure: Cannot pass AC at this sample size.")
  } else {
    paste0("Maximum Incorrect Samples Before Failure: ", single_sample_size_data$max_misses)
  }
)

# Render plot
output$curvePlot3 <- renderPlotly({
  validate(
    need(input$sample_size3 > 0,
         'Sample size must be greater than 0.'),
    need(dplyr::between(input$alpha3, 0, 1),
         'Significance level must be between 0 and 1')
  )
  if (nrow(single_sample_size_data$single_sample_size_data) == 0) {
    p <- ggplot() +
      scale_x_continuous("Sample Size",
                         limits = c(0, 100)) +
      scale_y_continuous("Confidence Limits",
                         limits = c(0, 1)) +
      theme(
        legend.position = "none",
        panel.grid.minor.x = element_blank()
      )
    ggplotly(p)
  } else {
    p <- ggplot(single_sample_size_data$single_sample_size_data,
                mapping = aes(text = paste("Point Estimate:", obs, "/", sample_size3$sample_size3,
                                           "(", point, ")",
                                           "<br>UCL:", round(upper_bound, 5),
                                           "<br>LCL:", round(lower_bound, 5),
                                           "<br>CDF(x):", round(cumulative_probability*100, 5), "%"))
                ) +
      geom_point(mapping = aes(x = obs, y = point),
                 size = .7) +
      geom_segment(mapping = aes(x = obs, xend = obs, 
                                 y = lower_bound, yend = upper_bound),
                   alpha = .3) +
      geom_hline(yintercept = requirement3$requirement3,
                 linetype = "dotted",
                 size = .3,
                 alpha = .9,
                 color = "#FF6600") +
      labs(x = "Number of Correct Calls",
           y = "Point Estimate and Confidence Limits") +
	    theme_bw()
      if (AC_type3$AC_type3 == "high_delta") {
        p <- p + geom_hline(yintercept = prq_delta3$prq_delta3,
                            linetype = "dotted",
                            size = .3,
                            alpha = .4,
                            color = "#FF6600")
      }
    ggplotly(p, tooltip="text") %>%
      layout(
        legend = list(
          title = list(text="PE and CLs",side="left"),
          orientation = "h",
          x=0.5,
          xanchor="center",
          y = -.2
        )
      )
  }
})

# Render datatable
output$single_sample_size_data <- DT::renderDataTable({
    validate(
      need(dplyr::between(requirement3$requirement3, 0, 1),
           'Requirement must be between 0 and 1'),
      need(is.numeric(sample_size3$sample_size3),
           'Sample size must be numeric.')
    )
    if (nrow(single_sample_size_data$single_sample_size_data) == 0) {
      data.frame(
      	obs = numeric(),
      	point= numeric(),
      	prob= numeric(),
      	lower_bound = numeric(),
      	upper_bound = numeric(),
      	pass = logical()
      )
    } else {
      single_sample_size_data$single_sample_size_data
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