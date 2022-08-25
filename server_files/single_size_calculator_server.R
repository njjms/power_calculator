#
# Single Size Calculator server components
#
# Initiate reactive values
blank_df <- data.frame(n_successes = numeric(),
 	                     point_estimate = numeric(),
 	                     binom_prob = numeric(),
 	                     lower_bound = numeric(),
 	                     upper_bound = numeric(),
 	                     pass = logical())

direction3 <- reactive({if_else(input$direction3 == 1, "gt", "lt")})
test3 <- reactive({case_when(input$test3 == 1 ~ "ws",
                             input$test3 == 2 ~ "cp")})
AC_type3 <- reactive({case_when(input$AC_type3 == 1 ~ "low",
                                input$AC_type3 == 2 ~ "medium",
                                input$AC_type3 == 3 ~ "high",
                                input$AC_type3 == 4 ~ "high_delta")})

# Calculate
single_sample_size_data <- reactive({
    tmp <- power_calc(sample_size = input$sample_size3,
                      true_prob = input$true_probability,
                      requirement = input$requirement3,
                      alpha = input$alpha3,
                      requirement_type = direction3(),
                      interval_type = test3(),
                      AC_type = AC_type3(),
                      prq_delta = input$prq_delta3)
    tmp$df$cumulative_probability <- cumsum(tmp$df$prob)
    tmp$df$n_successes <- tmp$df$obs
    tmp$df$point_estimate <- tmp$df$point
    tmp$df$binom_prob <- tmp$df$prob
    
    tmp[["max_misses"]] <- nrow(tmp$df[tmp$df$pass == 1,]) - 1
    
    # return reactive
    tmp
})

output$single_sample_size_power <- renderText(
  if (nrow(single_sample_size_data()$df) == 0) {
    "Power: N/A"
  } else {
    paste0("Probability of Failing AC: ", round(single_sample_size_data()$power*100, 5), "%")
  }
)

output$single_sample_size_max_misses <- renderText(
  if (nrow(single_sample_size_data()$df) == 0) {
    "Maximum Incorrect Samples Before Failure: N/A"
  } else if (single_sample_size_data()$max_misses == -1) {
    paste0("Maximum Incorrect Samples Before Failure: Cannot pass AC at this sample size.")
  } else {
    paste0("Maximum Incorrect Samples Before Failure: ", single_sample_size_data()$max_misses)
  }
)

# Render plot
output$curvePlot3 <- renderPlotly({
  validate(need(input$sample_size3 > 0,
                'Sample size must be greater than 0.'),
           need(dplyr::between(input$alpha3, 0, 1),
                'Significance level must be between 0 and 1'))
  
  if (nrow(single_sample_size_data()$df) == 0) {
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
    p <- ggplot(single_sample_size_data()$df,
                mapping = aes(
                  text = paste("Point Estimate:", n_successes,
                               "/", input$sample_size3,
                               "(", point, ")",
                               "<br>UCL:", round(upper_bound, 5),
                               "<br>LCL:", round(lower_bound, 5),
                               "<br>CDF(x):", round(cumulative_probability*100, 5), "%"))) +
      geom_point(mapping = aes(x = n_successes,
                               y = point_estimate),
                 size = .7) +
      geom_segment(mapping = aes(x = n_successes, xend = n_successes, 
                                 y = lower_bound, yend = upper_bound),
                   alpha = .3) +
      geom_hline(yintercept = input$requirement3,
                 linetype = "dotted",
                 size = .3,
                 alpha = .9,
                 color = "#FF6600") +
      labs(x = "Number of Correct Calls",
           y = "Point Estimate and Confidence Limits") +
	    theme_bw()
      if (AC_type3() == "high_delta") {
        p <- p + geom_hline(yintercept = input$prq_delta3,
                            linetype = "dotted",
                            size = .3,
                            alpha = .4,
                            color = "#FF6600")
      }
    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(
          title = list(text = "PE and CLs",
                       side = "left"),
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -.2)
      )
  }
})

# Render datatable
output$single_sample_size_dt <- DT::renderDataTable({
    validate(
      need(dplyr::between(input$requirement3, 0, 1),
           'Requirement must be between 0 and 1'),
      need(is.numeric(input$sample_size3),
           'Sample size must be numeric.')
    )
    if (nrow(single_sample_size_data()$df) == 0) {
      blank_df
    } else {
      single_sample_size_data()$df
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