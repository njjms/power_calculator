#
# Server code for sample size calculator 
#
# Initiate reactive values for calculator
#
sample_size_blank_df <- data.frame(sample_size = numeric(),
                                   selected_prob = numeric(),
                                   prob_fail_AC = numeric(),
                                   prob_pass_AC = numeric())
curve_plot_data <- reactiveValues(df = sample_size_blank_df)
                                    
requirement_type <- reactive({if_else(input$direction == 1, "gt", "lt")})

test_type <- reactive({case_when(input$test == 1 ~ "ws",
                                 input$test == 2 ~ "cp")})

AC_type_full <- reactive({case_when(input$AC_type == 1 ~ "low",
                                    input$AC_type == 2 ~ "medium",
                                    input$AC_type == 3 ~ "high",
                                    input$AC_type == 4 ~ "high_delta")})

plot_title <- eventReactive(input$run_calculation, {
  title = case_when(input$AC_type == 1 ~ paste("AC:", 
                                               ifelse(input$direction == 1,
                                                      "UCL \u2265",
                                                      "LCL \u2264"),
                                               input$requirement),
                     input$AC_type == 2 ~ paste("AC: PE", 
                                                ifelse(input$direction == 1,
                                                       "\u2265",
                                                       "\u2264"),
                                                input$requirement),
                     input$AC_type == 3 ~ paste("AC:",
                                                ifelse(input$direction == 1,
                                                       "LCL \u2265",
                                                       "UCL \u2264"),
                                                input$requirement),
                     input$AC_type == 4 ~ paste("AC:",
                                                ifelse(input$direction == 1,
                                                       "LCL \u2265",
                                                       "UCL \u2264"),
                                                input$prq_delta,
                                                "and PE", 
                                                ifelse(input$direction == 1,
                                                       "\u2265",
                                                       "\u2264"),
                                                input$requirement))
  title
})

plot_subtitle <- eventReactive(input$run_calculation, {
  subtitle = paste0(", ", 
                    ifelse(input$test == 1, 
                           "Wilson-Score",
                           "Clopper-Pearson"),
                    " Interval, 2-sided \u03B1=",
                    input$alpha)
  subtitle
})

selected_effect_sizes <- reactive({
  tmp <- as.numeric(trimws(unlist(strsplit(input$true_p, ","))))
  tmp <- tmp[!is.na(tmp)]
  tmp
})
   
sample_size_values <- reactive({
  req(input$n_minimum, input$n_maximum, input$step)
  s <- seq(input$n_minimum, input$n_maximum, by = input$step)
  s
}) 

# logic to run calculation
observeEvent(
  input$run_calculation, {
    add_curve_plot_data <- list()

    withProgress(message = "Calculating Power", value = 0, {
      for (p in selected_effect_sizes()) {
        incProgress(1/length(selected_effect_sizes()))
        pow <- sapply(sample_size_values(),
                      FUN = function(x) {
                        power_calc(sample_size = x,
                                   true_prob = p,
                                   requirement = input$requirement,
                                   alpha = input$alpha,
                                   requirement_type = requirement_type(),
                                   interval_type = test_type(),
                                   AC_type = AC_type_full(),
                                   prq_delta = input$prq_delta)$power
                        }
                      )
        tmp_power_df <- data.frame(
          sample_size = sample_size_values(),
          selected_prob = rep(p, length(sample_size_values())),
          prob_fail_AC = pow,
          prob_pass_AC = 1-pow
        )
        add_curve_plot_data[[length(add_curve_plot_data) + 1]] <- tmp_power_df
      }
    })
    curve_plot_data$df <- dplyr::bind_rows(add_curve_plot_data)
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
    need(all(dplyr::between(selected_effect_sizes(), 0, 1)),
        'True probabilities must be between 0 and 1')
  )
  if (nrow(curve_plot_data$df) == 0) {
    p <- ggplot() +
      scale_x_continuous("Sample Size",
                         limits = c(input$n_minimum,
                                    input$n_maximum)) +
      scale_y_continuous(ifelse(input$FlipY,
                                "Pr(Pass AC)",
                                "Pr(Fail AC)"),
                         limits = c(0, 1)) +
      theme(
        legend.position = "none",
        panel.grid.minor.x = element_blank()
      )
    ggplotly(p)
  } else {
    p<- ggplot(curve_plot_data$df,
               mapping = aes(x = sample_size,
                             y = if (input$FlipY) {
                                   prob_pass_AC
                                 } else {
                                   prob_fail_AC
                                 },
                             color = as.factor(selected_prob),
                             text = paste(
                               "Sample Size:", sample_size,
                               ifelse(input$FlipY,
                                      "<br>Pr(Pass AC):",
                                      "<br>Pr(Fail AC):"),
                               if(input$FlipY) {
                                 round(prob_pass_AC, 4)
                               } else {
                                 round(prob_fail_AC, 4)
                               },
                               "<br>True Prob:",
                               selected_prob
                             ))) +
    	    geom_line(group = 1) +
          labs(y = ifelse(input$FlipY,
                          "Pr(Pass AC)",
                          "Pr(Fail AC)"),
               x = "Sample Size",
               title = if(input$VerboseTitle) {
                         paste0(plot_title(), 
                                plot_subtitle())
                       } else {
                         plot_title()
                       }) +
    	    theme_bw()
    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(
          title = list(text = "True Probability",
                       side = "left"),
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -.2
        )
      )
  }
})

output$curvePlotData <- DT::renderDataTable({
  if (nrow(curve_plot_data$df) == 0) {
    sample_size_blank_df 
  } else {
    curve_plot_data$df
  }},
  extensions = c("Buttons", "Scroller"),
  options = list(
    dom = 'Bfrtip',
    deferRender = TRUE,
    scrollY = 400,
    scroller = TRUE,
    buttons = c('copy', 'csv', 'excel')
  )
)