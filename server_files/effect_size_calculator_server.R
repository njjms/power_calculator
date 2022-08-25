#
# Effect size calculator server components
#
# Initiate the reactive objects
#
effect_size_blank_df <- data.frame(sample_size = numeric(),
                                   true_prob = numeric(),
                                   prob_fail_AC = numeric(),
                                   prob_pass_AC = numeric())
curve_plot_data2 <- reactiveValues(df = effect_size_blank_df)

requirement_type2 <- reactive({if_else(input$direction2 == 1, "gt", "lt")})

test_type2 <- reactive({case_when(input$test2 == 1 ~ "ws",
                                  input$test2 == 2 ~ "cp")})

AC_type2_full <- reactive({case_when(input$AC_type2 == 1 ~ "low",
                                     input$AC_type2 == 2 ~ "medium",
                                     input$AC_type2 == 3 ~ "high",
                                     input$AC_type2 == 4 ~ "high_delta")})

plot_title2 <- eventReactive(input$run_calculation2, {
  title = case_when(input$AC_type2 == 1 ~ paste("AC:", 
                                                 ifelse(input$direction2 == 1,
                                                        "UCL \u2265",
                                                        "LCL \u2264"),
                                                 input$requirement2),
                     input$AC_type2 == 2 ~ paste("AC: PE", 
                                                 ifelse(input$direction2 == 1,
                                                        "\u2265",
                                                        "\u2264"),
                                                 input$requirement2),
                     input$AC_type2 == 3 ~ paste("AC:",
                                                 ifelse(input$direction2 == 1,
                                                        "LCL \u2265",
                                                        "UCL \u2264"),
                                                 input$requirement2),
                     input$AC_type2 == 4 ~ paste("AC:",
                                                 ifelse(input$direction2 == 1,
                                                        "LCL \u2265",
                                                        "UCL \u2264"),
                                                 input$prq_delta2,
                                                 "and PE", 
                                                 ifelse(input$direction2 == 1,
                                                        "\u2265",
                                                        "\u2264"),
                                                 input$requirement2))
  title
})

plot_subtitle2 <- eventReactive(input$run_calculation2, {
  subtitle = paste0(", ", 
                     ifelse(input$test2 == 1, 
                            "Wilson-Score",
                            "Clopper-Pearson"),
                    " Interval, 2-sided \u03B1=",
                    input$alpha2)
  subtitle
})

selected_sample_sizes <- reactive({
  # Parse text for sample sizes, remove any non-numerics
  tmp <- as.numeric(trimws(unlist(strsplit(input$sample_sizes2, ","))))
  tmp <- tmp[!is.na(tmp)]
  tmp
})

effect_size_values <- reactive({
     tmp <- input$requirement2 + seq(input$effect_size_range[1],
                                     input$effect_size_range[2],
                                     length.out = 1000)
     tmp <- tmp[(tmp >= 0 & (tmp <= 1))]
     tmp
})

# Run the calculation
observeEvent(input$run_calculation2, {
    add_curve_plot_data2 <- list()
    
    withProgress(message = "Calculating Power", value = 0, {
      for (s in selected_sample_sizes()) {
        incProgress(1/length(selected_sample_sizes()))
        pow <- sapply(effect_size_values(),
                      FUN = function(x) {
                        power_calc(sample_size = s,
                                   true_prob = x,
                                   requirement = input$requirement2,
                                   alpha = input$alpha2,
                                   requirement_type = requirement_type2(),
                                   interval_type = test_type2(),
                                   AC_type = AC_type2_full(),
                                   prq_delta = input$prq_delta2)$power
                      })
        tmp_power_df <- data.frame(
              sample_size = rep(s, length(effect_size_values())),
              true_prob = effect_size_values(),
              prob_fail_AC = pow,
              prob_pass_AC = 1 - pow
        )
        add_curve_plot_data2[[length(add_curve_plot_data2) + 1]] <- tmp_power_df
      }
    })
  curve_plot_data2$df <- dplyr::bind_rows(add_curve_plot_data2)
  },
  ignoreNULL = TRUE,
  ignoreInit = TRUE
)

# Render plot
output$curvePlot2 <- renderPlotly({
  validate(
    need(dplyr::between(input$requirement2, 0, 1),
         'Requirement must be between 0 and 1'),
    need(is.numeric(selected_sample_sizes()),
         'Sample sizes must be numeric.')
  )
  if (nrow(curve_plot_data2$df) == 0) {
    p <- ggplot() +
      scale_x_continuous("Effect Size",
                         limits = c(0, 1)) +
      scale_y_continuous(ifelse(input$FlipY2, "Pr(Pass AC)", "Pr(Fail AC)"),
                         limits = c(0, 1)) +
      theme(
        legend.position = "none",
        panel.grid.minor.x = element_blank()
      )
    ggplotly(p)
  } else {
    p <- ggplot(curve_plot_data2$df,
                mapping = aes(x = true_prob,
                              y = if (input$FlipY2) {
                                    prob_pass_AC
                                  } else {
                                    prob_fail_AC
                                  },
                              color = as.factor(sample_size),
                              text = paste("True Prob:", 
                                           round(true_prob, 5)*100, "%",
                                           ifelse(input$FlipY2, 
                                                  "<br>Pr(Pass AC):",
                                                  "<br>Pr(Fail AC):"),
                                           if (input$FlipY2) {
                                             round(prob_pass_AC, 4) 
                                           } else {
                                             round(prob_fail_AC, 4)
                                           },
                                           "<br>Sample Size:",
                                           sample_size))) +
    	    geom_line(group = 1) +
          geom_vline(xintercept = input$requirement2, 
                     linetype = "dotted",
                     size = .3,
                     alpha = .7) +
          labs(y = ifelse(input$FlipY2, "Pr(Pass AC)", "Pr(Fail AC)"),
               x = "True Probability",
               title = if (input$VerboseTitle2) {
                         paste0(plot_title2(),
                                plot_subtitle2())
                       } else {
                         plot_title2()
                       }) +
    	    theme_bw()
    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(
          title = list(text = "Sample Size",
                       side = "left"),
          orientation = "h",
          x=0.5,
          xanchor="center",
          y = -.2
        )
      )
  }
})

# Render datatable
output$curvePlotData2 <- DT::renderDataTable({
    if (nrow(curve_plot_data2$df) == 0) {
      effect_size_blank_df
    } else {
      curve_plot_data2$df
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
