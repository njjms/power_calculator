#
# Effect size calculator server components
#
# Initiate the reactive objects
#
curve_plot_data2 <- reactiveValues(curve_plot_data2 =
                                    data.frame(
                                      requirement_type= character(),
                                      requirement = numeric(),
                                      sample_size = numeric(),
                                      true_prob = numeric(),
                                      PrFailAC = numeric(),
                                      test = character(),
                                      alpha_2_sided = character(),
                                      AC_type = character()
                                    ))

selected_sample_sizes <- reactiveValues(sizes = 100)
true_probs <- reactiveValues(true_probs = vector(mode = "numeric"))
requirement2 <- reactiveValues(requirement2 = .95)
requirement_type2 <- reactiveValues(requirement_type2 = NA)
test_type2 <- reactiveValues(test_type2 = "ws")
alpha2 <- reactiveValues(alpha2 = .05)
AC_type2 <- reactiveValues(AC_type2 = "medium")
prq_delta2 <- reactiveValues(prq_delta2 = 0.94)
ACtext2 <- reactiveValues(title = "")

# Run the calculation
observeEvent(
  input$run_calculation2, {
    requirement_type2$requirement_type2 <- if_else(input$direction2 == 1, "gt", "lt")
    requirement2$requirement2 <- input$requirement2
    alpha2$alpha2 <- input$alpha2
    prq_delta2$prq_delta2 <- input$prq_delta2
    test_type2$test_type2 <- case_when(input$test2 == 1 ~ "ws", input$test2 == 2 ~ "cp")
    AC_type2$AC_type2 <- case_when(input$AC_type2 == 1 ~ "low",
                                   input$AC_type2 == 2 ~ "medium",
                                   input$AC_type2 == 3 ~ "high",
                                   input$AC_type2 == 4 ~ "high_delta")
    ACtext2$title <- case_when(input$AC_type2 == 1 ~  paste("AC:", ifelse(input$direction2 == 1, "UCL \u2265", "LCL \u2264"),input$requirement2),
                               input$AC_type2 == 2 ~ paste("AC: PE", ifelse(input$direction2 == 1, "\u2265", "\u2264"),input$requirement2),
                               input$AC_type2 == 3 ~ paste("AC:", ifelse(input$direction2 == 1, "LCL \u2265", "UCL \u2264"),input$requirement2),
                               input$AC_type2 == 4 ~ paste("AC:", ifelse(input$direction2 == 1, "LCL \u2265", "UCL \u2264"),input$prq_delta2,
                                                           "and PE",ifelse(input$direction2 == 1, "\u2265", "\u2264"),input$requirement2))
    ACtext2$subtitle=paste0(", ", 
                            ifelse(test_type2$test_type2=="ws", 
                                   "Wilson-Score", "Clopper-Pearson"),
                           " Interval, 2-sided \u03B1=", alpha2$alpha2)
    selected_sample_sizes$sizes <- as.numeric(trimws(unlist(strsplit(input$sample_sizes2,","))))
    selected_sample_sizes$sizes <- selected_sample_sizes$sizes[!is.na(selected_sample_sizes$sizes)]

    if (requirement_type2$requirement_type2 == "gt") {
       true_probs$true_probs <- requirement2$requirement2 - seq(-input$effect_size_neighborhood,
                                                                input$effect_size_neighborhood,
                                                                by = input$effect_size_step)
       true_probs$true_probs <- true_probs$true_probs[(true_probs$true_probs >= 0 &
                                                         (true_probs$true_probs <= 1))]
    } else {
       true_probs$true_probs <- requirement2$requirement2 + seq(-input$effect_size_neighborhood,
                                                                input$effect_size_neighborhood,
                                                                by = input$effect_size_step)
       true_probs$true_probs <- true_probs$true_probs[(true_probs$true_probs >= 0 &
                                                         (true_probs$true_probs <= 1))]
    }
    add_curve_plot_data2 <- list()

    withProgress(message="Calculating Power",value=0,{
      for (sample_size in selected_sample_sizes$sizes) {
        incProgress(1/length(selected_sample_sizes$sizes))
        pow <- sapply(true_probs$true_probs,
                        FUN = function(x) {
                          power_calc(sample_size = sample_size,
                                     true_prob = x,
                                     requirement = requirement2$requirement2,
                                     alpha = alpha2$alpha2,
                                     requirement_type = requirement_type2$requirement_type2,
                                     interval_type = test_type2$test_type2,
                                     AC_type = AC_type2$AC_type2,
                                     prq_delta = prq_delta2$prq_delta2)$power
                      })
        tmp_power_df <- data.frame(
              requirement_type = rep(requirement_type2$requirement_type2,
                                     length(true_probs$true_probs)),
              requirement = rep(requirement2$requirement2,
                                length(true_probs$true_probs)),
              sample_size = rep(sample_size, length(true_probs$true_probs)),
              true_prob = true_probs$true_probs,
              PrFailAC = pow,
              test = rep(ifelse(test_type2$test_type2=="ws", "Wilson-Score", "Clopper-Pearson"),
                         length(true_probs$true_probs)),
              alpha_2_sided = rep(alpha2$alpha2, length(true_probs$true_probs)),
              AC_type = rep(AC_type2$AC_type2,length(true_probs$true_probs))
        )
        add_curve_plot_data2$curve_plot_data2[[length(add_curve_plot_data2$curve_plot_data2) + 1]] <- tmp_power_df
      }
    })
  curve_plot_data2$curve_plot_data2 <- dplyr::bind_rows(add_curve_plot_data2$curve_plot_data2)
},
ignoreNULL = TRUE,
ignoreInit = TRUE)

# Render plot
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
      scale_y_continuous(ifelse(input$FlipY2, "Pr(Pass AC)", "Pr(Fail AC)"),
                         limits = c(0, 1)) +
      theme(
        legend.position = "none",
        panel.grid.minor.x = element_blank()
      )
    ggplotly(p)
  }else{
    p<- ggplot(curve_plot_data2$curve_plot_data2,
               mapping = aes(x = true_prob,
                             y = if (input$FlipY2) {1 - PrFailAC} else {PrFailAC},
                             color = as.factor(sample_size),
                             text = paste("True Prob:", true_prob,
                                          ifelse(input$FlipY2, "<br>Pr(Pass AC):", "<br>Pr(Fail AC):"),
                                          if(input$FlipY2) {round(1 - PrFailAC, 4)} else {round(PrFailAC, 4)},
                                          "<br>Sample Size:", sample_size))) +
    	    geom_line(group = 1) +
          geom_vline(xintercept = requirement2$requirement2, 
                     linetype = "dotted",
                     size = .3,
                     alpha = .7) +
          labs(y = ifelse(input$FlipY2,"Pr(Pass AC)","Pr(Fail AC)"),
               x = "True Probability",
               title = if(input$VerboseTitle2){
                 paste0(ACtext2$title,ACtext2$subtitle)}else{
                 ACtext2$title}) +
    	    theme_bw()
    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(
          title = list(text="Sample Size",side="left"),
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
    if (nrow(curve_plot_data2$curve_plot_data2) == 0) {
      data.frame(
        requirement_type= character(),
        requirement = numeric(),
        sample_size = numeric(),
        true_prob = numeric(),
        PrFailAC = numeric(),
        test = character(),
        alpha_2_sided = character(),
        AC_type = character())
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
