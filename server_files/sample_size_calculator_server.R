#
# Server code for sample size calculator 
#
# Initiate reactive values for calculator
#
curve_plot_data <- reactiveValues(curve_plot_data =
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

selected_true_ps <- reactiveValues(true_ps = 0.95)
requirement <- reactiveValues(requirement = NA)
requirement_type <- reactiveValues(requirement_type = NA)
test_type <- reactiveValues(test_type = "ws")
AC_type <- reactiveValues(AC_type = "medium")
prq_delta <- reactiveValues(prq_delta = 0.94)
alpha1 <- reactiveValues(alpha1 = .05)
ACtext <- reactiveValues(title = "")
ACtext <- reactiveValues(subtitle = "")

# logic to run calculation
observeEvent(
  input$run_calculation, {
    requirement_type$requirement_type <- if_else(input$direction == 1, "gt", "lt")
    requirement$requirement <- input$requirement
    prq_delta$prq_delta <- input$prq_delta
    alpha1$alpha1 <- input$alpha
    test_type$test_type <- case_when(input$test == 1 ~ "ws", input$test == 2 ~ "cp")
    AC_type$AC_type <- case_when(input$AC_type == 1 ~ "low",
                                 input$AC_type == 2 ~ "medium",
                                 input$AC_type == 3 ~ "high",
                                 input$AC_type == 4 ~ "high_delta")
    ACtext$title <- case_when(input$AC_type == 1 ~  paste("AC:",ifelse(input$direction == 1, "UCL \u2265", "LCL \u2264"),input$requirement),
                              input$AC_type == 2 ~ paste("AC: PE",ifelse(input$direction == 1, "\u2265", "\u2264"),input$requirement),
                              input$AC_type == 3 ~ paste("AC:",ifelse(input$direction == 1, "LCL \u2265", "UCL \u2264"),input$requirement),
                              input$AC_type == 4 ~ paste("AC:",ifelse(input$direction == 1, "LCL \u2265", "UCL \u2264"),input$prq_delta,
                                                         "and PE",ifelse(input$direction == 1, "\u2265", "\u2264"),input$requirement))
    ACtext$subtitle=paste0(", ",
                           ifelse(test_type$test_type=="ws",
                                  "Wilson-Score","Clopper-Pearson"),
                           " Interval, 2-sided \u03B1=", alpha1$alpha1)

    selected_true_ps$true_ps <- as.numeric(trimws(unlist(strsplit(input$true_p, ","))))
    selected_true_ps$true_ps <- selected_true_ps$true_ps[!is.na(selected_true_ps$true_ps)]
    sample_sizes <- seq(input$n_minimum, input$n_maximum, by=input$step)
    add_curve_plot_data <- list()

    withProgress(message="Calculating Power", value=0,{
      for(truep in selected_true_ps$true_ps){
        incProgress(1/length(selected_true_ps$true_ps))
        pow <- sapply(sample_sizes,
                      FUN = function(x) {
                        pw = power_calc(sample_size = x,
                                   true_prob = truep,
                                   requirement = requirement$requirement,
                                   alpha = alpha1$alpha1,
                                   requirement_type = requirement_type$requirement_type,
                                   interval_type = test_type$test_type,
                                   AC_type = AC_type$AC_type,
                                   prq_delta = prq_delta$prq_delta)$power
                        return(pw)
                      })
        tmp_power_df <- data.frame(
          requirement_type = rep(requirement_type$requirement_type, length(sample_sizes)),
          requirement = rep(requirement$requirement, length(sample_sizes)),
          sample_size = sample_sizes,
          true_prob = rep(truep,length(sample_sizes)),
          PrFailAC = pow,
          test = rep(ifelse(test_type$test_type=="ws", "Wilson-Score", "Clopper-Pearson"),
                     length(sample_sizes)),
          alpha_2_sided = rep(alpha1$alpha1, length(sample_sizes)),
          AC_type = rep(AC_type$AC_type, length(sample_sizes))
        )
        add_curve_plot_data$curve_plot_data[[length(add_curve_plot_data$curve_plot_data) + 1]] <- tmp_power_df
      }
    })

    curve_plot_data$curve_plot_data <- dplyr::bind_rows(add_curve_plot_data$curve_plot_data)
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
    need(all(dplyr::between(selected_true_ps$true_ps, 0, 1)),
        'True probabilities must be between 0 and 1')
  )
  if (nrow(curve_plot_data$curve_plot_data) == 0) {
    p <- ggplot() +
      scale_x_continuous("Sample Size",
                         limits = c(input$n_minimum,
                                    input$n_maximum)) +
      scale_y_continuous(ifelse(input$FlipY, "Pr(Pass AC)", "Pr(Fail AC)"),
                         limits = c(0, 1)) +
      theme(
        legend.position = "none",
        panel.grid.minor.x = element_blank()
      )
    ggplotly(p)
  } else {
    p<- ggplot(curve_plot_data$curve_plot_data,
               mapping = aes(x = sample_size,
                             y = if(input$FlipY){1-PrFailAC}else{PrFailAC},
                             color = as.factor(true_prob),
                             text = paste(
                               "Sample Size:", sample_size,
                               ifelse(input$FlipY, "<br>Pr(Pass AC):", "<br>Pr(Fail AC):"),
                               if(input$FlipY){round(1-PrFailAC,4)}else{round(PrFailAC,4)},
                               "<br>True Prob:", true_prob)
               )) +
    	    geom_line(group = 1) +
          labs(y = ifelse(input$FlipY, "Pr(Pass AC)", "Pr(Fail AC)"),
               x = "Sample Size",
               title = if(input$VerboseTitle) {
                 paste0(ACtext$title, ACtext$subtitle)} else {
                 ACtext$title}) +
    	    theme_bw()
    ggplotly(p, tooltip="text") %>%
      layout(
        legend = list(
          title = list(text="True Probability",side="left"),
          orientation = "h",
          x=0.5,
          xanchor="center",
          y = -.2
        )
      )
  }
})
output$curvePlotData <- DT::renderDataTable({
  if (nrow(curve_plot_data$curve_plot_data) == 0) {
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