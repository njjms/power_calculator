# Server logic for power calculator app
# This shiny dashboard is split into 3 apps: power when sample size is varied,
# power when effect size is varied, and power when sample size and effect size is specified
#

server <- function(input, output, session) {

  #
  # Sample size calculator server components
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

  #
  # Effect size calculator components
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
  
  #
  # Single Size Calculator Components
  #
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
}