server <- function(input, output, session) {
  
  # source server functions for each tab
  source("server_files/sample_size_calculator_server.R", local = TRUE)
  source("server_files/effect_size_calculator_server.R", local = TRUE)
  source("server_files/single_size_calculator_server.R", local = TRUE)
  
}