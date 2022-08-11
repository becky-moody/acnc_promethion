library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  ## Has a bunch of functions that will be needed
  #source(here::here('scripts/clean_promethion_file.R'))

  # initialize variables ----
  final_df <- reactiveVal()
  plot_df <- reactiveVal()


  source(here::here('tabs/data_editor_server.R'), local = TRUE)

  source(here::here('tabs/plotting_server.R'), local = TRUE)




})
