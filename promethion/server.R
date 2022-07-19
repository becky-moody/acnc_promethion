library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  ## Has a bunch of functions that will be needed
  source(here::here('scripts/clean_promethion_file.R'))




  # # text outputs
  # output$meta_note <- renderText('Note: This file must be formatted prior to uploading.')


  # # get start/stop time ----
  # start_light <- hms::as_hms('7:15:00') # inclusive
  # end_light <- hms::as_hms('19:00:00') # make sure the user put the hour that the light stopped aka when did dark start
  light_colors <- c(light = "#FFF68F", dark = "#8DEEEE")

  # get promethion data paths ----

  meta_df <- data.frame()
  final_df <- data.frame()

  # read meta data file ----
  observeEvent(input$animal_file,{
    if(is.null(input$animal_file)){
      meta_df <<- NULL
      return(NULL)
    }
    req(input$animal_file)
    meta_df <<- read.input.file(input$animal_file$datapath) %>%
      janitor::clean_names()

    cat('\n meta data file \n')
    print(head(meta_df, 2))
  })
#
  # observe({
  #   shinyjs::toggleElement("st_lg", condition = input$files_upload>0)
    #shinyjs::toggle("end_lg", condition = input$upload_files>0)
    #shinyjs::toggle("ph", condition = input$upload_files>0)
  # })

  observeEvent(input$upload_files_btn, {
    shinyjs::show("st_lg")
        #shinyjs::toggle("end_lg")
        #shinyjs::toggle("ph")
  })

  observe({
    shinyjs::toggle("dl", condition = input$calc_phases>0)
  })


  observeEvent(input$files_upload,ignoreInit = TRUE,{
    print('\n running promethion \n')
    if(is.null(input$prom_file)){
      return(NULL)
    }
    prom_file_paths <- list()
    for(i in 1:nrow(input$prom_file)){
      prom_file_paths[i] <- input$prom_file$datapath[i]
    }

    cat('\n promethion file paths \n')
    print(prom_file_paths)

    req(prom_file_paths)
    req(meta_df)

    prom_df <- join.cage.files(prom_file_paths)

    df <- left_join(prom_df, meta_df, by = c('cage_num', 'run')) %>%
      ## remove anything where there is not an animal_id
      drop_na(subject_id)
    final_df <<- df
    print(head(final_df,2))

  })

  df_phases <- eventReactive(input$calculate_phases,{
    cat('\n adding in phases \n')
    df <- add.phases(df, input$start_light, input$end_light)
    final_df <<- df

  })

  # agg_df <- eventReactive(!is_empty(final_df),{
  #   req(input$agg_data)
  #
  #   df <- final_df
  #   df <- df %>% group_by()
  #
  # })


  ## download as csv
  output$download_data <- downloadHandler(
    filename = function() {
      paste("promethion_cleaned",Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df_phases(), file, row.names = FALSE, col.names =TRUE)
    }
  )

})
