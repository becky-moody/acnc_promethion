library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  ## Has a bunch of functions that will be needed
  #source(here::here('scripts/clean_promethion_file.R'))

  source(here::here('tabs/data_editor_server.R'), local = TRUE)

  ## only show anything if you click on this tab
  observeEvent(input$promethion_app == 'Plot Data',{

    ## hide/show sidebar items
    if(is.null(final_df())){

      message('Need data uploaded.')
      shinyjs::show(id='no_data_warning')

      shinyjs::hide(id='filter_tab_name')
      shinyjs::hide(id="which_column_subject_id")

      shinyjs::hide(id='all_plot_filters')
      ## grouped these
      # shinyjs::hide(id="plot_metric_filter")
      # shinyjs::hide(id='plot_animal_filter')
      # shinyjs::hide(id="plot_phase_filter")

      shinyjs::hide(id="plot_selected")

    } else{

      shinyjs::hide(id='no_data_warning')

      shinyjs::show(id='filter_tab_name')
      shinyjs::show(id="which_column_subject_id")
      shinyjs::show(id='all_plot_filters')
    }

    req(!is.null(final_df()))
    final_df_cols <- colnames(final_df())
    ## if there are column names (could change this to if final_df is null)
    #if(!is.null(final_df_cols)){

      ## show and update phase filter if the light_dark col has been calculated
      if('light_dark' %in% final_df_cols){
        shinyjs::show(id='plot_phase_filter')
        final_df_phases <- unique(final_df()$phase)
        shinyWidgets::updatePickerInput(session,
                                        "plot_phase_filter",
                                        choices = c(final_df_phases),
                                        selected = character(0))
      }

      ## update column selections for subject ids
      shinyWidgets::updatePickerInput(session,
                                      "which_column_subject_id",
                                      # label = "download_data_col",
                                      choices = c(final_df_cols),
                                      selected = character(0))

      ## show and update subject id selections after choosing col
      observeEvent(input$which_column_subject_id,{
        shinyjs::show(id="plot_animal_filter")
        sub_ids <- unique(final_df()[input$which_column_subject_id])
        shinyWidgets::updatePickerInput(session,
                                        "plot_animal_filter",
                                        # label = "download_data_col",
                                        choices = c(sub_ids),
                                        selected = character(0))
      })

   # }


  }, ignoreInit = TRUE)



})
