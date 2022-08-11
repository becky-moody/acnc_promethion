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
    shinyjs::hide(id="plot_filter_progress")

    shinyjs::hide(id='outliers_plot')

  } else{

    shinyjs::hide(id='no_data_warning')

    shinyjs::show(id='filter_tab_name')
    shinyjs::show(id="which_column_subject_id")
    shinyjs::show(id='all_plot_filters')

    shinyjs::show(id='plot_selected')
    shinyjs::hide(id="plot_filter_progress")

    shinyjs::hide('outliers_plot')

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
                                    selected = c(final_df_phases))
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
    # create a new column that is subject ids; assuming this column name isn't there
    final_df(final_df() %>% mutate(study_subject_id = !!rlang::sym(input$which_column_subject_id)))#final_df()[input$which_column_subject_id]))
    sub_ids <- unique(final_df()$study_subject_id) #unique(final_df()[input$which_column_subject_id])
    shinyWidgets::updatePickerInput(session,
                                    "plot_animal_filter",
                                    # label = "download_data_col",
                                    choices = c(sub_ids),
                                    selected = character(0))
  })

  metrics <- unique(final_df()$metric)
  shinyWidgets::updatePickerInput(session,
                                  "plot_metric_filter",
                                  # label = "download_data_col",
                                  choices = c(metrics),
                                  selected = character(0))


  observeEvent(input$plot_selected,{
    df <- data.frame()
    # going to nest if this
    if(input$plot_selected > 0 & !is.null(input$plot_selected)){

      shinyjs::show(id="plot_filter_progress")
      shinyjs::hide(id='plot_selected')
      shinyjs::hide(id='outliers_plot')

      message('Filtering data for plotting')
      shinyWidgets::updateProgressBar(session, id = 'plot_filter_progress', value = 20)
      df <- final_df() %>% mutate(date_time = as.POSIXct(date_time))
      shinyWidgets::updateProgressBar(session, id = 'plot_filter_progress', value = 40)
      Sys.sleep(1)
      # only apply filters if these have values
      if(!is.null(input$plot_animal_filter)){

        message('Subjects: ', input$plot_animal_filter) # this doesn't have spaces but it's for me anyway
        df <- df %>% filter(study_subject_id %in% input$plot_animal_filter)
        #!!rlang::sym(input$which_column_subject_id) %in% input$plot_animal_filter)
      } else { # or just select cages, could be a lot if there are multiple runs
        df <- df %>% filter(cage_num %in% c(1,2))
      }

      shinyWidgets::updateProgressBar(session, id = 'plot_filter_progress', value = 60)



      if(!is.null(input$plot_metric_filter)){
        message('Metrics: ', input$plot_metric_filter)

        df <- df %>% filter(metric %in% input$plot_metric_filter)
      } else{ # or just select one random metric
        df <- df %>% filter(metric == sample(metrics,1))
      }

      # this is the only optional filter
      if(!is.null(input$plot_phase_filter)){
        message('Phases: ', input$plot_phase_filter)
        df <- df %>% filter(phase %in% input$plot_phase_filter)
      }

      shinyWidgets::updateProgressBar(session, id = 'plot_filter_progress', value = 95)
      Sys.sleep(1)
      shinyjs::hide(id="plot_filter_progress")
      shinyjs::show(id='plot_selected')
      shinyjs::show(id='outliers_plot')
    }

    plot_df(df)

  }, ignoreNULL = FALSE)


}, ignoreInit = TRUE)

plot_gg_ob <- eventReactive(plot_df(),{
  light_colors = c(light = "#FFF68F", dark = "#8DEEEE")

  req(nrow(plot_df())>0)
  message('Building plot')
  if(!is.null(input$plot_phase_filter)){

    message('Getting line breaks for plots')
    # for line breaks of when light/dark switches
    phase_changes <- plot_df() %>% group_by(metric) %>%
      mutate(max_value = max(value),
             min_value = min(value)) %>% ungroup() %>%
      mutate(value_range = max_value - min_value,
             text_placement = value_range * (2/3)+min_value) %>%
      group_by(metric, phase_num, phase, max_value,text_placement) %>%
      summarise(min_dt = min(date_time))

    message('Getting background for plots')
    # shade the background for light/dark
    shaded_rect <- plot_df() %>% group_by(metric) %>%
      mutate(y_max = max(value)) %>%
      group_by(study_subject_id, metric, light_dark, phase_num, y_max) %>%
      summarise(start_time = min(date_time), end_time = max(date_time),
                y_min = min(value)) %>%
      ungroup() %>% group_by(metric) %>%
      mutate(y_min = ifelse(any(y_min==0),0,y_min))

    message('Starting ggplot')
    #plot

    p<-ggplot(plot_df())+ theme_minimal() +
      labs(x = 'Datetime',
             y = 'value') +
      # background light/dark
      geom_rect(data = shaded_rect, aes(xmin = start_time,
                                        xmax = end_time,
                                        ymin = y_min,
                                        ymax = y_max,
                                        fill = light_dark),
                alpha = .75, stat = 'identity')+
      # set background colors
      scale_fill_manual(name = 'Phase', values = light_colors) +
      # lines for breaks
      geom_vline(data = phase_changes, aes(xintercept = min_dt),
                 show.legend = FALSE, color = 'black',linetype = 'dashed',alpha=.5 )+
      # text for line breaks
      # geom_text(data = phase_changes, aes(x = min_dt,
      #                                     y = text_placement,
      #                                     label = phase),
      #           # move label forward 1 hour (in seconds), rotate text, set font size, set transparency
      #           nudge_x = 3600, angle = 90, size = 4, alpha = .5)+
      # do points last so they're on top
      geom_point(aes(date_time, value, color = study_subject_id,
                     text = paste0('Date time: ', date_time, '  (',phase,')',
                     '\n Study Subject ID: ', study_subject_id,
                      '</br>Metric: ', metric,
                     '\n Value: ', round(value,4))))
      # grid by each metric
      #facet_grid(~metric, scales = 'free', switch = 'y')
    #shinipsum::random_ggplot(type = 'boxplot')

    plotly::ggplotly(p, tooltip = 'text')
  } else{

    message('No phases to shade')
    message('Starting ggplot')
    #plot

    p <- ggplot(plot_df())+ theme_minimal() +
      # do points last so they're on top
      geom_point(aes(date_time, value, color = study_subject_id)) #+
      # grid by each metric
      #facet_grid(~metric, scales = 'free', switch = 'y')

    plotly::ggplotly(p)
  }

}, ignoreInit = TRUE)

output$filtered_prom_plot <- plotly::renderPlotly(plot_gg_ob())
  #renderPlot(plot_gg_ob())
#shinipsum::random_ggplot(type = 'bar'))
