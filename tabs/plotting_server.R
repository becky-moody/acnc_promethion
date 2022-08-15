## UI input$ : which_column_subject_id, plot_animal_filter, plot_phase_filter, plot_metric_filter
if(input$promethion_app == 'Plot Data'){

  # always hide these; should be all progress bars/notifications
  shinyjs::hide(id="plot_filter_progress")

## only show anything if you click on this tab
#observeEvent(input$promethion_app == 'Plot Data',{
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

    # shinyjs::hide(id="plot_selected_as_ts")
    # shinyjs::hide(id="plot_selected_as_violin")
    shinyjs::hide(id='plot_type')
    shinyjs::hide(id='run_plot')

    shinyjs::hide(id='outliers_plot')

  } else{

    shinyjs::hide(id='no_data_warning')

    shinyjs::show(id='filter_tab_name')
    shinyjs::show(id="which_column_subject_id")
    shinyjs::show(id='all_plot_filters')

    # shinyjs::show(id='plot_selected_as_ts')
    # shinyjs::show(id="plot_selected_as_violin")
    shinyjs::show(id='plot_type')
    shinyjs::show(id='run_plot')

    shinyjs::hide('outliers_plot')



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

  calc_cols <- c('metric','value','aggregated_interval','phase_num','light_dark','total_phase_num','light_on','phase')


  c_final_df_cols <- unique(final_df_cols[!str_detect(final_df_cols, paste(calc_cols, collapse = '|'))])

  ## update column selections for subject ids
  shinyWidgets::updatePickerInput(session,
                                  "which_column_subject_id",
                                  # label = "download_data_col",
                                  choices = c(c_final_df_cols),
                                  selected = character(0))

  ## show and update subject id selections after choosing col
  observeEvent(input$which_column_subject_id,{
    shinyjs::show(id="plot_animal_filter")
    # create a new column that is subject ids; assuming this column name isn't there
    final_df(final_df() %>% mutate(study_subject_id = !!rlang::sym(input$which_column_subject_id)))#final_df()[input$which_column_subject_id]))
    sub_ids <- unique(final_df()$study_subject_id)
    sub_ids <- na.exclude(sub_ids)
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


  observeEvent(input$run_plot,{
    df <- data.frame()
    # going to nest if this

      shinyjs::hide(id='run_plot')
      shinyjs::show(id="plot_filter_progress")
      # shinyjs::hide(id='plot_selected_as_ts')
      # shinyjs::hide(id='plot_selected_as_violin')
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
      shinyjs::show(id='run_plot')
      # shinyjs::show(id='plot_selected_as_ts')
      # shinyjs::show(id='plot_selected_as_violin')
      #shinyjs::show(id='outliers_plot') # not ready yet


    plot_df(df)

  }, ignoreInit = TRUE)

  }
#}, ignoreInit = TRUE)



plot_gg_ob <- eventReactive(plot_df(),{
  light_colors = c(light = "#FFF68F", dark = "#8DEEEE")

  req(nrow(plot_df())>0)
  plotly_p <- NA
  if(input$plot_type == 'plot_ts'){
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

    plotly_p <- plotly::ggplotly(p, tooltip = 'text')
  } else{

    message('No phases to shade')
    message('Starting ggplot')
    #plot

    p <- ggplot(plot_df())+ theme_minimal() +
      # do points last so they're on top
      geom_point(aes(date_time, value, color = study_subject_id,
                     text = paste0('Date time: ', date_time,
                                   '\n Study Subject ID: ', study_subject_id,
                                   '</br>Metric: ', metric,
                                   '\n Value: ', round(value,4)))) #+
    # grid by each metric
    #facet_grid(~metric, scales = 'free', switch = 'y')

    plotly_p <- plotly::ggplotly(p, tooltip = 'text')
  } # else if(input$plot_type == 'plot_boxplot)
  }

  plotly_p
  }, ignoreInit = TRUE)
  # observeEvent(input$plot_selected_as_violin,{
  #
  #   fill_color <- c('dark'="#A4D3EE", 'light'="#FFEC8B")
  #   color_color <- c('dark'="#4682B4", 'light'= "#EE7621")
  #
  #   y <- example_prom %>% filter(subject_id == 'animal1',metric == 'vo2') %>% mutate(phase = fct_reorder(phase, total_phase_num))
  #
  #   ggplot(y, aes(x = value, y = phase, group = phase, color = light_dark, fill = light_dark)) +
  #     ggridges::geom_density_ridges(
  #       scale = 3, rel_min_height = 0.01,
  #       size = 0.5,alpha = .5 #,color = "steelblue", fill = "lightblue"
  #     ) +
  #     scale_fill_manual(values = `fill_color, guide = 'none') +
  #     scale_color_manual(values = color_color, guide = 'none')+
  #     #scale_x_continuous(breaks = 0:10) +
  #     #scale_y_reverse(breaks = 2000:2015, expand = c(0, 0)) +
  #     #coord_cartesian(clip = "off")+#, xlim = c(0, 10)) +
  #     ggridges::theme_ridges() +
  #     theme(plot.title.position = "plot",
  #       plot.caption.position = "plot",
  #       axis.text = element_text(size = 10)) +
  #     geom_boxplot(data=y, aes(y=phase, x = value),
  #                  width = .25, fill = NA,outlier.color = NA,
  #                  position= position_nudge(y=.05))
  # }, ignoreInit = TRUE) # end observe event(plot violin)


      #             fill = stat(x))) +
      # ggridges::geom_density_ridges_gradient(
      #   scale = 2, rel_min_height = 0.01,
      #   color = "gray50", show.legend = FALSE
      # ) +
      # scale_fill_viridis_c(option = "E") +
      # #scale_x_continuous(breaks = seq(10, 100, 10)) +
      # ggridges::theme_ridges() +
      # theme(
      #   plot.title.position = "plot",
      #   plot.caption.position = "plot",
      #   axis.text = element_text(size = 10)
      # )

    ### attempt at raincloud; I don't think this will be good automated because the data can be so varied; keeping for now though but going towards ridgeline to compare phases
   #  p<-ggplot(plot_df())+ theme_minimal() +
   #    geom_violin(aes(value, color = study_subject_id))
   #
   #  x <- example_prom %>% filter(subject_id == 'animal1', metric == 'allmeters')
   # ggplot(x,
   #         aes(x=subject_id, y = value,
   #             fill = subject_id, group = subject_id)) +
   #
   #
   #   ggdist::stat_slab(aes(thickness = stat(pdf*n)), scale = 0.7,justification = -.3) +
   #   geom_boxplot(width = .25)+
   #   ggdist::stat_dots(side = "bottom", scale = 0.7, slab_size = NA) +
   #   facet_wrap(~phase, scales = 'free')
   #
   #
   #   ggplot(x, aes(x=subject_id, y = value, color = subject_id, group = subject_id, fill = subject_id))+
   #    ggdist::stat_slab(aes(thickness = stat(pdf*n)),
   #              scale = 0.7) +
   #   geom_boxplot(fill = NA, width = .25, color = 'black', alpha = .3)+
   #    ggdist::stat_dotsinterval(side = "bottom",
   #                      scale = 0.6,
   #                      slab_size = NA, quantiles = 100, .width = 0) +
   #     facet_wrap(~subject_id, scales = 'free') +
   #     see::theme_modern()
   #    #see::geom_violindot(trim = FALSE,stackratio = 0.7) + coord_flip()
   #    #geom_violin( aes(x = subject_id, y = value, color = subject_id, draw_quantiles = TRUE))



  #}, ignoreInit = TRUE) # end eventreactive(plot_df())



output$filtered_prom_plot <- plotly::renderPlotly(plot_gg_ob())
  #renderPlot(plot_gg_ob())
#shinipsum::random_ggplot(type = 'bar'))

}
