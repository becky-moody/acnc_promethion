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
    shinyjs::hide(id='all_plot_filters')
    shinyjs::hide(id='run_plot_selections')
    shinyjs::hide(id='run_plot')
    shinyjs::hide(id='outliers_plot')

  } else{

    shinyjs::hide(id='no_data_warning')
    shinyjs::show(id='filter_tab_name')
    shinyjs::show(id='all_plot_filters')
    shinyjs::hide(id='run_plot_selections')
    shinyjs::hide(id='run_plot')
    shinyjs::hide('outliers_plot')

  req(!is.null(final_df()))
  final_df_cols <- colnames(final_df())

  ## update subject id selections after choosing col
  sub_ids <- unique(final_df()$analysis_subject_id)
  sub_ids <- na.exclude(sub_ids)
  shinyWidgets::updatePickerInput(session,
                                  "plot_animal_filter",
                                  choices = c(sub_ids),
                                  selected = character(0))


  metrics <- unique(final_df()$metric)
  shinyWidgets::updatePickerInput(session,
                                  "plot_metric_filter",
                                  choices = c(metrics),
                                  selected = character(0))

  ## show and update phase filter if the light_dark col has been calculated
  if('light_dark' %in% final_df_cols){
    shinyjs::show(id='plot_phase_filter')
    final_df_phases <- unique(final_df()$phase)

    #output$ <- renderText(paste0('The light is on from ',input$start_light,' to ', input$end_light,'.'))
    #shinyjs::show(id='light_on_off_note')
    ## changing to use html vs text to state when light is on/off
    light_on_off_note <- paste0('The light is on from ',input$start_light,' to ', input$end_light,'.')

    shinyjs::html(id='phase_filter_lab',paste('<h4 style = "display: inline;">Which light/dark phase?</h4>','<h6 style = "display: inline;">',light_on_off_note,'</h6>'))

    shinyWidgets::updatePickerInput(session,
                                    "plot_phase_filter",
                                    choices = c(final_df_phases),
                                    selected = c(final_df_phases))
  }

  observeEvent(input$filter_data_for_plot,{
    plot_df(NULL)  # reset data
    output$filtered_prom_ts_plot <<- NULL
    output$filtered_prom_boxplot <<- NULL
    df <- data.frame()

      shinyjs::hide(id='run_plot')
      shinyjs::show(id="plot_filter_progress")
      shinyjs::hide(id='outliers_plot')
      shinyjs::hide(id='run_plot_selections')
      shinyjs::hide(id='run_plot')

      message('Filtering data for plotting')
      shinyWidgets::updateProgressBar(session, id = 'plot_filter_progress', value = 20)
      df <- final_df() %>% mutate(date_time = as.POSIXct(date_time))
      shinyWidgets::updateProgressBar(session, id = 'plot_filter_progress', value = 40)
      Sys.sleep(1)
      # only apply filters if these have values
      if(!is.null(input$plot_animal_filter)){

        message('Subjects: ', input$plot_animal_filter) # this doesn't have spaces but it's for me anyway
        df <- df %>% filter(analysis_subject_id %in% input$plot_animal_filter)
      } else { # or just select a couple cages, could be a lot if there are multiple runs
        df <- df %>% filter(cage_num == sample(cage_num, 2))
      }

      shinyWidgets::updateProgressBar(session, id = 'plot_filter_progress', value = 60)

      if(!is.null(input$plot_metric_filter)){
        show_message <- paste('Metrics: ', ifelse(input$plot_diff_metrics == TRUE, 'difference of',''), input$plot_metric_filter)
        message(show_message)
        rm(show_message)

        df <- df %>% filter(metric %in% input$plot_metric_filter)
      } else{ # or just select one random metric
        df <- df %>% filter(metric == sample(metrics,1))
      }

      # this is the only optional filter
      if(!is.null(input$plot_phase_filter)){
        message('Phases: ', input$plot_phase_filter,'\n')
        df <- df %>% filter(phase %in% input$plot_phase_filter)
      }

      shinyWidgets::updateProgressBar(session, id = 'plot_filter_progress', value = 95)
      Sys.sleep(1)
      shinyjs::hide(id="plot_filter_progress")

      # shinyjs::show(id='plot_selected_as_ts')
      # shinyjs::show(id='plot_selected_as_violin')
      #shinyjs::show(id='outliers_plot') # not ready yet
      shinyjs::show(id='run_plot_selections')
      shinyjs::show(id='run_plot')

    plot_df(df)

  }, ignoreInit = TRUE)

  } # end hide/show else based on if data has been uploaded


observeEvent(input$run_plot,{
  req(nrow(plot_df())>0)
  shinyjs::hide(id='run_plot')
  shinyjs::show(id='plot_filter_progress')
  shinyWidgets::updateProgressBar(session, id = 'plot_filter_progress', value = 20)

  plotly_p <- NA
  plot_p <- NA
  ## set colors
  light_colors = c(light = "#FFF68F", dark = "#8DEEEE")
  fill_color <- c('dark'="#A4D3EE", 'light'="#FFEC8B")
  color_color <- c('dark'="#4682B4", 'light'= "#EE7621")
  cumulative_metrics <- c('foodupa','waterupa','pedmeters','allmeters')

  # plot checks
  plot_check_has_phases <- !is.null(input$plot_phase_filter) # TRUE = has phases; False = no phases
  plot_check_n_phases <- (length(input$plot_phase_filter) > 1) # if more than one convert to difference for cumulative values
  plot_check_is_cumulative_metric <- unique(plot_df()$metric) %in% cumulative_metrics # TRUE = is cumulative; FALSE = not cumulative; this matters for boxplots, cumulative will get changed to diff

  shinyWidgets::updateProgressBar(session, id = 'plot_filter_progress', value = 30)
  # Format data for plotting ----
  ## want boxplot ----
  if(input$plot_type == 'plot_boxplot'){
    ### of a cumulative metric (forcing difference) ----
    if(plot_check_is_cumulative_metric == TRUE){
      if(input$plot_log_transformation == TRUE){
        #### log transformed user chose: diff_log ----
        use_this_df_for_plot <- plot_df() %>% mutate(plot_value = diff_log)
        y_axis_name <- paste0(unique(use_this_df_for_plot$metric), ' differences of ln(value + 1) transformation')
        build_caption <- "Forced conversion to 1 lag difference of ln(value + 1) transformed values of this cumulative metric. Raw cumulative values can be viewed as 'Line over time'."
      } else{
        ### not log transformed forcing difference: diff ----
        use_this_df_for_plot <- plot_df() %>% mutate(plot_value = diff)
        y_axis_name <- paste0(unique(use_this_df_for_plot$metric), ' differences')
        build_caption <- "Forced conversion to 1 lag difference of this cumulative metric. Raw cumulative values can be viewed as 'Line over time'."
      }

    } else{
      ## of a non cumulative metric with user choosing----
      if(input$plot_log_transformation == TRUE & input$plot_diff_metrics == TRUE){
        #### log transformed difference : diff_log ----
      use_this_df_for_plot <- plot_df() %>% mutate(plot_value = diff_log)
      y_axis_name <- paste0(unique(use_this_df_for_plot$metric), ' differences of ln(value + 1) transformation')
      build_caption <- 'User chose 1 lag differences of ln(value + 1) transformed values.'
      } else if(input$plot_log_transformation == FALSE & input$plot_diff_metrics == TRUE){
        #### difference : diff ----
        use_this_df_for_plot <- plot_df() %>% mutate(plot_value = diff)
        y_axis_name <- paste0(unique(use_this_df_for_plot$metric), ' differences')
        build_caption <- 'User chose 1 lag differences of values.'
      } else if(input$plot_log_transformation == TRUE & input$plot_diff_metrics == FALSE){
        #### log transformed  log_value ----
        use_this_df_for_plot <- plot_df() %>% mutate(plot_value = log_value)
        y_axis_name <- paste0(unique(use_this_df_for_plot$metric), ' ln(value + 1) transformation')
        build_caption <- 'User chose ln(value + 1) transformed values.'
      } else{
        #### raw values ----
        use_this_df_for_plot <- plot_df() %>% mutate(plot_value = value)
        y_axis_name <- paste0(unique(use_this_df_for_plot$metric))
        build_caption <- NA
      }
    }
  } else if(input$plot_type == 'plot_ts'){
    ### want ts plot cumulative doesn't matter ----
    if(input$plot_log_transformation == TRUE & input$plot_diff_metrics == TRUE){
      #### log transformed difference : diff_log ----
      use_this_df_for_plot <- plot_df() %>% mutate(plot_value = diff_log)
      y_axis_name <- paste0(unique(use_this_df_for_plot$metric), ' difference of ln(value + 1) transformation')
      build_caption <- 'User chose 1 lag differences of ln(value + 1) transformed values.'
    } else if(input$plot_log_transformation == FALSE & input$plot_diff_metrics == TRUE){
      #### difference : diff ----
      use_this_df_for_plot <- plot_df() %>% mutate(plot_value = diff)
      y_axis_name <- paste0(unique(use_this_df_for_plot$metric), ' differences')
      build_caption <- 'User chose 1 lag differences of values.'
    } else if(input$plot_log_transformation == TRUE & input$plot_diff_metrics == FALSE){
      #### log transformed  log_value ----
      use_this_df_for_plot <- plot_df() %>% mutate(plot_value = log_value)
      y_axis_name <- paste0(unique(use_this_df_for_plot$metric), ' ln(value + 1) transformation')
      build_caption <- 'User chose ln(value + 1) transformed values.'
    } else{
      #### raw values ----
      use_this_df_for_plot <- plot_df() %>% mutate(plot_value = value)
      y_axis_name <- paste0(unique(use_this_df_for_plot$metric))
      build_caption <- NA
    }
  } else{ # end plot type
    ## else raw values ----
    use_this_df_for_plot <- plot_df() %>% mutate(plot_value = value)
    y_axis_name <- paste0(unique(use_this_df_for_plot$metric))
    build_caption <- NA
  }

  if(input$plot_type == 'plot_boxplot'){
    plot_check_has_phases <- 'light_dark' %in% colnames(use_this_df_for_plot)
    if(plot_check_has_phases == TRUE ){
      ###################################################################-
      ## Boxplots that include phases ----
      message('Starting plot')
      shinyWidgets::updateProgressBar(session, id = 'plot_filter_progress', value = 50)
      use_this_df_for_plot <- use_this_df_for_plot %>% mutate(phase = fct_reorder(phase, total_phase_num))

      message('Building plot')
      p <-ggplot(use_this_df_for_plot, aes(x=phase, y = plot_value,
                                           group = phase, fill = light_dark, color = light_dark)) +
        gghalves::geom_half_boxplot(nudge =.02, outlier.alpha = .8, outlier.size = .8, outlier.colour = "#CD2626" ) +
        gghalves::geom_half_violin(side = 'r',position = "identity", trim = TRUE, nudge = .02,
                                   scale = 3)+
        scale_fill_manual(values = fill_color, guide = 'none') +
        scale_color_manual(values = color_color, guide = 'none')+
        theme_minimal() +
        labs(x = 'Phases', y=y_axis_name, caption = build_caption) +
        facet_grid(rows = 'analysis_subject_id' ,scales = 'free_y') +
        theme(panel.spacing = unit(.05, "lines"),
              panel.border = element_rect(color = "black", fill = NA, size = .5),
              strip.background = element_rect(color = "black", size = .5))
      message('Finished with boxplot of metric with phases')

    } else if(plot_check_has_phases == FALSE){
      shinyWidgets::updateProgressBar(session, id = 'plot_filter_progress', value = 50)
      message('Building plot')

      p <-ggplot(use_this_df_for_plot, aes(x=0,y = plot_value, color = analysis_subject_id)) +
        gghalves::geom_half_boxplot(nudge =.02, outlier.alpha = .8, outlier.size = .8, outlier.colour = "#CD2626" ) +
        gghalves::geom_half_violin(side = 'r',position = "identity", trim = TRUE, nudge = .02,
                                   scale = 3)+
        theme_minimal() +
        labs(x = '', y=y_axis_name, caption = build_caption) +
        facet_grid(rows = 'analysis_subject_id' ,scales = 'free_y') +
        theme(panel.spacing = unit(.05, "lines"),
              panel.border = element_rect(color = "black", fill = NA, size = .5),
              strip.background = element_rect(color = "black", size = .5))

      message('Finished with boxplot of metric without phases')
    } else{
      p <- NULL
    }
    plot_p <- p
    shinyWidgets::updateProgressBar(session, id = 'plot_filter_progress', value = 80)
    output$filtered_prom_boxplot <- renderPlot(plot_p)

    ################################################################################
  } else if(input$plot_type == 'plot_ts'){
    plot_check_has_phases <- 'light_dark' %in% colnames(use_this_df_for_plot)
    if(plot_check_has_phases == TRUE ){
      shinyWidgets::updateProgressBar(session, id = 'plot_filter_progress', value = 50)

      message('Getting line breaks for phases')
      ### line breaks for light/dark switches (geom_vline)----
      phase_changes <- use_this_df_for_plot %>% group_by(metric) %>%
        mutate(max_value = max(plot_value),
               min_value = min(plot_value)) %>% ungroup() %>%
        mutate(value_range = max_value - min_value,
               # adjust text so it doesn't hang off, not sure if this will work for every one
               text_placement = value_range * (2/3)+min_value) %>%
        group_by(metric, phase_num, phase, max_value,text_placement) %>%
        summarise(min_dt = min(date_time))

      message('Getting background for phases')
      ## background for light/dark (geom_rect) ----
      shaded_rect <- use_this_df_for_plot %>% ungroup() %>%
        group_by(metric) %>%
        mutate(y_max = max(plot_value, na.rm = TRUE),
               y_min = min(plot_value, na.rm= TRUE),
               y_min = ifelse(y_min == 0, 0, y_min)) %>%
        group_by(metric, light_dark, phase_num, y_max, y_min) %>%
        summarise(start_time = min(date_time, na.rm = TRUE),
                  end_time = max(date_time, na.rm = TRUE)) %>%
        ungroup()

      message('Building plot')
      ## ggplot ts plot that include phases ----

      ## label notes: scales::label_wrap() stringr::str_wrap() for wrapping

      p <- ggplot(use_this_df_for_plot)+
        theme_minimal() +
        # set axis names
        labs(x = 'Date Time', y = y_axis_name, caption = build_caption) +
        # build background light/dark
        geom_rect(data = shaded_rect, aes(xmin = start_time,
                                          xmax = end_time,
                                          ymin = y_min,
                                          ymax = y_max,
                                          fill = light_dark),
                  alpha = .75, stat = 'identity')+
        # set background colors
        scale_fill_manual(values = fill_color) +
        # add lines for breaks
        geom_vline(data = phase_changes, aes(xintercept = as.numeric(min_dt)),
                   show.legend = FALSE, color = 'black',linetype = 'dashed',alpha=.5 )+
        geom_line(aes(x=date_time, y=plot_value, color = analysis_subject_id, group = analysis_subject_id)) +
        # points last so they're on top
        geom_point(aes(x=date_time, y=plot_value, color = analysis_subject_id, group = analysis_subject_id,
                       ## this will be hover tooltip in plotly
                       text = paste0('Date time: ', date_time, '  (',phase,')',
                                     '\nStudy Subject ID: ', analysis_subject_id,
                                     '</br>Metric: ', metric,
                                     '\nValue: ', round(plot_value,4))))

      rm(y_axis_name, shaded_rect, phase_changes) ## clean up
      message('Finished with plot \n')
    } else if(plot_check_has_phases == FALSE){

      p <- ggplot(use_this_df_for_plot)+ theme_minimal() +
        geom_line(aes(x=date_time, y=plot_value, color = analysis_subject_id, group = analysis_subject_id)) +
        geom_point(aes(x=date_time, y=plot_value, color = analysis_subject_id,group = analysis_subject_id,
                       text = paste0('Date time: ', date_time,
                                     '\nStudy Subject ID: ', analysis_subject_id,
                                     '</br>Metric: ', metric,
                                     '\nValue: ', round(plot_value,4)))) +
        labs(x = 'Date Time', y = y_axis_name, caption = build_caption)

    }else{
      p <- NULL
    }
    plotly_p <- plotly::ggplotly(p, tooltip = c('text'))

    shinyWidgets::updateProgressBar(session, id = 'plot_filter_progress', value = 80)
    output$filtered_prom_ts_plot <- plotly::renderPlotly(plotly_p)

  }
  Sys.sleep(.1)
  shinyjs::hide(id='plot_filter_progress')
  shinyjs::show(id='run_plot')

}, ignoreInit = TRUE) # end observeEvent run_plot
#



# observeEvent(input$calculate_outliers,{
#   cumulative_metrics <- c('foodupa','waterupa','pedmeters','allmeters')
#   req(nrow(plot_df())>0)
#   no_outlier_df(NULL)
#
#   df <- plot_df()
#   c_df <- df
#
#   ## outliers calculated using log(x+1) values; if cumulative using diff(log1p(value)) values
#   if(df$metric %in% cumulative_metrics){
#     df <- df %>% ungroup() %>%
#       group_by(metric, analysis_subject_id) %>%
#       arrange(date_time) %>%
#       mutate(calc_value = log_value - lag(log_value)) %>% ungroup()
#   }else{
#     df <- df %>% mutate(calc_value = log_value)
#   }
#
#
#   if(input$calc_outliers == 'remove_outliers'){
#     # phases included?
#     if('light_dark' %in% colnames(df)){
#       iqr_df <- df %>% group_by(metric, analysis_subject_id, phase_num) %>%
#         mutate(iqr = IQR(calc_value, na.rm = TRUE),
#                upper = quantile(calc_value, .75),
#                lower = quantile(calc_value, .25) ) %>%
#         ungroup() %>%
#         mutate(upper = upper  + 1.5 * IQR,
#                lower = lower  - 1.5 * IQR,
#           is_outlier = ifelse((calc_value > upper) | (calc_value < lower), 1, 0))
#     }else{
#       iqr_df <- df %>% group_by(metric, analysis_subject_id) %>%
#         mutate(iqr = IQR(calc_value))%>%
#         ungroup() %>%
#         mutate(upper = upper  + 1.5 * IQR,
#                lower = lower  - 1.5 * IQR,
#                is_outlier = ifelse((calc_value > upper) | (calc_value < lower), 1, 0))
#     }
#     c_df <- iqr_df %>% filter(is_outlier == 0) %>%
#       mutate(outlier_method = 'iqr') %>%
#       select(-upper,-lower,-iqr,-is_outlier)
#
#   } else if(input$calc_outliers == 'impute_outliers'){
#     ## do this later
#
#   }else if(input$calc_outliers == 'ts_outliers'){
#     #tsoutliers tsclean
#     ts_df <- ex_data %>% filter(metric == 'kcalhr', subject_id == 'animal1') %>%
#       mutate(analysis_subject_id = subject_id,
#              date_time = as.POSIXct(date_time) ) %>%
#
#
#     # %>% tsibble::as_tsibble(., index = date_time,
#     #                       key = c(metric, analysis_subject_id),
#     #                       regular = TRUE)
#
#
#     #ex_data <- read.csv(here::here('example data/promethion_cleaned_with_phases_5minutes_2022-08-10.csv'))
#   }
#   #no_outlier_df(c_df)
#
# }, ignoreInit = TRUE)




# plot_ts_ob <- eventReactive(input$plot_type == 'plot_ts',{
#   #req(input$plot_type == 'plot_ts')
# ###################################################################-
# # setup #
#   req(nrow(plot_df())>0)
#   #data_for_plots <- plot_df()
#   plotly_p <- NA
#   ## set colors
#   light_colors = c(light = "#FFF68F", dark = "#8DEEEE")
#   fill_color <- c('dark'="#A4D3EE", 'light'="#FFEC8B")
#   color_color <- c('dark'="#4682B4", 'light'= "#EE7621")
#   cumulative_metrics <- c('foodupa','waterupa','pedmeters','allmeters')
#
#   # if else checks #
#   plot_check_has_phases <- !is.null(input$plot_phase_filter) # TRUE = has phases; False = no phases
#   plot_check_is_cumulative_metric <- input$plot_metric_filter %in% cumulative_metrics # TRUE = is cumulative; FALSE = not cumulative; this matters for boxplots, cumulative will get changed to diff
#

# #return(plotly_p)
#
# }, ignoreInit = TRUE)
###################################################################-
###################################################################-




  #   if(!is.null(input$plot_phase_filter)){
  #
  #     ## There is no point plotting cumulative metrics as boxplots over time; get the 1 diff
  #     cumulative_metrics <- c('foodupa','waterupa','pedmeters','allmeters')
  #     plot_check <- (input$plot_metric_filter %in% cumulative_metrics & (length(input$plot_phase_filter))>1)
  #
    #   if(input$plot_metric_filter %in% cumulative_metrics){
    #     diff_plot_df <- plot_df() %>% ungroup() %>%
    #       group_by(metric, phase, subject_id) %>%
    #       arrange(date_time) %>%
    #       mutate(value_diff = value - lag(value))
    #     y_axis_name <- paste(input$plot_metric_filter,'difference ( ln(x+1) )')
    #
    #     p <-ggplot(diff_plot_df, aes(x=phase, y = log1p(value_diff), group = phase, fill = light_dark, color = light_dark)) +
    #       gghalves::geom_half_boxplot(nudge =.02, outlier.alpha = .8, outlier.size = .8, outlier.colour = "#CD2626" ) +
    #       gghalves::geom_half_violin(side = 'r',position = "identity", trim = TRUE, nudge = .02,
    #                                  scale = 3)+
    #       scale_fill_manual(values = fill_color, guide = 'none') +
    #       scale_color_manual(values = color_color, guide = 'none')+
    #       theme_minimal() +
    #       labs(x = 'Phases', y=y_axis_name, caption = "Converted to 1 lag difference, cumulative metrics need to be viewed as 'Line over time'.") +
    #       facet_grid(rows = 'subject_id' ,scales = 'free_y') +
    #       theme(panel.spacing = unit(.05, "lines"),
    #             panel.border = element_rect(color = "black", fill = NA, size = .5),
    #             strip.background = element_rect(color = "black", size = .5))
    #     ## clean up
    #     rm(diff_plot_df, y_axis_name)
    #     # end cumulative metric boxplots
    #   } else{
    #     y_axis_name <- paste(input$plot_metric_filter,'( ln(x+1) )')
    #   p <-ggplot(plot_df(), aes(x=phase, y = log1p(value), group = phase, fill = light_dark, color = light_dark)) +
    #   gghalves::geom_half_boxplot(nudge =.02, outlier.alpha = .8, outlier.size = .8, outlier.colour = "#CD2626" ) +
    #   gghalves::geom_half_violin(side = 'r',position = "identity", trim = TRUE, nudge = .02,
    #                              scale = 3)+
    #   scale_fill_manual(values = fill_color, guide = 'none') +
    #   scale_color_manual(values = color_color, guide = 'none')+
    #   theme_minimal() +
    #   labs(x = 'Phases',y='Value ln(x+1)') +
    #   facet_grid(rows = 'subject_id', scales = 'free_y', switch = 'y')
    #   ## clean up
    #   rm(y_axis_name)
    #   }
    #   plotly_p <- p ## not doing plotly for
    # } else{
    #   plotly_p <- NA
    # }
  #
  # } else {
  #   plotly_p <- NA
  #
  # }




# output$filtered_prom_ts_plot <- plotly::renderPlotly(plot_ts_ob())
#
# output$filtered_prom_boxplot <- renderPlot(plot_boxplot_ob())


}

############## old
  # observeEvent(input$plot_selected_as_violin,{
  #
    # fill_color <- c('dark'="#A4D3EE", 'light'="#FFEC8B")
    # color_color <- c('dark'="#4682B4", 'light'= "#EE7621")
    # out_color <- c('dark'="#528B8B", 'light'="#EE2C2C")
    #
    # y <- df %>% filter(subject_id %in% c('animal1','animal2'),metric == 'allmeters') %>% mutate(phase = fct_reorder(phase, total_phase_num))



    ### this doesn't work great for a lot of these
    # ggplot(y, aes(x = value, y = phase, group = phase, color = light_dark, fill = light_dark)) +
    #   #ggridges::geom_density_ridges(
    #   ggridges::geom_density_ridges(
    #     scale = 3, rel_min_height = 0.01,
    #     size = .3,alpha = .5 #,color = "steelblue", fill = "lightblue"
    #   ) +
    #   scale_fill_manual(values = fill_color, guide = 'none') +
    #   scale_color_manual(values = color_color, guide = 'none')+
    #   #scale_x_continuous(breaks = 0:10) +
    #   #scale_y_reverse(#breaks = 2000:2015, expand = c(0, 0)) +
    #   #coord_cartesian(clip = "off")+#, xlim = c(0, 10)) +
    #   ggridges::theme_ridges() +
    #   theme(plot.title.position = "plot",
    #     plot.caption.position = "plot",
    #     axis.text = element_text(size = 10)) +
    #   geom_boxplot(data=y, aes(y=phase, x = value),
    #                width = .25, fill = NA,#outlier.color = NA,
    #                position= position_nudge(y=.1))
  # }, ignoreInit = TRUE) # end observe event(plot violin)



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
     # ggdist::stat_slab(aes(thickness = stat(pdf*n)), scale = 0.7,justification = -.3) +
     # geom_boxplot(width = .25)+
     # ggdist::stat_dots(side = "bottom", scale = 0.7, slab_size = NA) +
     # facet_wrap(~phase, scales = 'free')
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



# output$filtered_prom_plot <- renderUI({
#   if(input$plot_type == 'plot_ts'){
#   plotly::renderPlotly(plot_gg_ob())
#   }
# })

  #renderPlot(plot_gg_ob())
#shinipsum::random_ggplot(type = 'bar'))


