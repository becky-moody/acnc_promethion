if(input$promethion_app == 'Data Editor'){


if(is.null(final_df())){

      shinyjs::hide(id='auto_file_notes')
      shinyjs::hide(id='file_progress_bar')

      shinyjs::hide(id='phase_ui')

      shinyjs::hide(id='data_download_header')
      shinyjs::hide(id='download_prom_data_btn')
      shinyjs::hide(id='download_full_data_btn')
    } else{
      shinyjs::show(id='phase_ui')
      if('light_dark' %in% colnames(final_df())){
        shinyjs::show(id='download_full_data_btn')
      }else{
        shinyjs::show(id='download_prom_data_btn')
      }
    }


observeEvent(input$auto_file_selection,{
  if(input$auto_file_selection == TRUE){
    shinyjs::hide(id='prom_file')
    shinyjs::hide(id='meta_file')
    shinyjs::show(id='auto_file_notes')
  } else {
    shinyjs::show(id='prom_file')
    shinyjs::show(id='meta_file')
    shinyjs::hide(id='auto_file_notes')
  }
})

## have user select subject_id column

observeEvent(input$meta_file,{
  req(input$meta_file)

  ext <- tools::file_ext(input$meta_file$name)
  meta_dat <- switch(ext,
                     csv = read.csv(input$meta_file$datapath),
                     xls = readxl::read_xls(input$meta_file$datapath),
                     xlsx = readxl::read_xlsx(input$meta_file$datapath),
                     validate('Please select a .csv, .xls, or .xlsx file.'))

  #fix column names; I don't care what is in this
  # NEED: run and cage_num
  meta_df <- meta_dat #%>%janitor::clean_names()
  meta_colnames <- colnames(meta_df)

  updateSelectInput(session,
                    "which_column_subject_id1",
                    choices = c(meta_colnames),
                    selected = character(0))

  #shinyjs::show(id="which_column_subject_id1")

}, ignoreNULL = TRUE)

observeEvent(input$auto_file_selection == TRUE,{
  shinyjs::html(id = 'sub_col_lab1', '<s>Which column in meta data contains subject ids?</s> This column has already been selected for the example data.')
  updateSelectInput(session,
                    "which_column_subject_id1",
                    choices = 'subject_id',
                    selected = 'subject_id')
  updateSelectInput(session, 'aggregate_data', label = 'Change the example data aggregation?')
  shinyjs::html(id = 'start_light_lab', 'Change Light/Dark Phases? <small>The example data is set from 7:00 to 19:00.</small>')
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# press aggregate_data_btn to read, clean, join files ----
observeEvent(input$aggregate_data_btn,{

  shinyjs::hide(id='aggregate_data_btn')
  shinyjs::show(id='file_progress_bar')

  # ## reset these
  # final_df(NULL)
  # shinyWidgets::updatePickerInput(session,
  #                                 "download_data_col",
  #                                 # label = "download_data_col",
  #                                 choices = character(0))

  #prom_dat <- eventReactive(input$aggregate_data_btn,{#$upload_files_btn,{
  # to do: remove this!
  # saved file from doing this

  ## testing file based on check box ----

  if(input$auto_file_selection == TRUE){
    #final_df(readxl::read_xlsx(here::here('data/promethion_cleaned_5minutes_2022-07-26.xlsx')))
    # shinyjs::hide(id='prom_file')
    # shinyjs::hide(id='meta_file')
    # shinyjs::show(id='auto_file_notes')
    shinyWidgets::updateProgressBar(session, id = 'file_progress_bar', value = 10)
    df <- read.csv(here::here('example data/promethion_cleaned_with_phases_5minutes_2022-08-25.csv'))

    if(input$aggregate_data == '5 minutes'){
      message('Using test data by 5 mins, no need to run.')
      final_df(df)

      metrics <- unique(df$metric)

      shinyWidgets::updatePickerInput(session,
                                      "download_data_col",
                                      # label = "download_data_col",
                                      choices = c(metrics),
                                      selected = c(metrics))
    } else{
      final_df(NULL)
      shinyWidgets::updateProgressBar(session, id = 'file_progress_bar', value = 30)

      agg_by <- input$aggregate_data
      #print(agg_by)
      message('Aggregating data by ', agg_by)
      # only the minutes makes needed; cant do cut(time, '30 minutes')
      agg_breaks <- c('5 minutes'= '5 min', '30 minutes'='30 min', '1 hour'='1 hour', '6 hours'='6 hour')
      agg_col <- paste0('date_time_', gsub(' ','', agg_by))

      cumulative_metrics <- c('foodupa','waterupa','pedmeters','allmeters')

      ## if by anything other than 5 minutes then aggregate data and then find differences
      #if(agg_by != '5 minutes'){
      shinyWidgets::updateProgressBar(session, id = 'file_progress_bar', value = 50)
      withProgress(message = 'Aggregating test file.', detail = 'This might take time.\n', value = 0, {
        agg_df <- df %>% mutate(date_time = as.POSIXct(date_time)) %>%
          arrange(date_time) %>%
          group_by(metric, run, cage_num) %>%
          # get aggregated time intervals;
          mutate(date_time = cut(date_time, agg_breaks[agg_by]),
                 date_time = as.POSIXct(date_time))

        incProgress(1/3, detail = paste0('Breaking datetimes by ',agg_breaks[agg_by],'.'))
        # exclude missing

        agg_df <- agg_df %>%
          group_by(run, cage_num, metric) %>%
          # get start/end date_times after aggregation
          mutate(start_date =  min(date_time, na.rm = TRUE),
                 end_date = max(date_time, na.rm = TRUE)) %>%
          ungroup() %>% arrange(metric,run, cage_num, date_time) %>%
          # now group by to calculate values
          group_by(date_time, metric, start_date, end_date,
                   file_num_uploaded, across(colnames(meta_df_c))) %>%
          # cumulative values are going to use max; average everything else
          summarize(value = case_when(all(metric %in% cumulative_metrics) ~
                                        max(raw_n, na.rm=TRUE),
                                      TRUE ~ mean(raw_n, na.rm = TRUE))) %>%
          ungroup()
        incProgress(1/3, detail = 'Calculating differences.')

        agg_df <- agg_df %>% ungroup() %>%
          mutate(date_time = as.POSIXct(date_time), # this is unnecessary but if it's not it's going to be a mess
                 # getting log values which will mostly be used for density plots later
                 log_value = log1p(value)) %>%
          group_by(run, cage_num, metric) %>%
          arrange(metric, run, cage_num, date_time) %>%
          mutate(diff = value - lag(value),
                 # because I need to make sure I use this and not log(diff(value))
                 diff_log = log_value - lag(log_value))

        incProgress(1/3, detail = 'Finished with aggregation. Formatting columns.')
        ## format the columns for saving
        aggregated_df <- agg_df %>%
          ungroup() %>% arrange(metric, run, cage_num, date_time) %>%
          mutate(aggregated_interval = agg_breaks[agg_by],
                 date_time = as.POSIXct(date_time)) %>%
          mutate(across(contains('date'), as.character)) %>%
          mutate(across(is.numeric, round, digits = 4)) %>%
          relocate(value, diff, .after = metric) %>%
          relocate(analysis_subject_id, run, cage_num, .before = date_time)

      })
      shinyWidgets::updateProgressBar(session, id = 'file_progress_bar', value = 90)

      # Update download columns selections ----
      metrics <- unique(aggregated_df$metric)

      shinyWidgets::updatePickerInput(session,
                                      "download_data_col",
                                      #label = "download_data_col",
                                      choices = c(metrics),
                                      selected = c(metrics)
      )

    final_df(aggregated_df)
    }

  } else{
    ## begin reading promethion files one at a time ----
    req(input$prom_file)
    n_prom_files <- length(input$prom_file$name)
    message('There are ', n_prom_files, ' files uploaded.')

    # initalize df of everything; will contain all promethion file info
    all_cage <- data.frame()
    shinyWidgets::updateProgressBar(session, id = 'file_progress_bar', value = 10)
    withProgress(message = 'Reading promethion files.', value = 0, {
      # calling twice inside loop for each file and then a final time outside loop
      progress_denominator <- n_prom_files * 2 + 1
      # loop through however many promethion files are uploaded;
      # input$prom_file is a list of all selected files
      for(i in 1:n_prom_files){
        incProgress(1/progress_denominator, detail = paste("Starting file", i))
        ### check file format
        # get file extension so use the correct read function
        ext <- tools::file_ext(input$prom_file$name[i])
        dat <- switch(ext,
                      csv = read.csv(input$prom_file$datapath[i]),
                      xls = readxl::read_xls(input$prom_file$datapath[i]),
                      xlsx = readxl::read_xlsx(input$prom_file$datapath[i]),
                      validate('If file is .xml, save as .xlsx in Excel. Otherwise upload a .csv.'))

        ####################################
        # manual testing;
        # TO DO: comment or delete when done
        # all_cage <- data.frame() # this is above but easier for testing here
        # dat <- read.csv('example data/example_prom2_for_testing_app.csv')
        # i=1
        # meta_dat <- readxl::read_xlsx('example data/example_metadata.xlsx')
        # agg_by <- '30 minutes'
        ####################################

        ## file has been read, start cleaning
        message('Working on file #', i, '. ', input$prom_file$name[i])

        ### promethion date adjust, pivoting, removing enviro cols
        cage_file <- dat %>% janitor::clean_names() %>%
          ## assume: always "date_time_1"
          rename(date_time = date_time_1) %>%
          # force datatime type
          mutate(date_time = as.POSIXct(date_time, format='%m/%d/%Y %H:%M', tz = Sys.timezone())) %>%
          # pivot all the metric columns
          pivot_longer(-c(1:6), names_to = 'name', values_to = 'raw_n') %>%
          ## the _ in this with break in separate
          mutate(name = gsub('kcal_hr','kcalhr',name)) %>%
          # break out metric and cage number
          separate(name, into = c('metric','cage_num'), sep = '_') %>%
          # force column types and create file_num for tracking
          mutate(cage_num = as.numeric(cage_num),
                 # start_dt = min(date_time, na.rm = TRUE),
                 # end_dt = max(date_time, na.rm = TRUE),
                 file_num_uploaded = i) %>%
          ## assume: these variables are not needed
          select(!contains('enviro')) %>%
          # sometimes pivoting adds all na rows; we don't want anything that's missing anyway
          na.omit() %>%
          # order the data, not really needed
          group_by(cage_num, metric) %>%
          mutate(start_date =  min(date_time, na.rm = TRUE),
                 end_date = max(date_time, na.rm = TRUE)) %>%
          ungroup()
        incProgress(1/progress_denominator, detail = paste("Finished with file", i))
        ### rbind each 'cleaned' prometion file to df
        # add to cage df; new rows = next file
        all_cage <- rbind(all_cage, cage_file)
      }


      message('Finished with upload, reading, pivoting, and joining promethion files.')
      incProgress(1/progress_denominator, detail = 'Calculating run column')
      ## get 'run' of prometion based on date

      # create a run column which will be used to join to meta data
      # to do: convert to dt?
      # assume: the "run" in the meta data will be oldest = 1 newest = #
      # create "run" column to order the files
      cage_run <- all_cage %>% ungroup() %>%
        select(file_num_uploaded, start_date) %>%
        unique() %>%
        arrange(start_date) %>%
        mutate(run = row_number())

      all_cage <- left_join(all_cage, cage_run,
                            by = c('file_num_uploaded', 'start_date')) %>%
        ungroup()

    })
    #don't need this anymore
    rm(cage_run)

    #print(head(all_cage,2))
    # all_cage

    ## Meta data read and inner join to prometion ----

    req(input$meta_file)
    req(input$which_column_subject_id1)
    message('Reading meta data file.')
    shinyWidgets::updateProgressBar(session, id = 'file_progress_bar', value = 50)
    withProgress(message = 'Reading meta data file.', value = 0, {
      incProgress(1/3, detail = 'Checking file.')
      # same as before
      ext <- tools::file_ext(input$meta_file$name)
      meta_dat <- switch(ext,
                         csv = read.csv(input$meta_file$datapath),
                         xls = readxl::read_xls(input$meta_file$datapath),
                         xlsx = readxl::read_xlsx(input$meta_file$datapath),
                         validate('Please select a .csv, .xls, or .xlsx file.'))

      #fix column names; I don't care what is in this
      # get analysis_subject_id
      # NEED: run and cage_num
      meta_df <- meta_dat %>%
        mutate(analysis_subject_id = !!rlang::sym(input$which_column_subject_id1)) %>%
        janitor::clean_names()
      #print(head(meta_df,2))

      req_cols <- c('run'='run number','run'='Run',
                    'cage_num'='cage','cage_num'='cage num','cage_num'='cage_number','cage_num'='cage number',
                    'meta_date'='date','meta_date'='start_date')
      incProgress(1/3, detail = 'Renaming columns and creating the analysis_subject_id')
      meta_df_c <- meta_df %>% rename(any_of(req_cols))

      if(!('run' %in% colnames(meta_df_c)) & length(input$prom_file$name) == 1){
        meta_df_c <- meta_df_c %>% mutate(run = 1)
      }

      validate(need('run' %in% colnames(meta_df_c),"Meta data file must include a 'run' column."))
      validate(need('cage_num' %in% colnames(meta_df_c),"Meta data file must include a 'cage_num' column."))
      incProgress(1/3, detail = 'Join with promethion file(s).')
      # join meta data and cage data; if missing meta then it's out
      df <- inner_join(all_cage, meta_df_c,
                       by = c('run','cage_num'))
      # output
      message('Finished combining meta data and promethion file.')
      print(head(df, 2))
      #df
    })


    ## start aggregation and force to ts type
    #req(input$aggregate_data)

    # Begin aggregations ----
    agg_by <- input$aggregate_data
    #print(agg_by)
    message('Aggregating data by ', agg_by)
    # only the minutes makes needed; cant do cut(time, '30 minutes')
    agg_breaks <- c('5 minutes'= '5 min', '30 minutes'='30 min', '1 hour'='1 hour', '6 hours'='6 hour')
    agg_col <- paste0('date_time_', gsub(' ','', agg_by))

    cumulative_metrics <- c('foodupa','waterupa','pedmeters','allmeters')

    ## if by anything other than 5 minutes then aggregate data and then find differences
    #if(agg_by != '5 minutes'){
    shinyWidgets::updateProgressBar(session, id = 'file_progress_bar', value = 70)
    withProgress(message = 'Aggregating file.', detail = 'This might take time.\n', value = 0, {
      agg_df <- df %>%mutate(date_time = as.POSIXct(date_time)) %>%
        arrange(date_time) %>%
        group_by(metric, run, cage_num) %>%
        # get aggregated time intervals;
        mutate(date_time = cut(date_time, agg_breaks[agg_by]),
               date_time = as.POSIXct(date_time))

      incProgress(1/3, detail = paste0('Breaking datetimes by ',agg_breaks[agg_by],'.'))
      # exclude missing

      agg_df <- agg_df %>%
        group_by(run, cage_num, metric) %>%
        # get start/end date_times after aggregation
        mutate(start_date =  min(date_time, na.rm = TRUE),
               end_date = max(date_time, na.rm = TRUE)) %>%
        ungroup() %>% arrange(metric,run, cage_num, date_time) %>%
        # now group by to calculate values
        group_by(date_time, metric, start_date, end_date,
                 file_num_uploaded, analysis_subject_id, across(colnames(meta_df_c))) %>%
        # cumulative values are going to use max; average everything else
        summarize(value = case_when(all(metric %in% cumulative_metrics) ~
                                      max(raw_n, na.rm=TRUE),
                                    TRUE ~ mean(raw_n, na.rm = TRUE))) %>%
        ungroup()
      incProgress(1/3, detail = 'Calculating differences.')

      agg_df <- agg_df %>% ungroup() %>%
        mutate(date_time = as.POSIXct(date_time), # this is unnecessary but if it's not it's going to be a mess
               # getting log values which will mostly be used for density plots later
               log_value = log1p(value)) %>%
        group_by(run, cage_num, analysis_subject_id,metric) %>%
        arrange(metric, run, cage_num, analysis_subject_id, date_time) %>%
        mutate(diff = value - lag(value),
               # because I need to make sure I use this and not log(diff(value))
               diff_log = log_value - lag(log_value))

      incProgress(1/3, detail = 'Finished with aggregation. Formatting columns.')
      ## format the columns for saving
      aggregated_df <- agg_df %>%
        ungroup() %>% arrange(metric, run, cage_num, analysis_subject_id, date_time) %>%
        mutate(aggregated_interval = agg_breaks[agg_by],
               date_time = as.POSIXct(date_time)) %>%
        mutate(across(contains('date'), as.character)) %>%
        mutate(across(is.numeric, round, digits = 4)) %>%
        relocate(value, diff, .after = metric) %>%
        relocate(analysis_subject_id, run, cage_num, .before = date_time)

      #change column name to match the aggregation level
      # rename_with(~agg_col, date_time_agg)
    })
    shinyWidgets::updateProgressBar(session, id = 'file_progress_bar', value = 90)

    # Update download columns selections ----
    metrics <- unique(aggregated_df$metric)

    shinyWidgets::updatePickerInput(session,
                                    "download_data_col",
                                    choices = c(metrics),
                                    selected = c(metrics)
    )


    message('Finished calculating aggregations and differences')
    # Assign to final_df variable
    final_df(aggregated_df)
  } # end else
  Sys.sleep(.1)
  shinyjs::hide(id='file_progress_bar')
  shinyjs::show(id='aggregate_data_btn')

  if('light_dark' %in% colnames(final_df())){
    shinyjs::show(id='data_download_header')
    shinyjs::show(id='download_full_data_btn')
    shinyjs::hide(id='download_prom_data_btn')
  }else{
    shinyjs::show(id='data_download_header')
    shinyjs::show(id='download_prom_data_btn')
    shinyjs::hide(id='download_full_data_btn')
  }
#
#   shinyjs::show(id='data_download_section')
#   shinyjs::show(id='download_prom_data_btn')
}, ignoreInit = TRUE)

observeEvent(input$aggregate_data_btn,{
  if(input$aggregate_data %in% c('5 minutes', '30 minutes')){
    shinyjs::show(id='phase_ui')
  } else{
    shinyjs::hide(id='phase_ui')
  }
}, ignoreInit = TRUE)

# add in light/dark phases calc_phases_btn ----
observeEvent(input$calc_phases_btn,{
  if(input$auto_file_selection == TRUE){
    shinyjs::html(id = 'start_light_lab', 'Set Light/Dark Phases? <small>The example data phases have been changed.</small>')
  } else{
    shinyjs::html(id = 'start_light_lab', 'Set Light/Dark Phases?')
  }

  shinyjs::hide(id = 'calc_phases_btn')
  shinyjs::hide(id='data_download_header')
  shinyjs::hide(id='download_full_data_btn')
  shinyjs::hide(id='download_prom_data_btn')
  #req(input$aggregate_data_btn > 0)
  message('Adding in light/dark phases')

  df <- final_df()

  req(input$start_light)
  req(input$end_light)


  start_light <- hms::as_hms(paste0(input$start_light,':00')) # inclusive
  end_light <- hms::as_hms(paste0(input$end_light,':00')) # make sure the user put the hour that the light stopped aka when did dark start

  withProgress(message = 'Adding in light/dark phases',value =0, {
    incProgress(1/3, detail = 'getting file')
    df_with_phases <- df %>%
      mutate(date_time = as.POSIXct(date_time),
             start_date = as.POSIXct(start_date)) %>%
      ungroup() %>%
      mutate(
        light_on = ifelse(
          ## time is start_light or greater
          (hms::as_hms(date_time) >= start_light) &
            ## but less than end light
            (hms::as_hms(date_time) < end_light),
          1, 0),
        ## will need this later
        start_in_light = ifelse((date_time == start_date) & (light_on ==1), 1, 0))
    incProgress(1/3, detail = 'still working')
    df_with_phases <- df_with_phases %>%
      group_by(run, cage_num, analysis_subject_id,metric) %>%
      arrange(run, cage_num, analysis_subject_id, metric, date_time) %>%
      ## this is a mess but it works...
      mutate(## fill in start with NA and replace it with 0; find difference between the current and lag light change; ex:
        # 1,0,0, 1+0=1,
        # 1,0,0, 1+0+0 = 1
        # 1,0,0, 1+0+0+0 = 1
        # 0,-1,0, 1+... = 0
        # 0,0,0,
        # 1,1,1, 1+1+0
        light_change = replace_na(c(NA, diff(light_on)),0),
        ## next ifelse only works if the starts with light on; if it starts with off then reverse signs
        #light_change = ifelse(start_in_light == 1, -light_change, light_change),

        total_phase_num =1+ cumsum(abs(light_change)),
        ## this will be used
        i = ifelse(light_change == 1, 1, 0),
        phase_num = 1+cumsum(i),
        phase_num = ifelse(any(start_in_light == 0) & light_on == 1, phase_num -1, phase_num),
        phase_num = ifelse((any(phase_num == 0) & light_on ==1),phase_num+1,phase_num)
      ) %>%
      ungroup() %>%
      ## zero question what the columns are; will use for plotting
      mutate(light_dark = ifelse(light_on == 1, 'light','dark'),
             phase = paste0(light_dark, ' phase ', phase_num))%>%
      ## don't need these anymore
      select(-light_change, - i, -start_in_light) %>%
      mutate(across(contains('date'), as.character)) %>%
      relocate(phase, .after =  value)

    incProgress(1/3, detail = 'finished')

  })

  ## replace final_df with prom with light/dark
  final_df(df_with_phases)
  shinyjs::show(id ='calc_phases_btn')

  if('light_dark' %in% colnames(final_df())){
    shinyjs::show(id='data_download_header')
    shinyjs::show(id='download_full_data_btn')
    shinyjs::hide(id='download_prom_data_btn')
  }else{
    shinyjs::show(id='data_download_header')
    shinyjs::show(id='download_prom_data_btn')
    shinyjs::hide(id='download_full_data_btn')
  }

  }, ignoreInit = TRUE)
}

# output head of table ----
output$all_prom_files <- DT::renderDataTable(head(final_df(),20),
                            caption = 'Only showing 20 rows of the data. Download will include all data')



# filter download data df ----
download_df <- reactive({
  df <- final_df()
  if(!is.null(df) & !is.null(input$download_data_col)){
    df %>% filter(metric %in% input$download_data_col)
  } else{
    return("No metrics selected")
  }
})

# download as csv without light/dark ----
output$download_prom_data_btn <- downloadHandler(
  filename = function() {
    paste("promethion_cleaned_",gsub(' ','',input$aggregate_data),"_", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(x = download_df(), file, row.names = FALSE)
  }
)
# download as csv with light/dark ----
output$download_full_data_btn <- downloadHandler(
  filename = function() {
    paste("promethion_cleaned_with_phases_",gsub(' ','',input$aggregate_data),"_", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(x = download_df(), file, row.names = FALSE)
  }
)


