tabPanel('Plot Data',
         sidebarPanel(width = 3,
                      ####################################################-
                      # show correct header if there is data or not
                      div(id='no_data_warning',h2('Please upload data.')),
                      div(id='filter_tab_name',h2('Apply Filters')),
                      ####################################################-
                      # choose column with subject ids
                      # shinyWidgets::pickerInput(
                      #   inputId = 'which_column_subject_id',
                      #   label = 'Which column in data contains subject ids?',
                      #   choices =  character(0),
                      #   selected = character(0),
                      #   options = list(`actions-box` = TRUE,
                      #                  `live-search`=TRUE),
                      #   multiple = FALSE
                      # ),
                      selectInput(
                        inputId = 'which_column_subject_id',
                        label = h4('Which column in data contains subject ids?',id='sub_col_lab'),
                        choices =  character(0),
                        selected = character(0),
                        # selectize = TRUE,
                        # options = list(`actions-box` = TRUE,
                        #                #`live-search`=TRUE
                        #                ),
                        multiple = FALSE
                      ),
                      ####################################################-
                      div(id = 'all_plot_filters',
                          tags$hr(style="border-color: black;"),
                          shinyWidgets::pickerInput(inputId = 'plot_animal_filter',
                                                    label = HTML(paste(
                    h4('Which subject(s)?',style = "display: inline;"),p(em('Plotting over time might become slow with many subjects.'), style = 'display: inline;'))),
                                                    choices =  character(0),
                                                    selected = character(0),
                                                    options = list(
                                                      `actions-box` = TRUE,
                                                      `live-search`=TRUE),
                                                    multiple = TRUE
                          ),

                          ## this update dropdown is in data_editor_server.R
                          shinyWidgets::pickerInput(inputId = 'plot_metric_filter',
                                                    label = HTML(paste(
                                                      h4('Which metric?',style = "display: inline;"),
                                                      h6(em("'_diff' metrics are the change between time."),style = "display: inline;"))),
                                                    choices =  character(0),
                                                    selected = character(0),
                                                    options = list(
                                                      `actions-box` = TRUE,
                                                      `live-search`=TRUE),
                                                    multiple = FALSE
                          ),
                          shinyWidgets::pickerInput(inputId = 'plot_phase_filter',
                                                    label = HTML(paste(
                                                      h4('Which light/dark phase?', id = 'phase_filter_lab'))),
                                                    choices =  character(0),
                                                    selected = character(0),
                                                    options = list(
                                                      `actions-box` = TRUE,
                                                      `live-search`=TRUE),
                                                    multiple = TRUE
                          ),
                        #textOutput(outputId='light_on_off_note'),
                          br(),
                          actionButton(inputId= 'filter_data_for_plot', label = 'Filter data?',
                                       width='100%'),
                          tags$hr(style="border-color: black;")
                      ),
                      ####################################################-

radioButtons(inputId = 'plot_type', label = h4('What type of plot?'),
             choices = list('Line over time' = 'plot_ts', 'Boxplot (transformation: ln(x+1))' = 'plot_boxplot')),
actionButton(inputId='run_plot', label = 'Plot the data.', width = '100%'),

                      shinyWidgets::progressBar(id = "plot_filter_progress", value = 0, striped = TRUE),

                      br(),
                      div(id = 'outliers_plot',
                          tags$hr(style="border-color: black;"),
                          h4('Outliers'),
                          p("This is used to quickly adjust the plots by removing/imputating 'outliers' to allow for a generalized observation of the data. In an analysis, outlier detection and handling <b>must be justified</b>."),
                          p('Details on calculations found in the Example tab.'),
                          radioButtons(inputId = 'calc_outliers', label = h5('Calculate Outliers by:'),
                                       choices = list('Removing' = 'remove_outliers',
                                                      'Imputing'='impute_outliers',
                                                      'Time series'='ts_outliers')),
                          actionButton(
                            inputId = 'calculate_outliers',
                            label = 'Calculate outliers and redo plot.',
                            width = '100%'
                          )


                          )
         ), # end sidebarpanel


         ####################################################-
         mainPanel(width = 9,
                   conditionalPanel(
                     condition = "input.plot_type == 'plot_ts'",
                     plotly::plotlyOutput('filtered_prom_ts_plot')
                   ),
                   conditionalPanel(
                     condition = "input.plot_type == 'plot_boxplot'",
                     plotOutput('filtered_prom_boxplot')
                   )

                   #plotOutput('filtered_prom_plot')
                   #plotly::plotlyOutput('filtered_prom_plot')
         )


)# end tabpanel
