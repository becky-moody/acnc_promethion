tabPanel(
  title = 'Data Editor',

  #fluidPage(


  # Application title
  #titlePanel("Promethion Data Editor"),

  # Sidebar with a slider input for number of bins
   sidebarLayout(
  sidebarPanel(width = 3,
               div(id='upload_tab_name',h2('Upload Files')),
               # div(style = "margin:auto",
               checkboxInput('auto_file_selection','Use example file.', value = FALSE),
               div(id = 'auto_file_notes',
                   htmltools::HTML(('To upload this file, click the "upload files and aggregate" button.</br>The example file is data broken into 5 minute intervals with the light phase going from 7:00 to 19:00 (7:00 pm).</br>This can be aggregated to a different interval and/or the phase times changed if desired.</br>After upload, can go straight to other tabs.'))),
               ## promethion files
               fileInput("prom_file",
                         HTML(paste0(
                           h3("Choose Promethion Files"),
                           h6(em("If file is .xml, open in Excel and save the 'CalR' sheet as .xlsx or .csv."), style = "font-size:12px;"))),
                         accept = c(
                           # "text/csv",
                           # "text/comma-separated-values,text/plain",
                           ".csv",
                           '.xlsx'),
                         multiple = TRUE
               ),


               fileInput("meta_file",
                         HTML(paste0(
                           h3("Choose Study Metadata File"),
                           h6(em("This file must be formatted and contain 'cage number' and 'run' or 'run number'"),style = "font-size:12px;")
                         )),
                         accept = c(
                           # "text/csv",
                           # "text/comma-separated-values,text/plain",
                           ".csv",
                           '.xlsx'),
                         multiple = FALSE
               ),
               selectInput(
                 inputId = 'which_column_subject_id1',
                 label = h4('Which column in meta data contains subject ids?',id='sub_col_lab1'),
                 choices =  character(0),
                 selected = character(0),
                 # selectize = TRUE,
                 # options = list(`actions-box` = TRUE,
                 #                #`live-search`=TRUE
                 #                ),
                 multiple = FALSE
               ),
               # actionButton(
               #   inputId = "upload_files_btn",
               #   label = "Upload files?", width = '100%'),
               #), # end div

               br(),
               selectInput(inputId = 'aggregate_data',
                           label = 'Aggregate data? (Currently every 5 minutes)',
                           choices = c('5 minutes', '30 minutes',
                                       '1 hour', '6 hours'),
                           selected = c('5 minute'),
                           multiple = FALSE
               ),
               actionButton(
                 inputId = "aggregate_data_btn",
                 label = "Upload files and aggregate?", width = '100%'),
               shinyWidgets::progressBar(id = "file_progress_bar", value = 0, striped = TRUE),
               br(),

               div(id = 'phase_ui',
               tags$hr(style="border-color: black;"),

               h3('Set Light/Dark Phases.',id = 'start_light_lab'),
               selectInput(inputId = 'start_light',
                           label = 'What time was the light turned on?',
                           choices = time_selection,
                           selected = c('07:00'),
                           multiple = FALSE,
                           #options = list(style = 'background-color:#FFF68F;') # I cant get this to work will need to set css
                           #choicesOpt=list(rep_len('background:#FFF68F;',length(time_selection)))
               ),
               selectInput(inputId = 'end_light',
                           label = 'What time was the light turned off?',
                           choices = time_selection,
                           selected = c('19:00'),
                           multiple = FALSE,
                           #choicesOpt = list(style = rep_len('background:#8DEEEE;',length(time_selection)))
               ),
               actionButton(
                 inputId = "calc_phases_btn",
                 label = "Calculate light/dark phases?",
                 width = '100%'),
               br(),

               tags$hr(style="border-color: black;"),
),

div(id='data_download_header',
h3('Download Data.'),
               shinyWidgets::pickerInput(inputId = 'download_data_col',
                                         label = HTML(paste(
                                           h4('Which metrics?',style = "display: inline;"))),
                                         choices =  character(0),
                                         selected = character(0),
                                         options = list(
                                           `actions-box` = TRUE,
                                           `live-search`=TRUE),
                                         multiple = TRUE
                                         #multiple = TRUE,
                                         #choicesOpt = list(style = rep_len('background:#8DEEEE;',length(time_selection)))
               )),
               downloadButton("download_prom_data_btn", "Download file without phases?", width = '100%'),
               downloadButton("download_full_data_btn", "Download file with phases?",width = '100%'),
br()
  ),# end sidebarPanel


  mainPanel(width = 9,
            DT::dataTableOutput('all_prom_files')
            #plotOutput("distPlot")
  )

  ) # end sidebarlayout
) # end tabPanel
