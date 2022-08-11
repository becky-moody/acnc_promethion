#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(scipen = 999)

if (!require("pacman")) {
  install.packages("pacman") }
library(pacman)

pacman::p_load(shiny, tidyverse,here, janitor, shinyjs, readxl, DT, tools,shinyWidgets, update = FALSE)
pacman::p_unload(here, janitor, readxl, tools,shinyWidgets)
# library(shiny)
# library(tidyverse)
# library(here)
# library(janitor)
# library(shinyjs)
# library(readxl)
# library(DT)
# detach(here)
# detach(janitor)
# detach(readxl)
# detach(shinyWidgets)

time_selection <- format( seq.POSIXt(as.POSIXct('2021-01-01 00:00'),
                                     as.POSIXct('2021-01-01 23:59'), by = "30 min"),"%H:%M")

#rmarkdown::render(here::here("www/example_promethion.Rmd"))
note_html <- base64enc::dataURI(file = here::here("www/example_promethion.html"), mime = "text/html")


# Define UI for application that draws a histogram
shinyUI(
tagList(
  shinyjs::useShinyjs(),

  navbarPage('Promethion', id = 'promethion_app',

             source(here::here('tabs/data_editor_ui.R'), local = TRUE)$value,

tabPanel('Plot Data',
         sidebarPanel(width = 3,
####################################################-
                      # show correct header if there is data or not
                      div(id='no_data_warning',h2('Please upload data.')),
                      div(id='filter_tab_name',h2('Apply Filters')),
####################################################-
                      # choose column with subject ids
                      shinyWidgets::pickerInput(
                        inputId = 'which_column_subject_id',
                        label = 'Which column in data contains subject ids?',
                         choices =  character(0),
                          selected = character(0),
                          options = list(`actions-box` = TRUE,
                                         `live-search`=TRUE),
                          multiple = FALSE
                        ),
####################################################-
                      div(id = 'all_plot_filters',
          tags$hr(style="border-color: black;"),
          shinyWidgets::pickerInput(inputId = 'plot_animal_filter',
                                    label = HTML(paste(
                                      h4('Which subject(s)?',style = "display: inline;"))),
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
                                      h4('Which light/dark phase?',style = "display: inline;"))),
                                    choices =  character(0),
                                    selected = character(0),
                                    options = list(
                                      `actions-box` = TRUE,
                                      `live-search`=TRUE),
                                    multiple = TRUE
          ),
          tags$hr(style="border-color: black;")
          ),
####################################################-
          actionButton(
            inputId = "plot_selected",
            label = "Plot selected", width = '100%'),
        shinyWidgets::progressBar(id = "plot_filter_progress", value = 0, striped = TRUE),
          br()
), # end sidebarpanel


####################################################-
          mainPanel(width = 9,
                    plotOutput('filtered_prom_plot')
                    #plotOutput("distPlot")
          )


         ),# end tabpanel

####################################################-
tabPanel('Example',
         fluidPage(
         #htmltools::includeHTML(here::here('md/example_promethion.html'))
         htmltools::tags$iframe(src = note_html, width = '100%',  height = 1000,  style = "border:none;")
         )
         )

)# end navbarpage

) # end ui
)
