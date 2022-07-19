#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if (!require("pacman")) {
  install.packages("pacman") }
library(pacman)

pacman::p_load(shiny, tidyverse,here, janitor, shinyjs, readxl, DT, tools, update = FALSE)
pacman::p_unload(here, janitor, readxl, tools)
# library(shiny)
# library(tidyverse)
# library(here)
# library(janitor)
# library(shinyjs)
# library(readxl)
# library(DT)

time_selection <- format( seq.POSIXt(as.POSIXct('2021-01-01 00:00'),
                                     as.POSIXct('2021-01-01 23:59'), by = "30 min"),"%H:%M")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  useShinyjs(),
    # Application title
    titlePanel("Promethion Data Editor"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(width = 3,
                   div(style = "margin:auto",
                   ## promethion files
                   fileInput("prom_file",
                             HTML(paste(
                               h3("Choose Promethion Files"),
                               h6("If file is .xml, open in Excel and save as .xlsx.", style = "font-size:12px;"))),
                             accept = c(
                               # "text/csv",
                               # "text/comma-separated-values,text/plain",
                               ".csv",
                               '.xlsx'),
                             multiple = TRUE
                   ),


                   fileInput("animal_file",
                        HTML(paste(
                   h3("Choose Study Metadata File"),
                   h6(em('This file must be formatted.'),style = "font-size:12px;")
                   )),
                             accept = c(
                               # "text/csv",
                               # "text/comma-separated-values,text/plain",
                               ".csv",
                               '.xlsx'),
                             multiple = FALSE
                   ),
                   br(),
                   selectInput(inputId = 'agg_data',
                    label = 'Aggregate data? (Currently every 5 minutes)',
                               choices = c('5 min' = 5, '30 min'=30,
                                           '1 hr'= 60, '5 hrs'= 300),
                               selected = c('5 min'),
                               multiple = FALSE
                   ),
                   actionButton(
                     inputId = "upload_files_btn",
                     label = "Upload files?", width = '100%'),
                   ),

                   ## line
                   tags$hr(style="border-color: black;"),


        shinyjs::hidden(p(id = "st_lg",
                   selectInput(inputId = 'start_light',
                          label = 'What time was the light turned on?',
                               choices = time_selection,
                               selected = c('07:00'),
                               multiple = FALSE,
                               #options = list(style = 'background-color:#FFF68F;') # I cant get this to work will need to set css
                               #choicesOpt=list(rep_len('background:#FFF68F;',length(time_selection)))
                   ))),
        shinyjs::hidden(p(id = "end_lg",
                          selectInput(inputId = 'end_light',
                               label = 'What time was the light turned off?',
                               choices = time_selection,
                               selected = c('19:00'),
                               multiple = FALSE,
                               #choicesOpt = list(style = rep_len('background:#8DEEEE;',length(time_selection)))
                   ))),
      shinyjs::hidden(p(id = "ph",
                        br(),
                        actionButton(
                     inputId = "calc_phases",
                     label = "Calculate light/dark phases?",
                     width = '100%'),
                   br())),

                   shinyjs::hidden(p(id = "dl",
                    br(), actionButton("download_data", "Download files?")
                   ))
      ),
        # Show a plot of the generated distribution
        mainPanel(width = 9,
            #plotOutput("distPlot")
        )
    )
))
