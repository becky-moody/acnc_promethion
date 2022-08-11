#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(scipen = 999)

# if (!require("pacman")) {
#   install.packages("pacman") }
# library(pacman)
#
# pacman::p_load(shiny, tidyverse,here, janitor, shinyjs, readxl, DT, tools,shinyWidgets, update = FALSE)
# pacman::p_unload(here, janitor, readxl, tools,shinyWidgets)
library(shiny)
library(tidyverse)
library(here)
library(janitor)
library(shinyjs)
library(readxl)
library(DT)
library(htmltools)
detach(here)
detach(janitor)
detach(readxl)
detach(shinyWidgets)

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
             source(here::here('tabs/plotting_ui.R'), local = TRUE)$value,


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
