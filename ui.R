#### shiny::runGitHub(repo = 'acnc_promethion', username = 'becky-moody' ref = )
# bootstrap themes: https://bootswatch.com can run bslib::bs_theme_preview()

options(scipen = 999)

if (!require("pacman")) {
  install.packages("pacman") }
library(pacman)

## add github:
## add cran: ggridges

pacman::p_load(shiny, tidyverse,here, janitor, shinyjs, readxl, DT, tools,shinyWidgets, htmltools, plotly, bslib, thematic, update = FALSE)
pacman::p_unload(here, janitor, readxl, tools, shinyWidgets, htmltools, plotly)


time_selection <- format( seq.POSIXt(as.POSIXct('2021-01-01 00:00'),
                                     as.POSIXct('2021-01-01 23:59'), by = "30 min"),"%H:%M")

#rmarkdown::render(here::here("www/example_promethion.Rmd"))
note_html <- base64enc::dataURI(file = here::here("www/example_promethion.html"), mime = "text/html")

### themes don't work with shinywidgets and are breaking in weird ways, will probably have to manually change the css -__-
# my_theme <- bs_theme(bootswatch = "minty", secondary ='#9BCD9B'
#                      #base_font = font_google("Righteous")
#                      )
# thematic_shiny(font = "auto")
#
# Define UI for application that draws a histogram
shinyUI(

tagList(

  shinyjs::useShinyjs(),


  navbarPage('Promethion', id = 'promethion_app',#theme = my_theme,

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
