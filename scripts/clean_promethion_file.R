# This will clean the promethion files and add in metadata
# TO DO: Figure out how to standardize the metadata

###########################-
# Libraries -----
###########################-
if (!require("pacman")) {
  install.packages("pacman") }
library(pacman)

pacman::p_load(shiny, tidyverse, here, janitor, readxl, DT, update = FALSE)
pacman::p_unload(here, janitor, readxl)

###########################-
# Settings ----
###########################-
options(scipen = 999) # no scientific notation pls
set.seed(72316)

###########################-
# Variables ----
###########################-
## File names from input ----
# prom_file_paths <- c(here('data/b230122 run 1 friday (2)_m_calr.csv'),here('data/run2_5min.xlsx'))
# meta_file_path <- here('data/test_metadata_edit.xlsx')

# f1 <- here('data/b230122 run 1 friday (2)_m_calr.csv')
# f2 <- here('data/run2_5min.xlsx')
# f <- c(f1,f2)
# mf <- here('data/Grouping_promethion.xlsx')


## Variables needed for joining multiple promethion files ----
df_cage <- data.frame()
df_enviro <- data.frame()
file_dt <- data.frame()

## Variables needed for light/dark specification
# start_light <- hms::as_hms('7:15:00') # inclusive
# end_light <- hms::as_hms('19:00:00') # make sure the user put the hour that the light stopped aka when did dark start
# light_colors <- c(light = "#FFF68F", dark = "#8DEEEE")

###########################-
# Function: read.input.file ----
###########################-
read.input.file <- function(path){
  if (is.null(path))
    return(NULL)

  validate(
    need(tools::file_ext(path) %in% c(
      'text/csv',
      'text/comma-separated-values',
      'csv',
      'xlsx'
    ), "Wrong File Format try again! If .xml, open in Excel and save as .xlsx."))

 if(grepl('xlsx', path)){
   return(readxl::read_xlsx(path))
   } else if (grepl('csv', path)){
     return(read.csv(path))
   } else {
     return(NULL)
   }

}

###########################-
# function: clean.cage, fixes col names & pivot longer -----
###########################-
clean.cage <- function(cage_file){
  x <- cage_file %>% janitor::clean_names() %>%
    mutate(date_time_1 = as.POSIXct(date_time_1, format='%m/%d/%Y %H:%M:%S' ))

  if(any(str_detect(colnames(x),'animal'))){
    x <- x %>% pivot_longer(cols = -c(1,2)) %>%
      mutate(name = gsub('kcal_hr','kcalhr',name),
             name = gsub('still_pct', 'stillpct',name),
             name = gsub('sleep_pct','sleeppct',name))%>%
      # now separate into variable and cage number
      separate(name, into = c('var','cage_num'), sep = '_') %>%
      select(-cage_num)
  } else{
    # make long df
    pivot_longer(cols = -c(1:6)) %>%
      # separate won't work on this because there are 2 underscores
      mutate(name = gsub('kcal_hr','kcalhr',name)) %>%
      # now separate into variable and cage number
      separate(name, into = c('var','cage_num'), sep = '_') %>%
      mutate(cage_num = as.numeric(cage_num))
  }

}


###########################-
# Join multiple promethion files -----
###########################-
join.cage.files <- function(cage_paths){
  if(is.null(cage_paths)){
    return(NULL)
  }
for(i in 1:length(cage_paths)){

  ## check data file paths for xlsx/csv to match with correct read function
  # d <- if(grepl('xlsx',f[i])){
  #   readxl::read_xlsx(f[i])
  # } else{
  #   read.csv(f[i])
  # }
  d <- read.input.file(cage_paths[[i]])

  ## apply function and add in file_num
  dat <- clean.cage(d) %>%
    mutate(file_num = i)

  ## I don't know if the enviro variables change so going to remove and can add back in later if need; Asked Dan and he didn't know either
  ## enviro columns with the dt, cage, file_num
  env <- dat %>%
    select(date_time_1, cage_num, contains('enviro'), file_num)
  ## data cols and file num
  cg <- dat %>% select(!contains('enviro')) %>%
    ## if there is anything missing in here I don't really care it's getting removed; pivots will sometimes add in some weird NAs
    na.omit()
  ## get file info in case I need to refer to a certain one for something
  min_max_dt <- cg %>%
    group_by(file_num) %>%
    summarize(start_dt = min(date_time_1, na.rm = TRUE),
              end_dt = max(date_time_1, na.rm = TRUE))

  # append to outside global vars
  file_dt <- rbind(file_dt, min_max_dt)
  df_cage <- rbind(df_cage, cg)
  df_enviro <- rbind(df_enviro,env)

}

## clean variables from this ----
rm(dat, env, cg, min_max_dt,d)


# Add in run number ----
###########################-
## run number is by time
file_dt <- file_dt %>%
  arrange(start_dt) %>%
  mutate(run = row_number())
## add run to cage data; this should line up with metadata
df_cage <- left_join(df_cage, file_dt, by = 'file_num')

df_cage
}

# Clean metadata ----
###########################-

### I think this might need to be a forced layout; can be manually entered via DT in shiny or just make them do an excel/csv a certain way
# mf <- here('data/Grouping_promethion.xlsx')
# m_df_r <- if(grepl('xlsx',mf)){
#   readxl::read_xlsx(mf)
# } else{
#   read.csv(mf)
# # }
# m_df <- rbind(data.frame(m = m_df_r$`Run 1 Study B230122`, run= rep(1,16)),
#               data.frame(m = m_df_r$`Run 2 Study B230122`, run = rep(2,16))) %>%
#   separate(m, into = c('cage', 'cage_num','rat','animal_id','sex','x'), sep = ' ')
#
# m_df <- m_df %>%
#   # adjust for "no cage"
#   mutate(cage_num = ifelse(cage == 'No', rat, cage_num)) %>%
#   select(cage_num, animal_id, sex, x, run)


# Join to metadata ----
###########################-
## join cage and meta data
# df <- left_join(df_cage , m_df, by = c('cage_num', 'run')) %>%
#   ## remove anything where there is not an animal_id
#   drop_na(animal_id)


# Add in phase numbers ----
## moved to varaibles at top
# start_light <- hms::as_hms('7:15:00') # inclusive
# end_light <- hms::as_hms('19:00:00') # make sure the user put the hour that the light stopped aka when did dark start
# light_colors <- c(light = "#FFF68F", dark = "#8DEEEE")
##

add.phases <- function(df, start_light = '7:30:00', end_light='19:00:00'){
  start_light <- hms::as_hms(paste0(start_light,':00')) # inclusive
  end_light <- hms::as_hms(paste0(end_light,':00')) # make sure the user put the hour that the light stopped aka when did dark start
df_l <- df %>% #filter(var == 'vo2', cage_num ==1, run ==1) %>%
  ## ordering time doesn't really matter here
  arrange(date_time_1) %>%
  mutate(light_on = ifelse(
    ## time is start_light or greater
    (hms::as_hms(date_time_1) >= start_light) &
      ## but less than end light
      (hms::as_hms(date_time_1) < end_light),
    1, 0),
    ## will need this later
    start_in_light = ifelse(date_time_1 == start_dt, 1, 0)) %>%
  group_by(run, cage_num, subject_id, var) %>%
  arrange(run, cage_num, subject_id, var, date_time_1) %>%
  mutate(## fill in start with NA and replace it with 0; find difference between the current and lag light change; ex:
    # 1,0,0, 1+0=1,
    # 1,0,0, 1+0+0 = 1
    # 1,0,0, 1+0+0+0 = 1
    # 0,-1,0, 1+... = 0
    # 0,0,0,
    # 1,1,1, 1+1+0
    light_change = replace_na(c(NA, diff(light_on)),0),
    ## next ifelse only works if the starts with light on; if it starts with off then reverse signs
    light_change = ifelse(start_in_light== 1, light_change, -light_change),
    ## this will be used
    i = ifelse(light_change == -1, 1, 0),
    phase_num = 1+cumsum(i)
  ) %>%
  ungroup() %>%
  ## zero question what the columns are; will use for plotting
  mutate(light_dark = ifelse(light_on == 1, 'light','dark'),
         phase = paste0(light_dark, ' phase ', phase_num)) %>%
  ## don't need these anymore
  select(-light_change, - i, -start_in_light)
df_l
}


# aggregate notes
# top down times so 12:30 = 12:30 to 1:29
# vo2 - avg
# vh20 avg
# kcal - avg
# rq avg
# foodupa avg
# waterupa avg
# bodymass avg
# xbreak sum?? wont be exact because I think the already aggregated 5 mins is going to be missing a few data points
# ybreak sum
# zbreak sum
# pedmeteres avg
# allmeters avg but wtf why...average a cumulative sum
