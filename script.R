# https://cocvd.med.uky.edu/sites/default/files/Sable%20Promethion%20system.pdf
# https://calrapp.org

if (!require("pacman")) {
  install.packages("pacman") }
library(pacman)

#pacman::p_load(shiny, tidyverse,here, janitor, shinyjs, readxl, DT, update = FALSE)
#pacman::p_unload(here, janitor, readxl)

library(tidyverse)
library(here)
library(janitor)
# library(readxl)

options(scipen = 999) # no scientific notation pls
set.seed(72316)

f1 <- here('data/b230122 run 1 friday (2)_m_calr.csv')
f2 <- here('data/run2_5min.xlsx')
f <- c(f1,f2) ### change to length input$datapaths

clean.cage <- function(cage_df){
  x <- cage_df %>% janitor::clean_names() %>%
    mutate(date_time_1 = as.POSIXct(date_time_1, format='%m/%d/%Y %H:%M:%S' )) %>%
  # make long df
  pivot_longer(cols = -c(1:6)) %>%
    # separate won't work on this because there are 2 underscores
    mutate(name = gsub('kcal_hr','kcalhr',name)) %>%
    # now separate into variable and cage number
    separate(name, into = c('var','cage_num'), sep = '_')
}

# cage data ----
df_cage <- data.frame()
df_enviro <- data.frame()
file_dt <- data.frame()

for(i in 1:length(f)){
  ## check data file paths for xlsx/csv to match with correct read function
  d <- if(grepl('xlsx',f[i])){
    readxl::read_xlsx(f[i])
    } else{
    read.csv(f[i])
      }

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

  file_dt <- rbind(file_dt, min_max_dt)
  df_cage <- rbind(df_cage, cg)
  df_enviro <- rbind(df_enviro,env)

}
rm(dat, env, cg, min_max_dt,d)

## run number is by time
file_dt <- file_dt %>%
  arrange(start_dt) %>%
  mutate(run = row_number())
## add run to cage data; this should line up with metadata
df_cage <- left_join(df_cage, file_dt, by = 'file_num')


### I think this might need to be a forced layout; can be manually entered via DT in shiny or just make them do an excel/csv a certain way
mf <- here('data/Grouping_promethion.xlsx')
m_df_r <- if(grepl('xlsx',mf)){
  readxl::read_xlsx(mf)
} else{
  read.csv(mf)
}

m_df <- rbind(data.frame(m = m_df_r$`Run 1 Study B230122`, run= rep(1,16)),
              data.frame(m = m_df_r$`Run 2 Study B230122`, run = rep(2,16))) %>%
  separate(m, into = c('cage', 'cage_num','rat','animal_id','sex','x'), sep = ' ')

m_df <- m_df %>%
  # adjust for "no cage"
  mutate(cage_num = ifelse(cage == 'No', rat, cage_num)) %>%
  select(cage_num, animal_id, sex, x, run)


## join cage and meta data
df <- left_join(df_cage , m_df, by = c('cage_num', 'run')) %>%
  ## remove anything where there is not an animal_id
  drop_na(animal_id)


start_light <- hms::as_hms('7:15:00') # inclusive
end_light <- hms::as_hms('19:00:00') # make sure the user put the hour that the light stopped aka when did dark start
light_colors <- c(light = "#FFF68F", dark = "#8DEEEE")

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
  group_by(run, cage_num, animal_id, var) %>%
  arrange(run, cage_num, animal_id, var, date_time_1) %>%
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




#### to do
# length of number of files uploaded will correspond to run #
## create a file_num col and then use minimum dt of that col as the run# that will match to meta data; this is incase the order files were selected is not the order of the actual runs

