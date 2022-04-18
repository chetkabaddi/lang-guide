### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ###              R   E   A   D          M    E              ### ### ### ### ### ###
### ### ### ### ### ###                                                         ### ### ### ### ### ###
### ### ### ### ### ###  *** no need to run all code chunks                     ### ### ### ### ### ###
### ### ### ### ### ###   ** sufficient to read the following csv files         ### ### ### ### ### ###
### ### ### ### ### ###       "plotting_data_missing_years_inserted.csv"        ### ### ### ### ### ###
### ### ### ### ### ###         "<complete_regional_data.csv>"                  ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###







##### PACKAGES AND OPTIONS #####
setwd("~/language/phase1/clean_install")
library(readr)
library(purrr)
library(dplyr)
library(tidyverse)
library(quanteda)
library(tidytext)
library(readr)
library(seastests)
options(scipen = 999)


##### DATASET ORGANISATION ####
##### LOAD MAIN DATAFILE (*file contains everything from 1991, all newspapers, and duplicates have been removed)
raw_data <- read_csv("FilteredDataset_RegionInfo_HS.csv", 
         col_types = cols(text_all = col_skip(),
                          text_select = col_skip(),
                          paper = col_factor(levels = c("guardian", 
                                                        "observer", "dailytelegraph", "sundaytelegraph", 
                                                        "dailymail", "sundaymail", "dailymirror", 
                                                        "sundaymirror", "times", "sundaytimes", 
                                                        "dailyexpress", "sundayexpress", 
                                                        "sun", "sunonsunday", "es", "ft")), 
                          month = col_factor(levels = c("January", 
                                                        "February", "March", "April", 
                                                        "May", "June", "July", "August", 
                                                        "September", "October", "November", 
                                                        "December")), date = col_date(format = "%d/%m/%Y"),
                          decade = col_factor(levels = c("1990s", 
                                                         "2000s", "2010s", "2020s")), 
                          context = col_factor(levels = c("text", 
                                                          "headline")), wc_title = col_integer(), 
                          wc_text = col_integer()))


##### DEVELOP USEFUL VALUES

#1# 11 newspapers used in phase 1. We have full archive access to these papers between 2001 and 2020
allpapers = c("observer", "sundaytelegraph", "sundaymail", "sundaymirror", "sundaytimes",
              "guardian", "dailytelegraph", "dailymail", "dailymirror", "times", "sun")

#2# Daily papers (n = 6)
weekpapers = c("guardian", "dailytelegraph", "dailymail", "dailymirror", "times", "sun")

#3# Sunday papers (n = 5)
sundaypapers = c("observer", "sundaytelegraph", "sundaymail", "sundaymirror", "sundaytimes", "sundaysun")

#4# Define timeframes
commonperiod1 = c("2001" : "2020") # period that phase 1 studies
commonperiod2 = c("2010": "2020") # period for which homelessness data is available
two_thousands = c("2001": "2009") # 2000s
twenty_tens = c("2010" : "2020") # 2010s


#5# Set a colour pallete
customcolours <- c('#9A6324', '#800000', '#e6194B', '#3cb44b', '#ffe119', '#4363d8','#aaffc3', "#9e1309", '#000075', '#f58231', '#911eb4') # 11 colours
customcolours1 <- c('#9A6324', '#800000', '#e6194B', '#3cb44b', '#ffe119', '#4363d8') # 6 colours for weekpapers
customcolours2 <- c('#aaffc3', "#9e1309", '#000075', '#f58231', '#911eb4') # 5 colours for sunday papers

### ### ### ### ### ### ### ### ### ### ### ###
##### CREATE DATA FILEs TO USE IN PHASE 1 #####
### ### ### ### ### ### ### ### ### ### ### ###

#1# Data phase 1 files contains the 11 papers between 2001 and 2020
          data_phase1 <- raw_data %>% 
            filter(paper %in% allpapers) %>% 
            filter(year %in% commonperiod1)

#2# PLOTTING DATA file is required to plot values for newspapers for years when no news report was published
#(a)  
          raw_data %>% 
            filter(paper %in% allpapers) %>% 
            filter(year %in% commonperiod1) %>% 
            group_by(year, newspaper, paper, context) %>%
            count() %>% 
            write.csv(file = "plotting_data.csv") # this file is edited manually to create <plotting_data_missing_values_inserted.csv>

#(b)
plotting_data_missing_years_inserted <- read_csv("plotting_data_missing_years_inserted.csv")

#3# PLACE MENTION DATA FILES
          place_data <- data_phase1 %>% 
            select(!c(text_title, text_context, wc_title, wc_text, `homeless*`, doc_id, paper_id, inner_london, outer_london, wales)) %>% 
            filter(has_cities == "TRUE") %>% 
            filter(context == "headline")
        
        #East Midlands
                  em_mentions <- place_data %>% 
                    select(date, year, newspaper, east_midlands) %>%
                    filter(east_midlands > 0) %>%
                    group_by(year) %>% # if <group_by(year, newspaper)> is used here, we also see the number of unique articles published by each paper
                    tally()%>% 
                    mutate(region = rep("East Midlands"))
        # East of England         
                  eoi_mentions <- place_data %>% 
                    select(date, year, newspaper, east_of_england) %>%
                    filter(east_of_england > 0) %>%
                    group_by(year) %>% # if <group_by(year, newspaper)> is used here, we also see the number of unique articles published by each paper
                    tally()%>% 
                    mutate(region = rep("East of England"))
        # London          
                  london_mentions <- place_data %>% 
                    select(date, year, newspaper, london) %>%
                    filter(london > 0) %>%
                    group_by(year) %>% # if <group_by(year, newspaper)> is used here, we also see the number of unique articles published by each paper
                    tally()%>% 
                    mutate(region = rep("London"))
        # North East          
                  ne_mentions <- place_data %>% 
                    select(date, year, newspaper, north_east) %>%
                    filter(north_east > 0) %>%
                    group_by(year) %>% # if <group_by(year, newspaper)> is used here, we also see the number of unique articles published by each paper
                    tally()%>% 
                    mutate(region = rep("North East"))
        # North West          
                  nw_mentions <- place_data %>% 
                    select(date, year, newspaper, north_west) %>%
                    filter(north_west > 0) %>%
                    group_by(year) %>% # if <group_by(year, newspaper)> is used here, we also see the number of unique articles published by each paper
                    tally()%>% 
                    mutate(region = rep("North West"))
        # South East          
                  se_mentions <- place_data %>% 
                    select(date, year, newspaper, south_east) %>%
                    filter(south_east > 0) %>%
                    group_by(year) %>% # if <group_by(year, newspaper)> is used here, we also see the number of unique articles published by each paper
                    tally()%>% 
                    mutate(region = rep("South East"))
        # South West          
                  sw_mentions <- place_data %>% 
                    select(date, year, newspaper, south_west) %>%
                    filter(south_west > 0) %>%
                    group_by(year) %>% # if <group_by(year, newspaper)> is used here, we also see the number of unique articles published by each paper
                    tally()%>% 
                    mutate(region = rep("South West"))
        # West Midlands          
                  wm_mentions <- place_data %>% 
                    select(date, year, newspaper, west_midlands) %>%
                    filter(west_midlands > 0) %>%
                    group_by(year) %>% # if <group_by(year, newspaper)> is used here, we also see the number of unique articles published by each paper
                    tally()%>% 
                    mutate(region = rep("West Midlands"))
        # Yorkshire & the Humber          
                  yh_mentions <- place_data %>% 
                    select(date, year, newspaper, yorkshire_humber) %>%
                    filter(yorkshire_humber > 0) %>%
                    group_by(year) %>% # if <group_by(year, newspaper)> is used here, we also see the number of unique articles published by each paper
                    tally()%>% 
                    mutate(region = rep("Yorkshire and the Humber"))

# COMBINE REGION FILES AND WRITE THEM AS A CSV 
region_mention_data <- rbind(em_mentions, eoi_mentions, london_mentions, ne_mentions, nw_mentions, se_mentions, sw_mentions, wm_mentions, yh_mentions)
write.csv(region_mention_data, file = "region_mention_data.csv") # this file was manually edited to insert missing years for East Midlands and North East

# ^^^^^^^^^^^^^^^^ <region_mention_data.csv> was manually edited to also add homelessness statistic and saved as a new file

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# ^^^^^^^^^^^^^^^^ NAME OF NEW FILE = <region_mention_data_missing_years_inserted_homeless_stats_added.csv>
# THIS FILE WILL BE USED FOR ALL HOMELESSNESS STATISTICS BASED CALCULATIONS
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

complete_regional_data <- read_csv("region_mention_data_missing_years_inserted_homeless_stats_added.csv", 
                                   col_types = cols(region = col_factor(levels = c("East Midlands", 
                                                                                   "East of England", "London", "North East", 
                                                                                   "North West", "South East", "South West",
                                                                                   "West Midlands", "Yorkshire and the Humber"))))

complete_regional_data <- complete_regional_data %>% 
  mutate(`Attention Score` = `stories about region`/`street homelessness (per million)`) # Attention score shows the number of focus articles published per homeless person per 100 thousand population 


tidy_complete_regional_data <- complete_regional_data %>% 
  select(!X1) %>% 
  pivot_longer(!c(region, year),
               names_to = "variable",
               values_to = "value")

##### Create FIGUREs 1 - 4 <plotting_data_missing_years_inserted.CSV> FILE ####

plotting_data_missing_years_inserted %>%
  filter(context == "headline") %>% ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~CHANGE HERE FOR MENTIONS/HEADLINES
  filter(paper %in% sundaypapers) %>% ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~CHANGE HERE FOR DAILY/SUNDAY
  group_by(newspaper, year) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = newspaper), size = 2)+
  scale_color_manual(values = customcolours2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0,0))+
  xlab ("Year")+
  ylab ("Number of unique stories")+ ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CHANGE ylab HERE
  ggtitle("Focus stories on homelessness in Sunday newspapers (2001 - 2020)")+ ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CHANGE TITLE OF THE PLOT HERE
  theme_dark()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


##### CALCULATIONS OF AVERAGE NUMBER OF STORIES IN EACH DECADE ####

# create file for supplementary material
        plotting_data_missing_years_inserted %>% 
          filter(context == "text") %>% ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~CHANGE HERE FOR MENTIONS/HEADLINES
          group_by(year, newspaper) %>% 
          summarise(number_of_stories = n()) %>% 
          write.csv(., file = "incidental_stories_allpapers_(2001-2020).csv")### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~CHANGE HERE FOR FILENAME

# Produce TABLE 1
        plotting_data_missing_years_inserted %>% 
          filter(context == "headline") %>% ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~CHANGE HERE FOR MENTIONS/HEADLINES
          group_by(year) %>% ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~GROUP BY YEAR ONLY for Table 1
          tally(n)

# calculate average articles in each decade
        plotting_data_missing_years_inserted %>% 
          filter(paper %in% sundaypapers) %>% ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~CHANGE HERE for Daily/Sunday
          filter(context == "text") %>% ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~CHANGE HERE for mentions/headline
          filter(year %in% two_thousands) %>% ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~CHANGE HERE for 2000s and 2010s
          summarise(average = mean(n))

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#### REGIONAL DISPARITIES IN NEWSPAPER ATTENTION TO HOMELESSNESS ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# (FIGURE 5)
        tidy_complete_regional_data %>%
          filter(variable == "stories about region") %>% 
          ggplot(aes(x = year, y = value))+
          geom_line(aes(color = region), size = 2)+
          scale_color_manual(values = customcolours) +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
          ylab("Number of focus articles mentioning a place from the region")+
          xlab("Year")+
          ggtitle("Attention to regions of England", subtitle = "2001 - 2020")+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          theme_dark()

# (FIGURE 6)        
        tidy_complete_regional_data %>% 
          filter(variable == "stories about region") %>% 
          ggplot(aes(fill = region, y = value, x = year))+
          geom_bar(position = "fill", stat = "identity")+
          scale_fill_manual(values = customcolours)+
          ylab("Proportion share")+
          xlab("Year")+
          ggtitle("Share of press attention", subtitle = "2001 - 2020")+
          theme_classic()

# (FIGURE 7)     
        tidy_complete_regional_data %>%
          filter(variable == "Attention Score") %>% 
          filter(year %in% commonperiod2) %>%
          group_by(year) %>% 
          mutate(nat_av = mean(value)) %>% 
          ggplot(aes(x = year, y = value))+
          geom_line(aes(color = region), size = 2)+
          geom_line(aes(x = year, y = nat_av), size = 1, linetype = 5)+
          scale_color_manual(values = customcolours) +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
          facet_wrap(~region)+
          ylab("Attention Score")+
          xlab("Year")+
          ggtitle("Yearly attention score relative to the national average (2010 - 2020)", subtitle = "Dashed line is the rolling national average")+
          theme_dark()+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# SEASONALITY
        
data_phase1 %>% 
  filter(context == "headline") %>% 
  group_by(month) %>% 
  tally()
        
        
data_phase1 %>% 
  filter(paper == "sundaytelegraph") %>% 
  filter(context == "headline") %>% 
  filter(year == "2019") %>% 
  tally()
  
complete_regional_data %>% 
group_by(region) %>% 
tally(`stories about region`) %>% 
  mutate(xxx = 708/n)


tidy_complete_regional_data %>%
  filter(!region == "London") %>% 
  filter(variable == "street homelessness") %>% 
  filter(year %in% commonperiod2) %>%
  #group_by(region) %>% 
  summarise(average = mean(value))
