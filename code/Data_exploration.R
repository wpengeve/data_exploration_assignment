library(tidyverse)
library(purr)
library(lubridate)

#Use list.files(), read_csv and bind_rows() to read and combine as data frame
list_filenames <- list.files(path="./data/", pattern="trends_up_to_", all.files=FALSE, full.names=TRUE)
df_list <- map(list_filenames, read_csv)
#for(i in 1:length(list_filenames)){
# read_csv(paste0("./data/", list_filenames[i]))
#}
df <- bind_rows(df_list)

#Read CollegeScorecardDatadictionary, id_name_link and MostRecentCohortsScorecardElements files
CollegeScorecardDatadictionary <- read_csv('./data/CollegeScorecardDatadictionary-09-08-2015.csv')
id_name_link <- read_csv('./data/id_name_link.csv')
Scorecard <- read_csv('./data/Most+Recent+Cohorts+(Scorecard+Elements).csv')

#Aggregate the Google Trends data
#Get the first ten characters from monthorweek, get the month from monthorweek and turn both results into date
df <- df %>% mutate(date = str_sub(monthorweek, 1, 10)) %>% mutate(date_usable = ymd(date)) %>% select(-date) %>% mutate(month = floor_date(date_usable, 'month'))

#Address variable-index by standardize it and then group_by, summarize it 
df1 <- df %>% group_by(schname,keyword) %>% mutate(index_standarized = (index - mean(index))/sd(index)) %>% na.omit() %>% group_by(schname, month) %>% summarize(total = sum(index_standarized))

#Merge in the Scorecard data
#Drop those schools sharing the same name
id_name_link <- id_name_link %>% group_by(schname) %>% mutate(n = n()) %>% filter(n==1) %>% select(-n)
#Join data frame together
final_df <- df %>% inner_join(id_name_link, 'schname') %>% inner_join(Scorecard, by = c("unitid" = "UNITID"))


                                                                    
                                                                    