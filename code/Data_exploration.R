library(tidyverse)
library(lubridate)
library(ggplot2)
library(fixest)
#install.packages("ggforce")
library(ggforce)
#install.packages("zoom")   # Install zoom package
library("zoom")  

#Use list.files(), read_csv and bind_rows() to read and combine as data frame.
list_filenames <- list.files(path="./data/", pattern="trends_up_to_", all.files=FALSE, full.names=TRUE)
df_list <- map(list_filenames, read_csv)
#for(i in 1:length(list_filenames)){
# read_csv(paste0("./data/", list_filenames[i]))
#}
df <- bind_rows(df_list)

#Read CollegeScorecardDatadictionary, id_name_link and MostRecentCohortsScorecardElements files.
CollegeScorecardDatadictionary <- read_csv('./data/CollegeScorecardDatadictionary-09-08-2015.csv')
id_name_link <- read_csv('./data/id_name_link.csv')
Scorecard <- read_csv('./data/Most+Recent+Cohorts+(Scorecard+Elements).csv')

#Aggregate the Google Trends data
#Get the first ten characters from monthorweek, get the month from monthorweek and turn both results into date
df <- df %>% mutate(date = str_sub(monthorweek, 1, 10)) %>% mutate(date_usable = ymd(date)) %>% select(-date) %>% mutate(month_of_record = floor_date(date_usable, 'month'))

#Address variable-index by standardize it and then group_by, summarize it
#Dataframe now is at schname and month level
df1 <- df %>% group_by(schname,keyword) %>% mutate(index_standardized = (index - mean(index))/sd(index)) %>% na.omit() %>% group_by(schname, month_of_record) %>% summarize(mean_index_standardized = mean(index_standardized))

#TEST###Observing within-group, calculating earnings, interests, demeaning.
#df1 <- df %>% group_by(schname,keyword) %>% mutate(index_standardized = (index - mean(index))/sd(index)) %>% na.omit() %>% 
#  group_by(schname) %>% mutate(mean_index_standardized = mean(index_standardized)) %>% mutate(demeaned_index = index_standardized - mean_index_standardized) %>%
#  group_by(schname, month_of_record) %>% summarize(mean_index_standardized = mean(demeaned_index))

#Filter out all the schools that predominantly grant bachelor's degree
Scorecard <- Scorecard %>% filter(PREDDEG == 3)

#For the calculate purpose, replace PrivacySuppressed and Null in md_earn_wne_p10-REPORTED-EARNINGS column into 0.
Scorecard$'md_earn_wne_p10-REPORTED-EARNINGS'[Scorecard$'md_earn_wne_p10-REPORTED-EARNINGS' == "PrivacySuppressed"] <- 0
Scorecard$'md_earn_wne_p10-REPORTED-EARNINGS'[Scorecard$'md_earn_wne_p10-REPORTED-EARNINGS' == "NULL"] <- 0

#Create a new column by transforming the data type of md_earn_wne_p10-REPORTED-EARNINGS variable
Scorecard<-transform(Scorecard, earnings = as.numeric(Scorecard$'md_earn_wne_p10-REPORTED-EARNINGS'))

#Calculate the Max, Min, Mean, 10th percentile, 25th percentile, 50th percentile, 90th percentile of the median earning of student working and not enrolled 10 years after entry.
#In order to define high-earning and low-earning colleges.
max_earning <- max(Scorecard$earnings) 
min_earning <- min(Scorecard$earnings)
mean_earning <- mean(Scorecard$earnings)
quantile(Scorecard$earnings, probs = c(0.1, 0.25, 0.75, 0.9, 1))
earning_29 <- quantile(Scorecard$earnings, probs = c(0.29))
earning_79 <-quantile(Scorecard$earnings, probs = c(0.79))

#Merge in the Scorecard data.
#Drop those schools sharing the same name.
id_name_link <- id_name_link %>% group_by(schname) %>% mutate(n = n()) %>% filter(n==1) %>% select(-n)

#Join data frame together
final_df <- df1 %>% inner_join(id_name_link, 'schname') %>% inner_join(Scorecard, by = c("unitid" = "UNITID")) %>% select(-c(OPEID, opeid6, INSTNM)) %>% filter(earnings >= earning_79 | earnings <= earning_29) %>% 
  mutate(earning_category = case_when(earnings >= earning_79 ~ '1', earnings <= earning_29 ~ '0')) %>%
  mutate(scorecard_implemented = case_when(month_of_record >= '2015-09-01' ~ '1', TRUE ~ '0'))

#Trying to see how the data looks like in graph-Explore with month_of_record and mean_index_standardized
ggplot(final_df, aes(x=month_of_record, y=mean_index_standardized)) + geom_point() + geom_smooth(method = 'lm')
m3 <- feols(mean_index_standardized~month_of_record, data = final_df)

#Split out those schools considered having high-earning and low-earning, group by month.
#In this assignment I consider high-earning as 79th percentile of earning and low-earning as 29th percentile of earning.
#Run regression model and plot the month_of_record and mean of the standardized index to see the relationship
#From the regression result and graph for both high-earning dataframe and low-earning dataframe we can see there is negative relationship between the month and search interest
high_earning_df <- final_df %>% filter(earnings >= earning_79)
ggplot(high_earning_df, aes(x=month_of_record, y=mean_index_standardized)) + geom_point() + geom_smooth(method = 'lm')
m1 <- feols(mean_index_standardized~month_of_record, data = high_earning_df)

low_earning_df <- final_df %>% filter(earnings <= earning_29)
ggplot(low_earning_df, aes(x=month_of_record, y=mean_index_standardized)) + geom_point() + geom_smooth(method = 'lm')
m2 <- feols(mean_index_standardized~month_of_record, data = low_earning_df)
etable(m1, m2)

#Filter and keep only earnings categorized as high and low earnings in the dataframe
#Run regression model and plot the month_of_record and mean of the standardized index to see the relationship
#From the regression result and graph we can see there is negative relationship between the month and search interest
final_df_high_low <- final_df %>% filter(earnings >= earning_79 | earnings <= earning_29)
ggplot(final_df_high_low, aes(x=month_of_record, y=mean_index_standardized)) + geom_point() + geom_smooth(method = 'lm')
m4 <- feols(mean_index_standardized~month_of_record, data = final_df_high_low)
etable(m3, m4)

#Plot high_earning_df and low_earning_df in same plot to see how data distributed
#From the graph we can see that after Sep, 2015 there are some higher searching interests in high-earning schools(higher than before Sep, 2015), 
#and at the right bottom corner we can see one point is even lower than left bottom corner(Before Sep, 2015 is lower than after Sep, 2015)
ggplot(NULL, aes(x=month_of_record, y=mean_index_standardized)) +    
  geom_point(data = high_earning_df, col = "red") +
  geom_point(data = low_earning_df, col = "blue") + 
  ggtitle("Combined Plot")

#To check total school numbers in each dataframe
number_of_schoold_final_df <- length(unique(final_df$schname))
number_of_schoold_high_earning_df <- length(unique(high_earning_df$schname))
number_of_schoold_low_earning_df <- length(unique(low_earning_df$schname))

#To look at the variable in final_df
vtable(final_df)

#Run the code in the chunk below to look at the distribution of earnings:
#ggplot(final_df, aes(x = earnings)) + geom_density()

#Forming the regression formula with Y(dependent variable) as mean of standardized index and X(independent variable) as log of earnings
#First I want to see if [earning after graduating from school] have any relationship with search interest.
#Since the earnings has some extreme values, I decided to transform data by putting log on earnings to address partially right skewed data.
#From the regression result we can see that: A 10% change in earnings is associated with a 0.1*0.0101 unit change in mean of standardized index.
model1 <- feols(mean_index_standardized~log(earnings), data = final_df)
etable(model1)
ggplot(final_df, aes(x=log(earnings), y= mean_index_standardized)) + geom_point() + geom_smooth(method = 'lm')

#Forming the regression formula with Y(dependent variable) as mean of standardized index and X(independent variable) as whether implemented scorecard or not(binary variable)
#From the regression result we can see that after implementing scorecard, the search interest actually decreased.
model2 <- feols(mean_index_standardized~scorecard_implemented, data = final_df)
etable(model2)
ggplot(final_df, aes(x=scorecard_implemented, y= mean_index_standardized)) + geom_point() + geom_smooth(method = 'lm')

#From the result from model2, I'm wondering after implementing scorecard, what is the effect of searching interest from high-earning/low-earning schools
#Forming the regression formula with Y(dependent variable) as mean of standardized index and X(independent variable) as whether implemented scorecard or not(binary variable), adding the earning category(high/low earning)
#From the regression result we can see that after implementing scorecard and with high-earning school, the search interest actually decreased.
model3 <- feols(mean_index_standardized~scorecard_implemented + earning_category, data = final_df)
etable(model3)
ggplot(data=final_df, mapping = aes(x=scorecard_implemented, y= mean_index_standardized)) + geom_point(aes(color = earning_category)) + theme_bw()

#From the result from model3, thinking there might have some potential variables is causing endogeneity. Which makes the model result biased.
#Forming the regression formula with all the couple of variables which I think might cause endogeneity as control variable. 
#From the regression result we can see that after implementing scorecard and with high-earning school and whether the school is under investigation, 
#whether the school is public/private for profit/private nonprofit, whether the school is closed or operating, the search interest actually decreased.
#From the result we can see that scorecard shifted interest between high and low-earning colleges in negative direction.
#For the coefficient of -0.3338 from scorecard_implement we can interpret as controlling all of other variables in the model, the difference in the search interests between implemented scorecard/not implemented scorecard.
#For the coefficient of 0.0091 from earning_category we can interpret as controlling all of  other variables in the model, the difference in the search interests between high/low earning schools.
#For the research question we have for the project and based on the result of the regression model. 
#We can see that the effect of implementing scorecard has different searching interest between high/low-earning schools by (0.068-0.3339+0.0091-0.0391*HCM2+0.0015*CONTROL-0.0038*CURROPER)-unit, 
#though only coefficients of intercept and scorecard_implemented is statistically significant.
model4 <- feols(mean_index_standardized~scorecard_implemented + earning_category + HCM2 + CONTROL + CURROPER, data = final_df)
etable(model4)

#Forming the regression formula with searching interest, scorecard and high/low-earning schools. Assuming that the relationship between search interest is different for different values of high/low-earning schools.
#Putting Y as searching interests, X as scorecard, Z as high/low-earning schools and interaction term for XZ.
#From the regression result we can see that all coefficients of intercept, scorecard, high/low-earning and interaction term are all statistically significant,
#which indicates that the result clearly reject the null hypothesis that all the coefficients are 0, all the coefficients of variables in the right hand side of model has effects on left hand side.
#We can use take derivative to get the effect of scorecard in the formula, in this model, the effect is (-0.287-0.0941*earning_category). 
#Means The effect of scorecard on search interest gets -0.3811 unit changes compare with high and low earning schools.
#We can use take derivative to get the effect of earning_category in the formula, in this model, the effect is (0.0271-0.0941*scorecard_implemented). 
#Means The effect of scorecard on search interest gets -0.067 unit changes compare with before and after implemented scorecard.
model5 <- feols(mean_index_standardized~scorecard_implemented*earning_category, data = final_df)
etable(model5)
ggplot(final_df,aes(y=mean_index_standardized,x=scorecard_implemented,color=factor(earning_category)))+geom_point()+stat_smooth(method="lm",se=FALSE)

#To see all of regression results together
etable(model1,model2,model3,model4,model5)

table(final_df['earning_category'])
 


