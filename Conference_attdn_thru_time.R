# This is s a script written by Matthew Morriss to evaluate the gender ratio
# of those attendees of the Geological Society of America meeting in 2018

#################################### Load tools####################################
rm(list = ls())
library(data.table)
library(XML)
library(rvest)
library(stringr)
library(tidyverse)
library(gender)
library(randomNames)
library(dplyr)
library(tidyverse)
# install.packages("genderdata", repos = "http://packages.ropensci.org")
setwd('/Users/matthew/Documents/GitHub/GSA_Gender')
websites <- read.csv('gsa_conferences.csv')
#################################### Extract Data ####################################


listofdf <- list() #create list to store dataframes in

for(i in 1:dim(websites)[1]){ #dim(websites)[1]
  Sys.sleep(0.1)
  print(i)
  # website <- read_html(as.character(websites$website[i]))
  #replaced a lot of the script with a function
    source('confExtract_2.R')
    conferenceNames <- confExtract_2(as.character(websites[i,1]), websites[i,3],websites[i,2])
    #stopped work November 26, 2018 for the moment with working code to this point.
    listofdf[[i]] <- conferenceNames
    rm(conferenceNames)
    flush.console() 
}

# for(i in 1:length(listofdf)){
#   df <- listofdf[[i]]
#   assign(as.character(websites$year[i]),df)
#   
# }

GSA_Names <- plyr::ldply(listofdf, data.frame)

write.csv(GSA_Names,'GSA_names.csv')
rm(website,df,i,confExtract)
#################################### Number of registrants/presenters ####################################
setwd('/Users/matthew/Documents/GitHub/GSA_Gender')
GSA_Names <- read.csv('GSA_names.csv')
library(tidyverse)
library(gender)
#################################### SECTION TITLE ####################################
attendence <- plyr::count(GSA_Names$Year)

ggplot(attendence, aes(x = x, y = freq))  +
  geom_line()+
  labs(x = "Year",y = 'Author Count',title = 'GSA conference Author Count through time')+
  theme_light()

#################################### Presenting v non-presenting ####################################
GSA_Names %>% 
  count(Year, is_presenting) %>%
  ggplot(aes(Year, n, color = is_presenting))+
    geom_line()

#################################### Create Gsa presenting ####################################
GSA_Names$First_name <- as.character(GSA_Names$First_name)

# Identify possible gender for GSA Names
# GSA_NamesS <- GSA_Names[1:1000,]
GSA_Names %>%
  # rowwise() %>%
  do(results = gender(.$First_name, years = c(1960, 2000),method = "ssa")) %>%
  do(bind_rows(.$results)) -> GSA_Gender

GSA_Gender %>%
  select(name, gender)%>%
  unique()-> GSA_Gender
names(GSA_Gender) <- c('First_name','gender')

  
GSA_Names %>%
  left_join(GSA_Gender, by = c('First_name')) -> GSA_Names2

GSA_Names2 <- GSA_Names2[complete.cases(GSA_Names2),]
GSA_NamesP <- GSA_Names2[GSA_Names2$is_presenting,]

#################################### Plot gender over year ####################################
GSA_Names2 %>%
  group_by(Year) %>%
  count(gender) %>%
  ggplot(aes(x = Year, y = n, color = gender))+
  geom_line()+
  theme_set(theme_light(base_size = 14))+
  labs(x = 'Year',y = 'Count', color = 'Gender',title = 'GSA National Meeting gender split of authors')
#################################### Proportion Female #################################### 
GSA_Names2 %>%
  group_by(Year,gender) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  ggplot(aes(x = Year, y = freq*100, color = gender))+
  geom_line()+
  labs(y = '% of Total attendees',title = 'GSA National Meeting gender split of authors')+
  ylim(0,100)+
  theme_set(theme_light(base_size = 14))
#################################### Proportion female presenting authors ####################################
GSA_NamesP %>%
  group_by(Year,gender) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  ggplot(aes(x = Year, y = freq*100, color = gender))+
  geom_line()+
  labs(y = '% of Total Presenting authors',title = 'GSA National Meeting gender split of Presenting authors')+
  ylim(0,100)+
  theme_set(theme_light(base_size = 14))

#################################### Proportion gender, all authors ####################################
GSA_NamesP %>%
  group_by(Year,gender) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  ggplot(aes(x = Year, y = freq*100, color = gender))+
  geom_line()+
  labs(y = '% of Total Presenting authors',title = 'GSA National Meeting gender split of Presenting authors')+
  ylim(0,100)+
  theme_set(theme_light(base_size = 14))
