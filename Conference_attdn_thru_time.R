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
install.packages("genderdata", repos = "http://packages.ropensci.org")
setwd('/Users/matthew/Documents/GitHub/GSA_Gender')
#################################### Extract Data ####################################
websites <- read.csv('gsa_conferences.csv')

listofdf <- list() #create list to store dataframes in
for(i in 1:2){
  website <- read_html(as.character(websites$website[i]))
  #replaced a lot of the script with a function
    source('confExtract.R')
    conferenceNames <- confExtract(website)
    colnames(conferenceNames)[1] <- c('First_name')
    
    listofdf[[i]] <- conferenceNames
    rm(conferenceNames)
}

for(i in 1:length(listofdf)){
  df <- listofdf[[i]]
  assign(as.character(websites$year[i]),df)
  
}
rm(website,df,i,confExtract)
#################################### Determine Gender ####################################
for (i in 1:length(listofdf) ){
  df <- gender(as.character(listofdf[[i]]$First_name[-1,]),years = c(1960,2000),method = "ssa")
  assign(paste0((websites$year[i]), '_Gender'),df)
}

GSAGender <-  data.frame(matrix(ncol = 2, nrow = 17971))
GSAGender[,1] <- rbind(data.frame(`2017_Gender`[-1,4]),data.frame(`2018_Gender`[-1,4]))
GSAGender[,2] <- rbind(as.data.frame(matrix(2017, ncol =1,nrow = dim(`2017_Gender`)[1]-1)),
                       as.data.frame(matrix(2018, ncol =1,nrow = dim(`2018_Gender`)[1]-1)))


#################################### plot results, all authors ####################################

g <- ggplot(GSAGender, aes(x = X1))+
  
  geom_bar()+
  facet_wrap(~GSAGender$X2)+
  theme_classic()+
  labs(x = 'Gender',
       y = 'Percent of all authors',
       title = '2017, 2018 GSA annual meeting, gender ratio of all authors',
       caption = 'Data compiled and plotted by M. Morriss')

g

#################################### re-run Gender analysis ####################################
#Re run gender analysis for only presenting authors
# rm(GSAGender, `2017_Gender`,`2018_Gender`)

#copy list of Df to now only have the presnting authors
listofPre <- list()
for(i in 1:length(listofdf)) {
  listofPre[[i]] <- listofdf[[i]][listofdf[[i]]$p_Author,]
}



for (i in 1:length(listofdf) ){
  df <- gender(as.character(listofPre[[i]]$First_name[-1,]),years = c(1960,2000),method = "ssa")
  assign(paste0((websites$year[i]), '_Gender'),df)
}
rm(i, df)

GSAGenderP <-  data.frame(matrix(ncol = 2, nrow = 6860))
GSAGenderP[,1] <- rbind(data.frame(`2017_Gender`[-1,4]),data.frame(`2018_Gender`[-1,4]))
GSAGenderP[,2] <- rbind(as.data.frame(matrix(2017, ncol =1,nrow = dim(`2017_Gender`)[1]-1)),
                       as.data.frame(matrix(2018, ncol =1,nrow = dim(`2018_Gender`)[1]-1)))

#################################### plot presenting authors ####################################

g <- ggplot(GSAGenderP, aes(x = X1))+
  
  geom_bar()+
  facet_wrap(~GSAGenderP$X2)+
  theme_classic()+
  labs(x = 'Gender',
       y = 'Percent of all authors',
       title = '2017, 2018 GSA annual meeting, gender ratio of Presenting authors',
       caption = 'Data compiled and plotted by M. Morriss')

g
