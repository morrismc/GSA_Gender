# This is s a script written by Matthew Morriss to evaluate the gender ratio
# of those attendees of the Geological Society of America meeting in 2018

#################################### Load tools####################################
rm(list = ls())
library(data.table)
library(XML)
library(rvest)
library(stringr)
library(tidyverse)

websites <- read.csv('gsa_conferences.csv')

website <- read_html(as.character(websites$website[2]))
#replaced a lot of the script with a function
source('confExtract.R')
conferenceNames <- confExtract(website)
colnames(conferenceNames)[1] <- c('First_name')


#################################### Download Gender package ####################################
library(gender)
library(randomNames)
install.packages("genderdata", repos = "http://packages.ropensci.org")


#################################### SECTION TITLE ####################################
a <- gender(as.character(conferencesNames$First_name[-1,]),years = c(1960,2000),method = "ssa")
# a <- gender('Seth',years = c(1950,2000),method = "demo")

#################################### plot results ####################################

g <- ggplot(z, aes(x = gender))+
  
  geom_bar()

g