#################################### SECTION TITLE ####################################
setwd('/Users/matthew/Documents/GitHub/GSA_Gender')
library(readxl)
library(ggplot2)
rm(list = ls())
data <- read_excel('invited_speakers.xlsx')

#################################### SECTION TITLE ####################################



g = ggplot(data, aes(x = data$X__1,
                     y = data$Percent,
                     fill = data$Gender,
                     group= data$Gender))+
  geom_col(position = 'dodge')+
  labs(x = 'Day of Conference', y = 'Percent', fill  = 'Gender',
       title = 'GSA 2018 Gender ratio in invited speakers',
       caption = 'Data compiled and plotted by M. Morriss')+
  scale_fill_manual(values = c('blue','red'))+
  theme(plot.caption = element_text(hjust = 0))+
  theme_classic()
  


g