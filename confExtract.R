# # This is a function written by Matthew Morriss on November 12, 2018 to download
# first name data from the GSA conference website and see who (gender wise) is list as 
# an author
# and who is presenting

confExtract <- function(website) {
  library(data.table)
  library(XML)
  library(rvest)
  library(stringr)
  library(tidyverse)
  
  authors <- website %>%
    html_nodes(xpath = '//*[@class = "author"]') %>%
    html_text()
  
  
  pStatus <- website %>%
    html_nodes('.papers') %>%
    html_text()
  #################################### Deal with Data ####################################
  #Clean up author
  authors <- sub('\\\r\n\t\t\r\n\t\t\t','',authors)
  authors <- sub('\\.\r\n\t\t\r\n\t','',authors)
  authors <- gsub('\r.*','',authors)
  names <- str_split(authors,',')
  
  firstNames <- as.data.frame(as.character(sapply(names,'[',2)),stringsAsFactors = FALSE)
  firstNames <- data.frame(sub("(\\w+).*", "\\1", firstNames$`as.character(sapply(names, "[", 2))`))
  firstNames <- as.data.frame(apply(firstNames,2,function(x)gsub('\\s+', '',x)))
  names(firstNames) <- c("First_name")
  firstNames$First_name <-  data.frame(unlist(firstNames$First_name))
  
  #clean up presenter status,the end goal here is a boolean array, the same size
  #as the names
  pStatus <- data.frame(grepl('\\*',pStatus))
  names(pStatus) <- c('p_Author')
  
  names <- cbind(firstNames, pStatus)
  return(names)
  
}