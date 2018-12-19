# # This is a function written by Matthew Morriss on November 12, 2018 to download
# first name data from the GSA conference website and see who (gender wise) is list as 
# an author
# and who is presenting

confExtract_2 <- function(website,typWebsite,year) {
  library(data.table)
  library(XML)
  library(rvest)
  library(stringr)
  library(tidyverse)
  website <-  html_session(website)
rm(authors,names,firstNames)
  
  if (typWebsite == 0){

  authors <- data_frame(website %>%
    html_nodes(xpath = '//*[@class = "author"]') %>% #grab author names
    html_text(trim = TRUE),
    
   pStatus <- website %>% #grab presenter status
    html_nodes('.papers') %>%
    html_text(trim = TRUE) %>%
    grepl('\\*',.))
  
  colnames(authors) <- c('author','is_presenting')
  } else {

    html_nodes(website, "a[href^='author']") %>% 
      html_attr("href") %>% 
      sprintf(paste("https://gsa.confex.com/gsa/",as.character(year),"AM/webprogram/%s",sep = ''), .) %>% 
      { pb <<- progress_estimated(length(.)) ; . } %>%  # we'll use a progress bar as this will take ~3m
      map_df(~{
        
        pb$tick()$print() # increment progress bar
        
        Sys.sleep(4) # PLEASE leave this in. It's rude to hammer a site without a crawl delay
        
        read_html(.x) %>% 
          html_nodes("div.item > div.author") %>% 
          map_df(~{
            data_frame(
              author = html_text(.x, trim = TRUE),
              is_presenting = html_nodes(.x, xpath="../div[@class='papers']") %>% 
                html_text(trim = TRUE) %>% # retrieve the text of all the "papers"
                paste0(collapse=" ") %>% # just in case there are multiple nodes we flatten them into one
                grepl("*", ., fixed=TRUE) # make it TRUE if we find the "*" 
            )
          })
      }) -> authors
    
    authors <- unique(authors) #[!duplicated(authors[,1]),] remove duplicates
    

      
    }
    

  
  
  #################################### Deal with Data ####################################
  #Clean up author
  
  names <- str_split(authors$author,',')
  
  firstNames <- as.data.frame(as.character(sapply(names,'[',2)),stringsAsFactors = FALSE)
  firstNames <- data.frame(sub("(\\w+).*", "\\1", firstNames$`as.character(sapply(names, "[", 2))`))
  firstNames <- as.data.frame(apply(firstNames,2,function(x)gsub('\\s+', '',x)))
  names(firstNames) <- c("First_name")
  firstNames$First_name <-  data.frame(unlist(firstNames$First_name))
  
  
  yr <- rep(year,dim(firstNames)[1])
  names <- cbind(firstNames$First_name, authors$is_presenting,yr)
  colnames(names) <- c('First_name','is_presenting','Year')
  
  return(names)
  
}