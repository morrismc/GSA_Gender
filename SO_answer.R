#################################### SECTION TITLE ####################################
library(rvest)
library(tidyverse)

pg <- read_html("https://gsa.confex.com/gsa/2016AM/webprogram/authora.html")

html_nodes(pg, "a[href^='author']") %>% 
  html_attr("href") %>% 
  sprintf("https://gsa.confex.com/gsa/2016AM/webprogram/%s", .) %>% 
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
  }) -> author_with_presenter_status

author_with_presenter_status1 <- unique(author_with_presenter_status[,1])