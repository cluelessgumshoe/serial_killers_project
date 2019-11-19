##------------------------------------------------------------------------------------------##
##                     Israel Keyes - Location analysis                                     ##
##------------------------------------------------------------------------------------------##

#Data Source: http://dig.abclocal.go.com/wls/documents/wls_081313_Israel%20Keyes_timeline.pdf

#-------#
# Setup #
#-------#

# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load packages
p_load(tidyverse, pdftools, stringr, tidytext)


#----------------#
# Load and Clean #
#----------------#

url = "http://dig.abclocal.go.com/wls/documents/wls_081313_Israel%20Keyes_timeline.pdf"

dat <- pdftools::pdf_text(url) %>% str_trim(side = "both")

make_table <- function(table){
  dat_f <- table %>% read_lines() %>%   # separate lines
    str_replace("\r\n\r\n\f\r\n","\r\n\r\n") %>%
    str_split("\r\n\r\n", simplify = T) %>% 
    read_fwf(fwf_empty(.)) %>%  # read as fixed-width file
    as.data.frame()
    #names(dat_f) = dat_f[1,] #use the first row as row names
    #dat_f <- dat_f[-1,] #remove the first row now that it has names
    #names(dat_f) = c("DATES","LOCATIONS","ACTIVITY")
    #dat_f <- dat_f[,-3]
    #dat_f %<>% distinct()

return(dat_f)
}
dat_f <- make_table(dat)

view(dat_f)


dat_f2 <- dat_f %>% 
  mutate(
    `DATES LOCATION` = case_when( #need to correct for the table having multiple lines in the third column
      is.na(`DATES LOCATION`) & !is.na(lead(`DATES LOCATION`,1)) & !is.na(ACTIVITY) ~ "SKIP", #make a dummy for the column that goes with group
      TRUE ~ `DATES LOCATION` 
    )
  ) %>%
  fill(`DATES LOCATION`,.direction = "down") %>% #fill in values below
  mutate(`DATES LOCATION` = na_if(`DATES LOCATION`, "SKIP")) %>% #now replace NA
  fill(`DATES LOCATION`,.direction = "up") %>%  #and fill them into those
  group_by(`DATES LOCATION`) %>% #preserve
  mutate(ACTIVITY = paste(ACTIVITY, collapse = ' ')) %>% #combine
  distinct() %>% #remove duplicate rows
  separate(`DATES LOCATION`,"DATES","LOCATION",sep=".[:digit:]{4,}[:space:][:alpha:]{3,}") 
  
view(dat_f2)

#################################################################################################################################
#

dat2 <- read_delim("data/wls_081313_Israel Keyes_timeline.txt", delim = "\r\n\r\n") %>% str_trim(side = "both") %>% str_replace("\r\n\r\n\f\r\n","\r\n\r\n")

make_table2 <- function(table){
  dat_f <- table %>% read_lines() %>%   # separate lines
    str_split("\r\n\r\n", simplify = T) %>% 
    read_fwf(fwf_empty(.)) %>%  # read as fixed-width file
    as.data.frame()
  #names(dat_f) = c("DATES","LOCATIONS","ACTIVITY")
  #dat_f<- select(-ACTIVITY)
}
dat_f2 <- make_table2(dat2[1])

view(dat_f2)

###################################################################################################################################
#try with tabulizer - more garbage
library(tabulizer)

report <- "http://dig.abclocal.go.com/wls/documents/wls_081313_Israel%20Keyes_timeline.pdf"
dat3 <- extract_tables(report, encoding = "UTF-8")








