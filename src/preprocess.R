packages <- c("tidyr", "readxl", "dplyr", "magrittr", "purrr", "ggplot2") 
invisible(lapply(packages, require, character.only = TRUE ))
source('src/functions.R')
#filepath <- "Data/SB datasheets-Per Trait - CMPT.xlsx"
filepath <- "Data/Speed Breeding 2023data.xlsx"


#######################
#--- Read in Data ---#
#######################
sheetnames <-  readxl::excel_sheets(filepath)
sheetnames

# Plant Height
sbHeight <- readxl::read_excel(filepath, sheet = "SB Height", skip = 2) %>% rename(entry = 'SB ENTRY')
cHeight <- readxl::read_excel(filepath, sheet = "C Height", skip = 2) %>% rename(entry = 'SB ENTRY')

#

#####################
## Clean Data ##
####################

# Note: columns being read in as char, instead of date. temporary solution to manually input with list
# Note: error in sbDates in excel - 10/31/2023 was entered as 10/31/2024 - corrected here
sbDates<- c("10/31/2023",	"11/1/2023", "11/3/2023","11/6/2023",	"11/8/2023",	"11/13/2023",	
            "11/15/2023",	"11/17/2023",	"11/20/2023",	"11/22/2023",	"11/24/2023",	"11/27/2023",	
            "11/29/2023",	"12/1/2023",	"12/7/2023",	"12/14/2023",	"12/21/2023",	"12/28/2023",
            "1/4/2024",	"1/11/2024",	"1/18/2024")
cDates <- c("11/13/2023",	"11/15/2023",	"11/17/2023",	"11/20/2023",	"11/22/2023",	"11/24/2023",	"11/27/2023",	
            "11/29/2023",	"12/1/2023", "12/7/2023",	"12/14/2023",	"12/21/2023",	"12/28/2023",	"1/4/2024",	"1/11/2024",	"1/18/2024")
names(sbHeight) <- c("entry", sbDates)
names(cHeight) <- c("entry", cDates)

# Pivot longer s.t. date is a col, label (sb/c), and combine
cHeight %<>% pivot_longer(!entry, , names_to = "date", values_to = "height") %>% 
  mutate(
    date = as.Date(date, "%m/%d/%Y"),
    exp = "c")
sbHeight %<>% pivot_longer(!entry, , names_to = "date", values_to = "height") %>% 
  mutate(
    date = as.Date(date, "%m/%d/%Y"),
    exp = "sb")
height <- merge(x = cHeight, y = sbHeight, by = c('entry', 'date', 'height', 'exp'), all = TRUE) 
#rm(sbHeight, cHeight, cDates, sbDates)
  
# Separate Entry Names
height %<>% rowwise() %>% mutate(
  entryP1 = strsplit(entry, "[-]")[[1]][1],
  entryP2 = strsplit(entry, "[-]")[[1]][2],
  entryP3 = strsplit(entry, "[-]")[[1]][3],
) 


