packages <- c("tidyr", "readxl", "dplyr", "magrittr", "purrr", 
              "ggplot2", "stringr", "snakecase") 
invisible(lapply(packages, require, character.only = TRUE ))
source('src/functions.R')




#######################
#--- Read in Data ---#
#######################

# 2023 Data
filepath23 <- "Data/Speed Breeding 2023.xlsx"
sheetname23 <- "Transplant"
experiment_list <- c("SB", "C")
plant_traits <- c("PLANT HEIGHT (cm)", "LEAF #", "LEAF COLOR", 
                  "BUDS", "FLOWERS", "FRUIT")

df_2023 <- generate2023Data(filepath23, sheetname23, experiment_list, plant_traits) %>% 
  suppressMessages() 

leaf_width <- read_excel(filepath23, sheet = 'Leaf Width', skip = 2)

basal_branches <- read_excel(filepath23, sheet = 'Basal Branches', skip = 2)

# Variety Key
filepath_key <- "Data/Variety Key.xlsx"
variety_key <- read_excel(filepath_key)

variety_key %<>% mutate(
  label23 = paste0('23C', label23) )


#######################
#---  Clean Data   ---#
#######################

# Rename cols snakecase
colnames(df_2023) <- to_snake_case(colnames(df_2023)) 
df_2023 %<>% rename(leaf_num = leaf,
                    buds_num = buds,
                    flowers_num = flowers,
                    fruit_num = fruit)

colnames(variety_key) <- to_snake_case(colnames(variety_key)) 
colnames(leaf_width) <- to_snake_case(colnames(leaf_width))
colnames(basal_branches) <- to_snake_case(colnames(basal_branches))


# Separate Entry Names
df_2023 %<>% rowwise() %>% 
  mutate( #Add variety and plant individual identifier
    variety = strsplit(entry, "[-]")[[1]][1],
    identifier = paste0(strsplit(entry, "[-]")[[1]][2], '-', strsplit(entry, "[-]")[[1]][3])
  ) %>% select(variety, identifier, everything(),-entry) 

leaf_width %<>% rowwise() %>% 
  mutate( #Add variety and plant individual identifier
    variety = strsplit(entry, "[-]")[[1]][1],
    identifier = paste0(strsplit(entry, "[-]")[[1]][2], '-', strsplit(entry, "[-]")[[1]][3]),
    week = 1
  ) %>% select(variety, identifier, everything(),-entry) 

basal_branches %<>% rowwise() %>% 
  mutate( #Add variety and plant individual identifier
    variety = strsplit(entry, "[-]")[[1]][1],
    identifier = paste0(strsplit(entry, "[-]")[[1]][2], '-', strsplit(entry, "[-]")[[1]][3]),
    week = 1
  ) %>% select(variety, identifier, everything(),-entry) 

# Convert date to date class
df_2023 %<>% mutate(
  date = as.Date(date, format =  "%Y-%m-%d")
) 

leaf_width %<>% mutate(
  date = as.Date(date, format =  "%Y-%m-%d")
) 

basal_branches %<>% mutate(
  date = as.Date(date, format =  "%Y-%m-%d")
) 

# Make leaf_color a char
df_2023 %<>% mutate(
  leaf_color = leaf_color %>% as.character()
) 



# Summarize data by varitety for every date and experiment type
summary_2023 <- df_2023 %>% group_by(variety, date, experiment) %>% 
  summarise(avg_height = mean(plant_height_cm, na.rm = T),
            sd_height = sd(plant_height_cm, na.rm = T),
            avg_leaf_num = mean(leaf_num, na.rm = T),
            sd_leaf_num = sd(leaf_num, na.rm = T),
            avg_buds_num = mean(buds_num, na.rm = T),
            sd_buds_num = sd(buds_num, na.rm = T),
            avg_flowers_num = mean(flowers_num, na.rm = T),
            sd_flowers_num = sd(flowers_num, na.rm = T),
            avg_fruit_num = mean(fruit_num, na.rm = T),
            sd_fruit_num = sd(fruit_num, na.rm = T),
            #mode_leaf_color = Mode(leaf_color),
            #total_buds_num =sum(buds_num, na.rm = T),
            #total_flower_num = sum(flowers_num, na.rm = T),
            #total_fruit_num = sum(fruit_num, na.rm = T),
            
            # avg_buds
            # avg_flowers
            # avg_fruit
            ) 

# Replace NA with 0
summary_2023[is.na(summary_2023)] <- 0

# NOTE: no data was taken on 12/19/2023 except leaf # and leaf color for  C
# Making the rest of values NA except leaf # and color on this date in case we ever want to use this data
# However for now, we will filter it out because SB was not collected on this day
keep_columns <- c('variety', 'experiment', 'date', 'avg_leaf_num', 'sd_leaf_num')

rows_to_update <- summary_2023$date == '2023-12-19'
columns_to_update <- setdiff(names(summary_2023), keep_columns)
summary_2023[rows_to_update, columns_to_update] <- NA

summary_2023 %<>% filter(!(date == '2023-12-19')) # comment me out to use this data

# NOTE: SB data taken on 1/25 and C taken on 1/26. Want to overwrite this s.t. these entries will be compared with each other
summary_2023 %<>% mutate(date=case_when(date == '2024-01-26' ~as.Date('2024-01-25', format = "%Y-%m-%d"),
                                        T ~date)) 



# Merge name and gbs into dataframe
summary_2023 %<>% 
  mutate(label_23 = variety) %>% #str_extract(variety, "(?<=\\C)\\d+$")) %>% 
  merge(variety_key %>% select(name, gbs, label_23), by = 'label_23') %>%
  select(-label_23)


# Basal Branches: Summmarize data and add names + gbs
basal_summary <- basal_branches %>% group_by(variety, experiment, date) %>% 
  summarise(avg_basal = mean(basal_branches, na.rm = T),
            sd_basal = sd(basal_branches, na.rm = T)) %>% 
  mutate(label_23 = variety) %>%   
  merge(variety_key %>% select(name, gbs, label_23), by = 'label_23') %>%
  select(-label_23)


# Leaf Width: Summarize data and add names + gbs
leaf_width_summary <- leaf_width %>% group_by(variety, experiment, date) %>% 
  summarise(avg_width = mean(leaf_width_cm, na.rm = T),
            sd_width = sd(leaf_width_cm, na.rm = T)) %>% 
  mutate(label_23 = variety) %>%   
  merge(variety_key %>% select(name, gbs, label_23), by = 'label_23') %>%
  select(-label_23)


##############
# Combine 
summary_2023 %<>% merge(basal_summary, by = c("variety", "experiment", "date",
                                              "name", "gbs"), all = TRUE) %>% 
  merge(leaf_width_summary, by = c("variety", "experiment", "date",
                                   "name", "gbs"), all = TRUE)

######################################################################
### RUN CHECKS TO MAKE SURE DATA IS HOW WE EXPECT

# Only the expected variety names
varList <- c('23C340', '23C341', '23C342', '23C343')
summary_2023 %>% filter(variety%in%varList) %>% nrow() == nrow(summary_2023)

# For each date, there is 1 entry for each variety (4) for C and SB
summary_2023 %>% select(date) %>% unique() %>% 
  mutate(numberC = map(date, function(x) summary_2023 %>% filter(date == x, experiment == "C") %>% nrow()),
    repeatVarC = map(date, function(x) summary_2023 %>% 
                       filter(date == x, experiment == "C") %>% select(variety) %>% nrow() ),
    numberSB = map(date, function(x) summary_2023 %>% filter(date == x, experiment == "SB") %>% nrow()),
    repeatVarSB = map(date, function(x) summary_2023 %>% 
                       filter(date == x, experiment == "SB") %>% select(variety) %>% nrow())
  ) %>% select(-date) %>% apply(. , 2, function(col) all(col == 4)) %>% all()


checkSameForEachDate(summary_2023)
