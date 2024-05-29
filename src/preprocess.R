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
  label_23 = paste0('23C', label_23) )


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
            #avg_buds_num = mean(buds_num, na.rm = T),
            #avg_flowers_num = mean(flowers_num, na.rm = T),
            #avg_fruit_num = mean(fruit_num, na.rm = T),
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

# Merge name and gbs into dataframe
summary_2023 %<>% 
  mutate(label_23 = str_extract(variety, "(?<=\\C)\\d+$")) %>% 
  merge(variety_key %>% select(name, gbs, label_23), by = 'label_23') %>%
  select(-label_23)

