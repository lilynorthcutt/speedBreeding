### Function ###

#Functions to read in 2023 data
generate2023Data <- function(filepath, sheetname, experiment_list, plant_traits){
  'Function to cleanly combine both speed breeding and control data into one dataframe'
  
  experiment_list <- lapply(experiment_list, function(x) dataByExperiment(filepath, x, sheetname, plant_traits))
  df <- bind_rows(experiment_list)
  
  return(df)
}

dataByExperiment <- function(filepath, exp, sheetname, plant_traits){
  'Function to read in specified sheet, and output wrangled dataframe from the sheet'
  
  # Read in data from sheet
  df <- readxl::read_excel(filepath, sheet = paste0(exp, " ", sheetname), skip = 2)
  dates <- readxl::read_excel(filepath, sheet = paste0(exp, " ", sheetname), skip = 1, n_max = 1, col_names = FALSE)
  dates <- as.list(as.data.frame(t(dates[ , colSums(is.na(dates))==0])))$V1
  
  
  # Rearrange data in proper columns 
  traits_list <- lapply(plant_traits, function(x) dataByTrait(df, x, dates, plant_traits))
  df <- traits_list %>% reduce(full_join, by = c("entry", "date") ) %>% mutate( experiment = exp) # Add experiment label
  return(df)
}  

dataByTrait <- function(df, plant_trait, dates, plant_traits){
  'Function to take in the full data frame, filter down to the specified plant trait,
  and output a pivotted longer df with columns [entry, date, {plant_trait}]'
  
  # Select only plant trait cols (example only height)
  df_plant_trait <- df %>% select(dplyr::contains(c('ENTRY', plant_trait)))
  # Rename columns as dates to prep for pivotting
  colnames(df_plant_trait) <- c("entry", dates)
  # Pivot_longer()
  df_plant_trait %<>% pivot_longer(!entry, names_to = "date", values_to = plant_trait) 
  
  return(df_plant_trait)
}



#Functions to run and view tukeys hsd
tukeyHsdByTrait <- function(df, colname){
  'Function to take in our dataframe, modify the data for hypothesis testing
  and run Tukeys HSD on the selected data - returning the test'
  
  trait <- df %>% select(variety, experiment, date, colname) %>% 
    # SB collected jan 25 and C collected jan 26 => 
    # assign C as jan 26 s.t. both datas can be considered same day
    mutate(date=case_when(date == '2024-01-26' ~as.Date('2024-01-25', format = "%Y-%m-%d"),
                          T ~date)) %>% 
    pivot_wider( names_from = experiment, values_from = colname) %>% 
    # there is no data taken on dec 19th
    filter(!(date == '2023-12-19')) %>% 
    # Only NA's left are where control is too small to be transplanted yet
    # replace NA's with 0
    replace(is.na(.), 0) %>% ungroup() %>% 
    #Add week number and change in trait  
    group_by(variety) %>% arrange(date) %>% 
    mutate(
      week = row_number(),
      change_exp = SB - lag(SB),
      change_c = C - lag(C)
    )
  
  
  # Now pivot date back to longer to perform tukey
  trait %<>% select(-SB, -C) %>% 
    pivot_longer(c('change_exp', 'change_c'), 
                 names_to = 'experiment', values_to = 'change_in_trait') %>% 
    filter(!is.na(change_in_trait)) %>% 
    mutate(
      experiment = as.factor(experiment)
    ) %>% ungroup()  %>%  mutate(
      experiment = case_when(experiment == 'change_exp' ~'SB',
                             T ~"C"),
      rownum = row_number()
    )
  
  
  ##### TUKEYS HSD
  trait$variety <- as.factor(trait$variety)
  aov <- aov(change_in_trait ~  variety * week * experiment,#+ Error(variety/week), 
             data = trait)
  tukey_result <- TukeyHSD(aov)
  
  
  return(tukey_result)
}

prepTukeysDf <- function(tukey_result){
  'Function that takes tukey HSD output and preps it so we can graph it ourselves'
  
  results <- tukey_result$`variety:experiment` %>% as.data.frame() 
  rownames(results) <- rownames(tukey_result[["variety:experiment"]])
  
  results %<>% tibble::rownames_to_column("name") %>% rowwise() %>% 
    mutate(var1 = unlist(str_extract_all(name, "\\d+C\\d+"))[1],
           var2 = unlist(str_extract_all(name, "\\d+C\\d+"))[2],
           exp1 = str_extract(name, "(?<=:)[A-Za-z]+(?=-)"),
           exp2 = sapply(
             strsplit(name, "-"), 
             function(x) substr(x[2], regexpr(":", x[2]), regexpr(":", x[2]) + 2)) %>% 
             gsub(":", "",.)
    )
  
  return(results)
}

# Graphing Functoins
addVarName <- function(df, colname){
  'Function that adds the variety name by 
  the pedigree number for a specified column'
  
  df[['label_23']] <- df[[colname]]

  df %<>%merge(
    variety_key %>% select(name, label_23) %>% rename(var_name = name), 
    by = 'label_23') 
  
  df[[paste0(colname, "_name")]] <- df[['var_name']]
  
  return(df %>% select(-var_name))
  
}
