source('src/preprocess.R')

################################
#--- Graphs Summarized Data ---#
################################

# Height (SCATTER)
ggplot(summary_2023)+
  geom_point(aes(x = date, y = avg_height, color = experiment))+
  geom_line(aes(x = date, y = avg_height, color = experiment))+
  geom_errorbar(aes(date, ymin = avg_height - sd_height, ymax = avg_height + sd_height, color = experiment)) +
  facet_wrap(.~ name)+
  xlab("")+ ylab("Avg Height")+ggtitle("Height Over Time for SB vs. Control")


# Height Percent Increase (SCATTER)
height_increase <- summary_2023 %>% group_by(name, experiment) %>% arrange(date, .by_group = TRUE) %>% 
  mutate(percent_change =  case_when( lag(avg_height) == 0 ~NA,
                                      avg_height == 0 ~NA,
                                      T ~(avg_height -lag(avg_height))/lag(avg_height))
        ) #%>% filter(!is.na(percent_change))

ggplot(height_increase)+
  geom_point(aes(x = date, y = percent_change*100, color = experiment))+
  geom_line(aes(x = date, y = percent_change*100, color = experiment))+
  facet_wrap(.~ name)+
  xlab("")+ ylab("Avg Height Increase (%)")+ggtitle("Percent Increase in Height Over Time for SB vs. Control")


# Leaf Number
ggplot(summary_2023)+
  geom_point(aes(x = date, y = avg_leaf_num, color = experiment))+
  geom_line(aes(x = date, y = avg_leaf_num, color = experiment))+
  geom_errorbar(aes(date, ymin = avg_leaf_num - sd_leaf_num, ymax = avg_leaf_num + sd_leaf_num, color = experiment)) +
  facet_wrap(.~ name)+
  xlab("")+ ylab("Avg Leaf Num")+ggtitle("Leaf Num Over Time for SB vs. Control")


# Leaf width (BAR)
leaf_width_summary <- leaf_width %>% group_by(variety, experiment, date) %>% 
  summarise(avg_width = mean(leaf_width_cm, na.rm = T),
            sd_width = sd(leaf_width_cm, na.rm = T)) %>% 
  mutate(label_23 = variety) %>%   
  merge(variety_key %>% select(name, gbs, label_23), by = 'label_23') %>%
  select(-label_23)

ggplot(leaf_width_summary)+
  geom_bar(aes(x = date, y = avg_width, color = experiment, fill = experiment), stat="identity", position=position_dodge(), alpha = .65)+
  geom_errorbar(aes(date, ymin = avg_width - sd_width, ymax = avg_width + sd_width, color = experiment), width=.2,  position=position_dodge(.9)) +
  facet_wrap(.~name)+
  xlab("")+ ylab("Avg Leaf Width")+ggtitle("Leaf Width for SB vs. Control")

ggplot(leaf_width_summary)+
  geom_bar(aes(x = name, y = avg_width, color = experiment, fill = experiment), stat="identity", position=position_dodge(), alpha = .65)+
  geom_errorbar(aes(name, ymin = avg_width - sd_width, ymax = avg_width + sd_width, color = experiment), width=.2,  position=position_dodge(.9)) +
  xlab("")+ ylab("Avg Leaf Width")+ggtitle("Leaf Width for SB vs. Control")+
  coord_flip()





# Basal Branch (BAR)
basal_summary <- basal_branches %>% group_by(variety, experiment, date) %>% 
  summarise(avg_basal = mean(basal_branches, na.rm = T),
            sd_basal = sd(basal_branches, na.rm = T)) %>% 
  mutate(label_23 = variety) %>%   
  merge(variety_key %>% select(name, gbs, label_23), by = 'label_23') %>%
  select(-label_23)

ggplot(basal_summary)+
  geom_bar(aes(x = date, y = avg_basal, color = experiment, fill = experiment), stat="identity", position=position_dodge(), alpha = .65)+
  geom_errorbar(aes(date, ymin = avg_basal - sd_basal, ymax = avg_basal + sd_basal, color = experiment), width=.2,  position=position_dodge(.9)) +
  facet_wrap(.~name)+
  xlab("")+ ylab("Avg # Basal Branches")+ggtitle("Basal Branch Number for SB vs. Control")


ggplot(basal_summary)+
  geom_bar(aes(x = name, y = avg_basal, color = experiment, fill = experiment), stat="identity", position=position_dodge(), alpha = .65)+
  geom_errorbar(aes(name, ymin = avg_basal - sd_basal, ymax = avg_basal + sd_basal, color = experiment), width=.2,  position=position_dodge(.9)) +
  xlab("")+ ylab("Avg # Basal Branches")+ggtitle("Basal Branch Number for SB vs. Control")+
  coord_flip()



# ============= # ============= # ============= # ============= # ============= 
# Total Buds
ggplot(summary_2023)+
  geom_line(aes(x = date, y = total_buds_num, color = experiment))+
  facet_wrap(.~ name)+
  xlab("")+ ylab("Total Buds")+ggtitle("Total Buds Over Time for SB vs. Control")

# Total Flowers
ggplot(summary_2023)+
  geom_point(aes(x = date, y = total_flower_num, color = experiment))+
  facet_wrap(.~ name)+
  xlab("")+ ylab("Total Flowers")+ggtitle("Total Flowers Over Time for SB vs. Control")

# Total Fruit
ggplot(summary_2023)+
  geom_point(aes(x = date, y = total_fruit_num, color = experiment))+
  facet_wrap(.~ name)+
  xlab("")+ ylab("Total Fruit")+ggtitle("Total Fruit Over Time for SB vs. Control")

# ============= # ============= # ============= # ============= # ============= 
# Bar graph
features <- c("avg_height", "avg_leaf_num","total_buds_num","total_flower_num",
              "total_fruit_num")
total_by_date <- df_2023 %>% group_by(variety, experiment) %>% 
  summarise(avg_height = mean(plant_height_cm, na.rm = T),
            avg_leaf_num = mean(leaf_num, na.rm = T),
            avg_buds_num = mean(buds_num, na.rm = T),
            avg_flowers_num = mean(flowers_num, na.rm = T),
            avg_fruit_num = mean(fruit_num, na.rm = T),
            #mode_leaf_color = Mode(leaf_color),
            total_buds_num =sum(buds_num, na.rm = T),
            total_flower_num = sum(flowers_num, na.rm = T),
            total_fruit_num = sum(fruit_num, na.rm = T),
            
            # avg_buds
            # avg_flowers
            # avg_fruit
  ) %>% 
  mutate('23_c' = str_extract(variety, "(?<=\\C)\\d+$")) %>% 
  merge(variety_key %>% select(name, gbs, '23_c'), by = '23_c') %>%
  select(-'23_c')

bargraph_pivot_data <- total_by_date %>% 
  pivot_longer(all_of(features), names_to = 'features',values_to = 'values') 


ggplot(bargraph_pivot_data)+
  geom_col(aes(x = features, y = values, color = experiment, fill = experiment), 
           position = "dodge")+
  facet_wrap(.~name)+ 
  xlab("")+ylab("")+ ggtitle(("Total difference in measure features for SB vs. C"))

