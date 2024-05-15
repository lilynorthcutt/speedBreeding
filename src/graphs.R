source('src/preprocess.R')

#######################
#---   Plot Data   ---#
#######################

#> colnames(summary_2023)
#[1] "variety"          "date"             "experiment"       "avg_height"      
#[5] "avg_leaf_num"     "total_buds_num"   "total_flower_num" "total_fruit_num" 

ggplot(summary_2023)+
  geom_point(aes(x = date, y = avg_height, color = experiment))+
  facet_wrap(.~ name)+
  xlab("")+ ylab("Avg Height")+ggtitle("Height Over Time for SB vs. Control")

ggplot(summary_2023)+
  geom_point(aes(x = date, y = avg_leaf_num, color = experiment))+
  facet_wrap(.~ name)+
  xlab("")+ ylab("Avg Leaf Num")+ggtitle("Leaf Num Over Time for SB vs. Control")

ggplot(summary_2023)+
  geom_line(aes(x = date, y = total_buds_num, color = experiment))+
  facet_wrap(.~ name)+
  xlab("")+ ylab("Total Buds")+ggtitle("Total Buds Over Time for SB vs. Control")

ggplot(summary_2023)+
  geom_point(aes(x = date, y = total_flower_num, color = experiment))+
  facet_wrap(.~ name)+
  xlab("")+ ylab("Total Flowers")+ggtitle("Total Flowers Over Time for SB vs. Control")

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


