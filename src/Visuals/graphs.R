source('src/preprocess.R')

################################
#--- Graphs Summarized Data ---#
################################

# Height (SCATTER)
ggplot(summary_2023 %>% filter(!is.na(avg_height)))+
  geom_point(aes(x = date, y = avg_height, color = experiment))+
  geom_line(aes(x = date, y = avg_height, color = experiment))+
  geom_errorbar(aes(date, ymin = avg_height - sd_height, ymax = avg_height + sd_height, color = experiment)) +
  facet_wrap(.~ name, ncol = 4)+
  xlab("")+ ylab("Avg Height (in)")+ggtitle("Average Height Over Time")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size = 20))


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
ggplot(summary_2023 %>% filter(!is.na(avg_leaf_num)) %>% mutate(experiment = case_when(experiment == 'C' ~'C', T ~"trt")))+
  geom_point(aes(x = date, y = avg_leaf_num, color = experiment))+
  geom_line(aes(x = date, y = avg_leaf_num, color = experiment))+
  geom_errorbar(aes(date, ymin = avg_leaf_num - sd_leaf_num, ymax = avg_leaf_num + sd_leaf_num, color = experiment)) +
  facet_wrap(.~ name)+theme_bw()+
  theme(#legend.position="bottom",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size = 20)
        )+
  xlab("")+ ylab("Avg Leaf Num")+ggtitle("Leaf Count Over Time")

# Bud Number
ggplot(summary_2023 %>% filter(!is.na(avg_buds_num)))+
  geom_point(aes(x = date, y = avg_buds_num, color = experiment))+
  geom_line(aes(x = date, y = avg_buds_num, color = experiment))+
  geom_errorbar(aes(date, ymin = avg_buds_num - sd_buds_num, ymax = avg_buds_num + sd_buds_num, color = experiment)) +
  facet_wrap(.~ name)+
  xlab("")+ ylab("Avg Bud Num")+ggtitle("Bud Num Over Time for SB vs. Control")


# Flower Number
ggplot(summary_2023 %>% filter(!is.na(avg_flowers_num)))+
  geom_point(aes(x = date, y = avg_flowers_num, color = experiment))+
  geom_line(aes(x = date, y = avg_flowers_num, color = experiment))+
  geom_errorbar(aes(date, ymin = avg_flowers_num - sd_flowers_num, ymax = avg_flowers_num + sd_flowers_num, color = experiment)) +
  facet_wrap(.~ name)+
  xlab("")+ ylab("Avg Flower Num")+ggtitle("Flower Num Over Time for SB vs. Control")


# Fruit Number
ggplot(summary_2023 %>% filter(!is.na(avg_fruit_num)))+
  geom_point(aes(x = date, y = avg_fruit_num, color = experiment))+
  geom_line(aes(x = date, y = avg_fruit_num, color = experiment))+
  geom_errorbar(aes(date, ymin = avg_fruit_num - sd_fruit_num, ymax = avg_fruit_num + sd_fruit_num, color = experiment)) +
  facet_wrap(.~ name)+
  xlab("")+ ylab("Avg Fruit Num")+ggtitle("Fruit Num Over Time for SB vs. Control")



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


ggplot(basal_summary %>% mutate(experiment = case_when(experiment == 'C' ~'C', T ~"trt")))+
  geom_bar(aes(x = name, y = avg_basal, color = experiment, fill = experiment), stat="identity", position=position_dodge(), alpha = .65)+
  geom_errorbar(aes(name, ymin = avg_basal - sd_basal, ymax = avg_basal + sd_basal, color = experiment), width=.2,  position=position_dodge(.9)) +
  xlab("")+ ylab("Avg Number of Basal Branches")+ggtitle("Count of Basal Branch")+
  coord_flip() + theme_bw() + theme(text = element_text(size = 20))


# ============= # ============= # ============= # ============= # ============= 
# Days Until x
# ============= # ============= # ============= # ============= # ============= 
df <- df_2023 %>% select(variety, identifier, experiment, date, buds_num, 
                              flowers_num, fruit_num) 
df[is.na(df)] <- 0

# Calculate the first week that had the first bud, flower, and fruit for
# each PLANT (group by variety, identifier, experiment )
# (This is necessary so we can calculate mean AND sd)
df  %<>% arrange(experiment, variety, date) %>%
  group_by(experiment, variety, identifier) %>%
  mutate(week_number = row_number()) %>% # label week 
  group_by(experiment, variety, identifier) %>%
  summarise(first_bud_week = ifelse(any(buds_num != 0), 
                                    min(week_number[buds_num != 0]), 19),
            first_flower_week = ifelse(any(flowers_num != 0), 
                                       min(week_number[flowers_num != 0]), 19),
            first_fruit_week = ifelse(any(fruit_num != 0), 
                                      min(week_number[fruit_num != 0]), 19)) %>% 
  ungroup()

# Now summarize down to the avg/sd first by variety and experiment
df %<>%  group_by(variety, experiment) %>% 
  summarise(
    avg_first_bud = mean(first_bud_week, na.rm = T) ,
    sd_first_bud = sd(first_bud_week, na.rm = T),
    avg_first_flower = mean(first_flower_week, na.rm = T),
    sd_first_flower = sd(first_flower_week, na.rm = T),
    avg_first_fruit = mean(first_fruit_week, na.rm = T),
    sd_first_fruit = sd(first_fruit_week, na.rm = T)
  )

# ADD IN VARIETY NAME
variety_for_merge <- variety_key %>% select(name, gbs, label_23) %>% rename(variety = 'label_23')
df %<>% merge(variety_for_merge, by = 'variety')

# NOTE: DECIDING NOT TO ROUND BECAUSE FOR IF SOME PLANTS 

# Changing it so that if plants never get buds they have a mean of 0 
# (instead of 19 which we previously had for hyp testing because there are only 18 weeks in the study)
df %<>% mutate(avg_first_bud = case_when(avg_first_bud == 19 ~0, T~avg_first_bud),
               avg_first_flower = case_when(avg_first_flower == 19 ~0, T~avg_first_flower),
               avg_first_fruit = case_when(avg_first_fruit == 19 ~0, T~avg_first_fruit)) %>% 
  mutate(sd_first_bud = case_when(avg_first_bud == 0 ~0, T~sd_first_bud),
         sd_first_flower = case_when(avg_first_flower == 0 ~0, T~sd_first_flower),
         sd_first_fruit = case_when(avg_first_fruit == 0 ~0, T~sd_first_fruit),)

# NOTE: DECIDING NOT TO ROUND BECAUSE AVG COULD BE >18.5 BUT NOT NEVER, THUS
# THIS WILL CATCH THAT CASE
# example - Chiltepin Control Buds

# Buds
ggplot(df %>% mutate(temp = 1))+
  geom_bar(aes(x = temp, y = avg_first_bud, color = experiment, fill = experiment), stat="identity", position=position_dodge(), alpha = .65)+
  geom_errorbar(aes(temp, ymin = avg_first_bud - sd_first_bud, ymax = avg_first_bud + sd_first_bud, color = experiment), width=.2,  position=position_dodge(.9)) +
  facet_wrap(.~name)+
  ylab("Avg # First Day Until Buds")+ggtitle("First Day Until Plants Get Buds")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

#sum <- summary_2023 %>% filter(name == 'Chiltepin', experiment == 'C', !is.na(avg_buds_num)) %>% select(name, date, avg_buds_num) 
#d <- df %>% filter(variety == '23C343', experiment == 'C') #%>% select(variety, date, buds_num) 

# Flower
ggplot(df %>% mutate(temp = 1))+
  geom_bar(aes(x = temp, y = avg_first_flower, color = experiment, fill = experiment), stat="identity", position=position_dodge(), alpha = .65)+
  geom_errorbar(aes(temp, ymin = avg_first_flower - sd_first_flower, ymax = avg_first_flower + sd_first_flower, color = experiment), width=.2,  position=position_dodge(.9)) +
  facet_wrap(.~name)+
  ylab("Avg # First Day Until Buds")+ggtitle("First Day Until Plants Get Flowers")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Fruit
ggplot(df %>% mutate(temp = 1))+
  geom_bar(aes(x = temp, y = avg_first_fruit, color = experiment, fill = experiment), stat="identity", position=position_dodge(), alpha = .65)+
  geom_errorbar(aes(temp, ymin = avg_first_fruit - sd_first_fruit, ymax = avg_first_fruit + sd_first_fruit, color = experiment), width=.2,  position=position_dodge(.9)) +
  facet_wrap(.~name)+
  ylab("Avg # First Day Until Buds")+ggtitle("First Day Until Plants Get Fruits")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

