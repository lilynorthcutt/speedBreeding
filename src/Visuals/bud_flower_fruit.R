source('src/preprocess.R')



###################################################
#--- GRAPHS ON BUD, FLOWER, FRUIT RELATIONSHIP ---#
###################################################

#--- Graphs focusing on bud to flower relationship (bud to flower vs. dropped bud)

# Num Buds vs. Num Flowers
ggplot(df_2023)+
  geom_point(aes(x = buds_num, y = flowers_num, color = experiment))+
  facet_wrap(.~name)
ggplot(summary_2023)+
  geom_point(aes(x = avg_buds_num, y = avg_flowers_num, color = experiment))+
  facet_wrap(.~name)
  # Only Early Jalapeno and Orange Habanero have flowers

# Flowering by date 
ggplot(summary_2023 %>% filter(!is.na(avg_flowers_num)))+
  geom_point(aes(x = date, y = avg_flowers_num, color = experiment))+
  geom_line(aes(x = date, y = avg_flowers_num, color = experiment))+
  #geom_errorbar(aes(date, ymin = avg_flowers_num - sd_flowers_num, ymax = avg_flowers_num + sd_flowers_num, color = experiment)) +
  facet_wrap(.~ name)+
  xlab("")+ ylab("Avg Flower Num")+ggtitle("Flower Num Over Time for SB vs. Control")+
  theme_bw()

# Flowering by date for Early Jalapeno:
earlyj_budvfruit <- summary_2023 %>% filter(!is.na(avg_flowers_num), name == 'Early Jalapeno',
                                            avg_buds_num>0) %>% 
  select(name, date, avg_buds_num, avg_flowers_num, avg_fruit_num, experiment) %>% 
  pivot_longer(c("avg_buds_num", "avg_flowers_num", "avg_fruit_num"), names_to = 'measure', values_to = 'value') %>% 
  mutate(measure = case_when(measure == 'avg_buds_num' ~"Average Bud #", 
                             measure == 'avg_flowers_num' ~"Average Flower #",
                             T ~"Average Fruit #"))

ggplot(earlyj_budvfruit, aes(x = date, y = value, fill = measure))+
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap(.~ experiment)+
  xlab("")+ ylab("Avg Flower Num")+ggtitle("Early Jalapeno: Bud density vs. Flowering")+
  theme_bw()+
  scale_fill_brewer(palette="Paired")

# Flowering by date for Orange Habanero:
orangeH_budvfruit <- summary_2023 %>% filter(!is.na(avg_flowers_num), name == 'Orange Habanero',
                                            avg_buds_num>0) %>% 
  select(name, date, avg_buds_num, avg_flowers_num, avg_fruit_num, experiment) %>% 
  pivot_longer(c("avg_buds_num", "avg_flowers_num", "avg_fruit_num"), 
               names_to = 'measure', values_to = 'value') %>% 
  mutate(measure = case_when(measure == 'avg_buds_num' ~"Average Bud #", 
                             measure == 'avg_flowers_num' ~"Average Flower #",
                             T ~"Average Fruit #"))

ggplot(orangeH_budvfruit, aes(x = date, y = value, fill = measure))+
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap(.~ experiment)+
  xlab("")+ ylab("Avg Flower Num")+ggtitle("Orange Habanero: Bud density vs. Flowering")+
  theme_bw()+
  scale_fill_brewer(palette="Paired")

#--- Graphs illustrating "race" to first flowering ------------------------------
# First bud
ggplot(first_time)+
  geom_point(aes(x = name, y = first_bud_num_days, color = experiment))+
  coord_flip()+
  ylab("Number of Days from Planting")+ xlab("")+ggtitle("Days Until First Bud")+
  theme_bw()+
  scale_fill_brewer(palette="Paired")+
  ylim(0, 135)

# First Flower
ggplot(first_time)+
  geom_point(aes(x = name, y = first_flower_num_days, color = experiment))+
  coord_flip()+
  ylab("Number of Days from Planting")+ xlab("")+ggtitle("Days Until First Flower")+
  theme_bw()+
  scale_fill_brewer(palette="Paired")+
  ylim(0, 135)


# First Fruit
ggplot(first_time)+
  geom_point(aes(x = name, y = first_fruit_num_days, color = experiment))+
  coord_flip()+
  ylab("Number of Days from Planting")+ xlab("")+ggtitle("Days Until First Fruit")+
  theme_bw()+
  scale_fill_brewer(palette="Paired")+
  ylim(0, 135)

#--- Graphs illustrating "race" to first flowering AND fruiting ----------------
ggplot(first_time)+
  geom_errorbar(aes(name, ymin = first_flower_num_days, ymax = first_fruit_num_days, color = experiment), 
                width = .5, position=position_dodge(.9))+  
  coord_flip()+
  ylab("Number of Days from Planting")+ xlab("")+ggtitle("First Flowering and Fruiting Days")+
  theme_bw()+
  scale_fill_brewer(palette="Paired")+
  ylim(0, 135)



