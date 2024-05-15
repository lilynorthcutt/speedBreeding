source('src/preprocess.R')

##########################################
### PERFORM HYP TEST 
##########################################

################################################################################
## Height_data 
height <- summary_2023 %>% select(variety, experiment, date, avg_height) %>% 
  # SB collected jan 25 and C collected jan 26 => 
  # assign C as jan 26 s.t. both datas can be considered same day
  mutate(date=case_when(date == '2024-01-26' ~as.Date('2024-01-25', format = "%Y-%m-%d"),
                        T ~date)) %>% 
  pivot_wider( names_from = experiment, values_from = avg_height) %>% 
  # there is no data taken on dec 19th
  filter(!(date == '2023-12-19')) %>% 
  # Only NA's left are where control is too small to be transplanted yet
  # replace NA's with 0
  replace(is.na(.), 0) %>% ungroup() %>% 
  #Add week number and change in height  
  group_by(variety) %>% arrange(date) %>% 
  mutate(
    week = row_number(),
    change_exp = SB - lag(SB),
    change_c = C - lag(C)
  )


# Now pivot date back to longer to perform tukey
height %<>% select(-SB, -C) %>% 
  pivot_longer(c('change_exp', 'change_c'), 
               names_to = 'experiment', values_to = 'change_in_height') %>% 
  filter(!is.na(change_in_height)) %>% 
  mutate(
    experiment = as.factor(experiment)
  ) %>% ungroup()  %>%  mutate(
    experiment = case_when(experiment == 'change_exp' ~'SB',
                           T ~"C"),
    rownum = row_number()
  )

library(ggpubr)
library(rstatix)
height %>% 
  dplyr::group_by(variety, experiment, week) %>%
  get_summary_stats(change_in_height, type = "mean_sd") 

bxp <- ggboxplot(
  height, x = "week", y = "change_in_height",
  color = "experiment", palette = "jco",
  facet.by = "variety", short.panel.labs = FALSE
)
bxp

ggqqplot(height, "change_in_height", ggtheme = theme_bw()) +
  facet_grid( experiment ~ week, labeller = "label_both")

##### TUKEYS HSD
height$variety <- as.factor(height$variety)
aov <- aov(change_in_height ~  variety * week * experiment,#+ Error(variety/week), 
           data = height)
aov

tukey_result <- TukeyHSD(aov)
tukey_result

summary(tukey_result)

plot(tukey_result, las = 1 )

# My plot

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



ggplot(results)+
  geom_point(aes(x = name, y = diff))+
  geom_errorbar(aes(name,ymin = lwr, ymax = upr), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()

ggplot(results %>% filter(var1 == var2))+
  geom_point(aes(x = name, y = diff))+
  geom_errorbar(aes(name,ymin = lwr, ymax = upr), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()

ggplot(results %>% filter(exp1 == exp2))+
  geom_point(aes(x = name, y = diff))+
  geom_errorbar(aes(name,ymin = lwr, ymax = upr), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()+
  facet_wrap(.~ exp1)

################################################################################
## Height

tukey_result_height_raw <- tukeyHsdByTrait(summary_2023, "avg_height")
tukey_height<- prepTukeysDf(tukey_result_height_raw)

ggplot(tukey_height %>% filter(var1 == var2))+
  geom_point(aes(x = name, y = diff))+
  geom_errorbar(aes(name,ymin = lwr, ymax = upr), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()

ggplot(tukey_height %>% filter(exp1 == exp2))+
  geom_point(aes(x = name, y = diff))+
  geom_errorbar(aes(name,ymin = lwr, ymax = upr), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()+
  facet_wrap(.~ exp1)

################################################################################
## Basal Branches

aov_basal <- aov(basal_branches ~  variety * experiment,#+ Error(variety/week), 
                 data = basal_branches)
tukey_basal_raw <- TukeyHSD(aov_basal)
tukey_basal<- prepTukeysDf(tukey_basal_raw)

ggplot(tukey_basal %>% filter(var1 == var2))+
  geom_point(aes(x = name, y = diff))+
  geom_errorbar(aes(name,ymin = lwr, ymax = upr), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()

################################################################################
## Leaf Width

aov_width <- aov(leaf_width_cm ~  variety * experiment,#+ Error(variety/week), 
                 data = leaf_width)
tukey_width_raw <- TukeyHSD(aov_width)
tukey_width<- prepTukeysDf(tukey_width_raw)

ggplot(tukey_width %>% filter(var1 == var2))+
  geom_point(aes(x = name, y = diff))+
  geom_errorbar(aes(name,ymin = lwr, ymax = upr), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()

################################################################################
## Leaf Number

tukey_result_leafnum_raw <- tukeyHsdByTrait(summary_2023, "avg_leaf_num")
tukey_leafnum<- prepTukeysDf(tukey_result_leafnum_raw)

ggplot(tukey_leafnum %>% filter(var1 == var2))+
  geom_point(aes(x = name, y = diff))+
  geom_errorbar(aes(name,ymin = lwr, ymax = upr), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()
