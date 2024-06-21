library(emmeans)
source('src/preprocess.R')

##########################################
### PERFORM HYP TEST 
##########################################


descriptors <- c('variety', 'experiment', 'date', 'name', 'gbs')
feature <- c("height", "leaf_num", "buds", "flowers", "fruit", "basal", "width")
colnames_avg <- colnames(test%>% select(-all_of(descriptors)) ) %>% 
  grep("avg", ., value = TRUE, ignore.case = TRUE)

#####
### AVERGAGE
################################################################################

#######
### Height
#######
feature_col <- grep(feature[1], colnames_avg, value = TRUE, ignore.case = TRUE)

# Select necessary cols for height
df <- summary_2023 %>% select(all_of(c(descriptors, feature_col))) %>% 
  mutate(avg_height = as.numeric(avg_height)) %>%  # change to feature_col here
  filter(!is.na(avg_height)) # change to feature_col here

# Convert date to factor
df$date <- factor(df$date)

# Perform repeated measures ANOVA
# V1
anova_result <- aov(avg_height ~ experiment * date + Error(variety/date), data = df)
summary(anova_result)


# V2
anova_result2 <- df %>%
  anova_test(dv = avg_height, wid = variety, within = c(experiment, date))
print(anova_result2)

### Perform repeated measures ANOVA with Tukeys HSD
aov_model_simple <- aov(avg_height ~ experiment * date + variety, data = df)
# Extract estimated marginal means
emmeans_result <- emmeans(aov_model_simple, ~ experiment | variety)
# Perform pairwise comparisons with Tukey adjustment
tukey_result_condition <- pairs(emmeans_result, adjust = "tukey")
print(tukey_result_condition)


# Running for each individual 
# Perform ANOVA for each plant variety
anova_results <- list()
emmeans_results <- list()
plot_data <- data.frame()

for (variety in unique(df$variety)) {
  # Subset data for the current variety
  variety_data <- df %>% filter(variety == !!variety)
  
  # Check if there are at least two levels of 'experiment' with data
  if (length(unique(variety_data$experiment)) < 2) {
    cat("Variety:", variety, "- Not enough data for comparison\n")
    next
  }
  
  # Perform ANOVA
  aov_model <- aov(avg_height ~ experiment  , data = variety_data)
  anova_results[[variety]] <- summary(aov_model)
  
  # Perform Tukey's HSD test using emmeans package
  emmeans_model <- emmeans(aov_model, ~ experiment)
  emmeans_result <- pairs(emmeans_model, adjust = "tukey")
  emmeans_results[[variety]] <- emmeans_result
  
  # Combine with confidence intervals
  ci <- as.data.frame(confint(emmeans_result)) %>% 
    mutate(name = variety_data$name[[1]])
  
  # Append to plot_data
  plot_data <- bind_rows(plot_data, ci)
  
}

# Print results
for (variety in unique(df$variety)) {
  cat("Variety:", variety, "\n")
  print(anova_results[[variety]])
  print(emmeans_results[[variety]])
  cat("\n")
}

ggplot(plot_data)+
  geom_point(aes(x = name, y = estimate))+
  geom_errorbar(aes(name,ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()+
  ylab('Difference in SB and C')+
  xlab("")+
  ggtitle("Tukeys HSD on Difference in Plant Height")





#######
### Leaf Number
#######

# Select necessary cols for leaf Num
df <- summary_2023 %>% select(all_of(c(descriptors, 'avg_leaf_num'))) %>% 
  mutate(avg_leaf_num = as.numeric(avg_leaf_num)) %>%  # change to feature_col here
  filter(!is.na(avg_leaf_num)) # change to feature_col here

# Convert date to factor
df$date <- factor(df$date)

# Running for each individual 
# Perform ANOVA for each plant variety
anova_results <- list()
emmeans_results <- list()
plot_data <- data.frame()

for (variety in unique(df$variety)) {
  # Subset data for the current variety
  variety_data <- df %>% filter(variety == !!variety)
  
  # Check if there are at least two levels of 'experiment' with data
  if (length(unique(variety_data$experiment)) < 2) {
    cat("Variety:", variety, "- Not enough data for comparison\n")
    next
  }
  
  # Perform ANOVA
  aov_model <- aov(avg_leaf_num ~ experiment  , data = variety_data)
  anova_results[[variety]] <- summary(aov_model)
  
  # Perform Tukey's HSD test using emmeans package
  emmeans_model <- emmeans(aov_model, ~ experiment)
  emmeans_result <- pairs(emmeans_model, adjust = "tukey")
  emmeans_results[[variety]] <- emmeans_result
  
  # Combine with confidence intervals
  ci <- as.data.frame(confint(emmeans_result)) %>% 
    mutate(name = variety_data$name[[1]])
  
  # Append to plot_data
  plot_data <- bind_rows(plot_data, ci)
  
}

# Print results
for (variety in unique(df$variety)) {
  cat("Variety:", variety, "\n")
  print(anova_results[[variety]])
  print(emmeans_results[[variety]])
  cat("\n")
}

ggplot(plot_data)+
  geom_point(aes(x = name, y = estimate))+
  geom_errorbar(aes(name,ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()+
  ylab('Difference in SB and C')+
  xlab("")+
  ggtitle("Tukeys HSD on Difference in Leaf Number")










#######
### Basal Branch Number
######
# Select necessary cols for height
df <- summary_2023 %>% select(variety, experiment, avg_basal) %>% 
  mutate(avg_basal = as.numeric(avg_basal)) %>%  # change to feature_col here
  filter(!is.na(avg_basal)) # change to feature_col here

# Perform ANOVA for avg_basal across all varieties
aov_model <- aov(avg_basal ~ experiment, data = df)

# Perform emmeans (estimated marginal means)
emmeans_model <- emmeans(aov_model, ~ experiment)

# Perform Tukey's HSD test
tukey_result <- emmeans_model %>%contrast("pairwise", adjust = "tukey")

# Print summary
summary(aov_model)
print(tukey_result)
print(confint(tukey_result))



#######
### Leaf Width
######
# Select necessary cols for height
df <- summary_2023 %>% select(variety, experiment, avg_width) %>% 
  mutate(avg_width = as.numeric(avg_width)) %>%  # change to feature_col here
  filter(!is.na(avg_width)) # change to feature_col here

# Perform ANOVA for avg_basal across all varieties
aov_model <- aov(avg_width ~ experiment, data = df)

# Perform emmeans (estimated marginal means)
emmeans_model <- emmeans(aov_model, ~ experiment)

# Perform Tukey's HSD test
tukey_result <- emmeans_model %>%contrast("pairwise", adjust = "tukey")

# Print summary
summary(aov_model)
print(tukey_result)
print(confint(tukey_result))




#####
### RATE OF CHANGE
################################################################################

#######
### Height
#######
# Select necessary cols for height
df <- summary_2023 %>% select(all_of(c(descriptors, 'avg_height'))) %>% 
  mutate(avg_height = as.numeric(avg_height)) %>%  # change to feature_col here
  filter(!is.na(avg_height)) # change to feature_col here

# Calculate change in height
df %<>%
  arrange(variety, experiment, date) %>%
  group_by(variety, experiment) %>%
  mutate(delta_height = avg_height - lag(avg_height)) %>%
  na.omit()  # Remove rows where lagged value is NA

# Convert date to factor
df$date <- factor(df$date)


# Running for each individual 
# Perform ANOVA for each plant variety
anova_results <- list()
emmeans_results <- list()
plot_data <- data.frame()

for (variety in unique(df$variety)) {
  # Subset data for the current variety
  variety_data <- df %>% filter(variety == !!variety)
  
  # Check if there are at least two levels of 'experiment' with data
  if (length(unique(variety_data$experiment)) < 2) {
    cat("Variety:", variety, "- Not enough data for comparison\n")
    next
  }
  
  # Perform ANOVA
  aov_model <- aov(delta_height ~ experiment  , data = variety_data)
  anova_results[[variety]] <- summary(aov_model)
  
  # Perform Tukey's HSD test using emmeans package
  emmeans_model <- emmeans(aov_model, ~ experiment)
  emmeans_result <- pairs(emmeans_model, adjust = "tukey")
  emmeans_results[[variety]] <- emmeans_result
  
  # Combine with confidence intervals
  ci <- as.data.frame(confint(emmeans_result)) %>% 
    mutate(name = variety_data$name[[1]])
  
  # Append to plot_data
  plot_data <- bind_rows(plot_data, ci)
  
}

# Print results
for (variety in unique(df$variety)) {
  cat("Variety:", variety, "\n")
  print(anova_results[[variety]])
  print(emmeans_results[[variety]])
  cat("\n")
}

ggplot(plot_data)+
  geom_point(aes(x = name, y = estimate))+
  geom_errorbar(aes(name,ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()+
  ylab('Difference in SB and C')+
  xlab("")+
  ggtitle("Tukeys HSD on Difference in Plant Growth Change")










#######
### Leaf Number
#######
# Select necessary cols for height
df <- summary_2023 %>% select(all_of(c(descriptors, 'avg_leaf_num'))) %>% 
  mutate(avg_leaf_num = as.numeric(avg_leaf_num)) %>%  # change to feature_col here
  filter(!is.na(avg_leaf_num)) # change to feature_col here

# Calculate change in height
df %<>%
  arrange(variety, experiment, date) %>%
  group_by(variety, experiment) %>%
  mutate(delta_leaf_num = avg_leaf_num - lag(avg_leaf_num)) %>%
  na.omit()  # Remove rows where lagged value is NA

# Convert date to factor
df$date <- factor(df$date)


# Running for each individual 
# Perform ANOVA for each plant variety
anova_results <- list()
emmeans_results <- list()
plot_data <- data.frame()

for (variety in unique(df$variety)) {
  # Subset data for the current variety
  variety_data <- df %>% filter(variety == !!variety)
  
  # Check if there are at least two levels of 'experiment' with data
  if (length(unique(variety_data$experiment)) < 2) {
    cat("Variety:", variety, "- Not enough data for comparison\n")
    next
  }
  
  # Perform ANOVA
  aov_model <- aov(avg_leaf_num ~ experiment  , data = variety_data)
  anova_results[[variety]] <- summary(aov_model)
  
  # Perform Tukey's HSD test using emmeans package
  emmeans_model <- emmeans(aov_model, ~ experiment)
  emmeans_result <- pairs(emmeans_model, adjust = "tukey")
  emmeans_results[[variety]] <- emmeans_result
  
  # Combine with confidence intervals
  ci <- as.data.frame(confint(emmeans_result)) %>% 
    mutate(name = variety_data$name[[1]])
  
  # Append to plot_data
  plot_data <- bind_rows(plot_data, ci)
  
}

# Print results
for (variety in unique(df$variety)) {
  cat("Variety:", variety, "\n")
  print(anova_results[[variety]])
  print(emmeans_results[[variety]])
  cat("\n")
}

ggplot(plot_data)+
  geom_point(aes(x = name, y = estimate))+
  geom_errorbar(aes(name,ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()+
  ylab('Difference in SB and C')+
  xlab("")+
  ggtitle("Tukeys HSD on Difference in Rate of Change in Leaf Number")













#####
### DAYS UNTIL
################################################################################
# Filter to necessary columns
df <- summary_2023 %>% select(variety, experiment, date, avg_buds_num, 
                              avg_flowers_num, avg_fruit_num) %>% 
  mutate(avg_buds_num = as.numeric(avg_buds_num)) %>%  # change to feature_col here
  filter(!is.na(avg_buds_num)) # change to feature_col here

# Calculate the first week that had the first bud, flower, and fruit for
# each variety in each experiemnt
df  %<>% arrange(experiment, variety, date) %>%
  group_by(experiment, variety) %>%
  mutate(week_number = row_number()) %>% # label week 
  group_by(experiment, variety) %>%
  summarise(first_bud_week = ifelse(any(avg_buds_num != 0), 
                                    min(week_number[avg_buds_num != 0]), 19),
            first_flower_week = ifelse(any(avg_flowers_num != 0), 
                                       min(week_number[avg_flowers_num != 0]), 19),
            first_fruit_week = ifelse(any(avg_fruit_num != 0), 
                                      min(week_number[avg_fruit_num != 0]), 19))

#######
### Bud Num
#######
# Perform ANOVA for avg_basal across all varieties
aov_model <- aov(first_bud_week ~ experiment, data = df)

# Perform emmeans (estimated marginal means)
emmeans_model <- emmeans(aov_model, ~ experiment)

# Perform Tukey's HSD test
tukey_result <- emmeans_model %>%contrast("pairwise", adjust = "tukey")

# Print summary
summary(aov_model)
print(tukey_result)
print(confint(tukey_result))


#######
### Flower Num
#######
# Perform ANOVA for avg_basal across all varieties
aov_model <- aov(first_flower_week ~ experiment, data = df)

# Perform emmeans (estimated marginal means)
emmeans_model <- emmeans(aov_model, ~ experiment)

# Perform Tukey's HSD test
tukey_result <- emmeans_model %>%contrast("pairwise", adjust = "tukey")

# Print summary
summary(aov_model)
print(tukey_result)
print(confint(tukey_result))



#######
### Fruit Num
#######
# Perform ANOVA for avg_basal across all varieties
aov_model <- aov(first_fruit_week ~ experiment, data = df)

# Perform emmeans (estimated marginal means)
emmeans_model <- emmeans(aov_model, ~ experiment)

# Perform Tukey's HSD test
tukey_result <- emmeans_model %>%contrast("pairwise", adjust = "tukey")

# Print summary
summary(aov_model)
print(tukey_result)
print(confint(tukey_result))





#####
### CUMULATIVE
################################################################################
# Filter to columns of interest 
df <- summary_2023 %>% select(variety, experiment, date, avg_buds_num, 
                              avg_flowers_num, avg_fruit_num) %>% 
  mutate(avg_buds_num = as.numeric(avg_buds_num)) %>%  # change to feature_col here
  filter(!is.na(avg_buds_num)) # change to feature_col here

# Add cumulative counts
df %<>%
  arrange(experiment, variety, date) %>%
  group_by(experiment, variety) %>%
  mutate(cumulative_bud = cumsum(avg_buds_num),
         cumulative_flower = cumsum(avg_flowers_num),
         cumulative_fruit = cumsum(avg_fruit_num))

# !!! Q: double counting??
# would it make sense to look at straight avgs?



################################################################################
################################################################################
### AVG ###
################################################################################
################################################################################
# Height
tukey_result_height_raw <- tukeyHsdByTraitAvg(summary_2023, "avg_height")
tukey_height<- prepTukeysDf(tukey_result_height_raw)

tukey_height %<>% addVarName('var1')
tukey_height %<>% addVarName('var2')


ggplot(tukey_height %>% filter(var1 == var2))+
  geom_point(aes(x = var1_name, y = diff))+
  geom_errorbar(aes(var1_name,ymin = lwr, ymax = upr), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()+
  ylab('Difference in SB and C')+
  xlab("")+
  ggtitle("Tukeys HSD on Difference Plant Growth Rate")


# Leaf Num

# Bud Num

# Flower Num

# Fruit Num

# Basal Branch Num
aov_basal <- aov(basal_branches ~  variety * experiment,#+ Error(variety/week), 
                 data = basal_branches)
tukey_basal_raw <- TukeyHSD(aov_basal)
tukey_basal<- prepTukeysDf(tukey_basal_raw)

tukey_basal %<>% addVarName('var1')
tukey_basal %<>% addVarName('var2')

ggplot(tukey_basal %>% filter(var1 == var2))+
  geom_point(aes(x = var1_name, y = diff))+
  geom_errorbar(aes(var1_name,ymin = lwr, ymax = upr), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()+
  ylab('Difference in SB and C')+
  xlab("")+
  ggtitle("Tukeys HSD on Difference Basal Branch Number")


# Leaf Width Num
aov_width <- aov(leaf_width_cm ~  variety * experiment,#+ Error(variety/week), 
                 data = leaf_width)
tukey_width_raw <- TukeyHSD(aov_width)
tukey_width<- prepTukeysDf(tukey_width_raw)

tukey_width %<>% addVarName('var1')
tukey_width %<>% addVarName('var2')

ggplot(tukey_width %>% filter(var1 == var2))+
  geom_point(aes(x = var1_name, y = diff))+
  geom_errorbar(aes(var1_name,ymin = lwr, ymax = upr), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()+
  ylab('Difference in SB and C')+
  xlab("")+
  ggtitle("Tukeys HSD on Difference in Leaf Width")


################################################################################
################################################################################
### RATE OF CHANGE ###
################################################################################
################################################################################

## Height
tukey_result_height_raw <- tukeyHsdByTraitROC(summary_2023, "avg_height")
tukey_height<- prepTukeysDf(tukey_result_height_raw)

tukey_height %<>% addVarName('var1')
tukey_height %<>% addVarName('var2')


ggplot(tukey_height %>% filter(var1 == var2))+
  geom_point(aes(x = var1_name, y = diff))+
  geom_errorbar(aes(var1_name,ymin = lwr, ymax = upr), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()+
  ylab('Difference in SB and C')+
  xlab("")+
  ggtitle("Tukeys HSD on Difference Plant Growth Rate")


## Leaf Number
tukey_result_leafnum_raw <- tukeyHsdByTraitROC(summary_2023, "avg_leaf_num")
tukey_leafnum<- prepTukeysDf(tukey_result_leafnum_raw)

tukey_leafnum %<>% addVarName('var1')
tukey_leafnum %<>% addVarName('var2')

ggplot(tukey_leafnum %>% filter(var1 == var2))+
  geom_point(aes(x = var1_name, y = diff))+
  geom_errorbar(aes(var1_name,ymin = lwr, ymax = upr), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()+
  ylab('Difference in SB and C')+
  xlab("")+
  ggtitle("Tukeys HSD on Difference in Leaf Number")

# Bud Num
tukey_result_bud_raw <- tukeyHsdByTraitROC(summary_2023, "avg_buds_num")
tukey_bud<- prepTukeysDf(tukey_result_bud_raw)

tukey_bud %<>% addVarName('var1')
tukey_bud %<>% addVarName('var2')


ggplot(tukey_bud %>% filter(var1 == var2))+
  geom_point(aes(x = var1_name, y = diff))+
  geom_errorbar(aes(var1_name,ymin = lwr, ymax = upr), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()+
  ylab('Difference in SB and C')+
  xlab("")+
  ggtitle("Tukeys HSD on Difference in Bud Number")


# Flower Num
tukey_result_flower_raw <- tukeyHsdByTraitROC(summary_2023, "avg_flowers_num")
tukey_flower<- prepTukeysDf(tukey_result_flower_raw)

tukey_flower %<>% addVarName('var1')
tukey_flower %<>% addVarName('var2')


ggplot(tukey_flower %>% filter(var1 == var2))+
  geom_point(aes(x = var1_name, y = diff))+
  geom_errorbar(aes(var1_name,ymin = lwr, ymax = upr), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()+
  ylab('Difference in SB and C')+
  xlab("")+
  ggtitle("Tukeys HSD on Difference in Flower Number")


# Fruit Num
tukey_result_fruit_raw <- tukeyHsdByTraitROC(summary_2023, "avg_fruit_num")
tukey_fruit<- prepTukeysDf(tukey_result_flower_raw)

tukey_fruit %<>% addVarName('var1')
tukey_fruit %<>% addVarName('var2')


ggplot(tukey_fruit %>% filter(var1 == var2))+
  geom_point(aes(x = var1_name, y = diff))+
  geom_errorbar(aes(var1_name,ymin = lwr, ymax = upr), width = 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()+
  ylab('Difference in SB and C')+
  xlab("")+
  ggtitle("Tukeys HSD on Difference in Fruit Number")











#