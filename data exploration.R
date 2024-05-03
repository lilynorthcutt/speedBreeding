source('src/preprocess.R')

######################
### Explore Data ###
######################
# Summarize based on 23C
sumHeight <- height %>% group_by(entryP1, date, exp) %>% 
  summarize(heightAvg = mean(height, na.rm = T), 
            stdev = sd(height, na.rm = T)) 


# Calculate avg increase rate



#
lm_fit_sb <- lm(log(heightAvg) ~date, data = sumHeight %>% filter(exp == 'sb'))
lm_fit_c <- lm(log(heightAvg) ~date, data = sumHeight %>% filter(exp == 'c'))

summary(lm_fit_sb)
summary(lm_fit_c)

lm_fit_sb_23C240 <- lm(log(heightAvg) ~date, data = sumHeight %>% filter(exp == 'sb', entryP1 == '23C340'))
lm_fit_c_23C240 <- lm(log(heightAvg) ~date, data = sumHeight %>% filter(exp == 'c', entryP1 == '23C340'))
summary(lm_fit_sb_23C240)
summary(lm_fit_c_23C240)

# Plot
ggplot(sumHeight) +
  geom_point(aes(x = date, y = heightAvg, color = exp))+
  geom_errorbar(aes(x = date, y = heightAvg, ymin = heightAvg - stdev, ymax = heightAvg + stdev, color = exp))+
  facet_wrap(~entryP1)+
  ggtitle("Plant Height")+
  xlab("Date")+ylab("Avg Height")

ggplot(sumHeight) +
  geom_point(aes(x = date, y = log(heightAvg), color = exp))+
  #geom_errorbar(aes(x = date, y = log(heightAvg), ymin = log(heightAvg - stdev), ymax = log(heightAvg + stdev), color = exp))+
  facet_wrap(~entryP1)+
  ggtitle("Plant Height")+
  xlab("Date")+ylab("Log Avg Height")+
  geom_smooth(aes(x = date, y = log(heightAvg), color = exp),method = "lm", formula = y~x)

ggplot(sumHeight %>% group_by(exp, date) %>% summarize(heightAvg = mean(heightAvg, na.rm = TRUE)))+
  geom_point(aes(x = date, y = heightAvg, color = exp))

#