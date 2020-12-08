# 
# 
# 
# pesticide_averagebw_join %>% ggplot(aes(rate, value)) + geom_point(aes(color=County))
# 
# pesticide_averagebw_join %>% 
#   filter(County %in% c("Fresno","Kern","San Joaquin", "Kings", "Madera", "Tulare")) %>% ggplot(aes(rate, value)) + geom_point(aes(color=County))
# pesticide_averagept_join %>% 
#   filter(County %in% c("Fresno","Kern","San Joaquin", "Kings", "Madera", "Tulare")) %>% 
#   ggplot(aes(rate, value)) + geom_point(aes(color=County))



# California Department of Food and Agriculture 
# California Agriculture Statistcs Review
# Top 10 Agriculture Counties

agro <- c("Kern", "Tulare", "Fresno", "Monetery", "Merced", "Stanislaus", 
          "San Joaquin", "Ventura", "Imperial")


mch_regression <- MCH.CDC.Data_Race %>% 
  filter(Year == 2016) %>%
  mutate(agricultural = ifelse(County %in% agro, 1, 0))

mch_lbw_regression <- df2 %>% 
  filter(Year == 2016) %>%
  mutate(agricultural = ifelse(County %in% agro, 1, 0))


lbw_regression <- lbwdata %>% 
  filter(Year == 2016) %>%
  mutate(agricultural = ifelse(County %in% agro, 1, 0)) %>%
  rename(lbw_births = Events, rate_lbw = Rate)

ptbirth_regression <- ptbirthdata %>% 
  filter(Year == 2016) %>%
  mutate(agricultural = ifelse(County %in% agro, 1, 0)) %>%
  rename(pt_births = Events)

lbw_pt <- full_join(lbw_regression, ptbirth_regression, by = c("Year", "County", "agricultural"))

lbw_pt %>% ggplot(aes(rate_lbw, rate_pt, color = factor(agricultural))) + 
  geom_point()
lbw_pt %>% ggplot(aes(rate_lbw, rate_pt,)) + 
  geom_point() + geom_smooth()

lbw_pt %>% ggplot(aes(rate_pt, agricultural)) + 
  geom_point()
lbw_pt %>% ggplot(aes(rate_lbw, agricultural)) + 
  geom_point()



ptmod <- glm(agricultural ~ rate_pt, lbw_pt, family = binomial())
summary(ptmod)                  

lbwmod <- glm(agricultural ~ rate_lbw, lbw_pt, family = binomial())
summary(lbwmod) 

lbw_ptmod <- glm(agricultural ~ rate_pt + rate_lbw, lbw_pt, family = binomial())
summary(lbw_ptmod)  

lbw_ptmod2 <- glm(agricultural ~ rate_pt + rate_lbw + rate_pt*rate_lbw, lbw_pt, family = binomial())
summary(lbw_ptmod2)  

 
lbw_pt %>% ggplot(aes(rate_pt, agricultural)) + 
  geom_point() +
  geom_line(aes(y = predict(ptmod, type = "response")))


df2 %>% mutate(agricultural = ifelse(County %in% agro, 1, 0)) %>%
  ggplot(aes(Year, Average.Birth.Weight, color = County)) + 
  geom_point() 

df2 %>% mutate(agricultural = ifelse(County %in% agro, 1, 0)) %>%
  ggplot(aes(Year, Average.Birth.Weight, color = factor(agricultural))) + 
  geom_point()

MCH.CDC.Data_Race %>% mutate(agricultural = ifelse(County %in% agro, 1, 0)) %>%
  ggplot(aes(Year, Average.Birth.Weight, color = Mothers.Race)) + 
  geom_point()

MCH.CDC.Data_Race %>% mutate(agricultural = ifelse(County %in% agro, 1, 0)) %>%
  ggplot(aes(Year, Average.Birth.Weight, color = factor(agricultural))) + 
  geom_point()



mch_regression %>%
  ggplot(aes(Average.LMP.Gestational.Age, Average.Birth.Weight, color = Mothers.Race)) + 
  geom_point() + geom_line(aes(y = predict(avg_bw_mod2016)))


mod1 <- lm(Average.Birth.Weight ~ agricultural, mch_regression)
summary(mod1)

avg_bw_mod2016 <- lm(Average.Birth.Weight ~ factor(Mothers.Race, ordered = F) + Average.LMP.Gestational.Age, mch_regression)
summary(avg_bw_mod2016)

avg_bw_mod2007 <- lm(Average.Birth.Weight ~ factor(Mothers.Race, ordered = F) + Average.LMP.Gestational.Age, filter(MCH.CDC.Data_Race, Year == 2007))
summary(avg_bw_mod2007)
avg_bw_mod2010 <- lm(Average.Birth.Weight ~ factor(Mothers.Race, ordered = F) + Average.LMP.Gestational.Age, filter(MCH.CDC.Data_Race, Year == 2010))
summary(avg_bw_mod2010)
avg_bw_mod2013 <- lm(Average.Birth.Weight ~ factor(Mothers.Race, ordered = F) + Average.LMP.Gestational.Age, filter(MCH.CDC.Data_Race, Year == 2013))
summary(avg_bw_mod2013)
avg_bw_mod2019 <- lm(Average.Birth.Weight ~ factor(Mothers.Race, ordered = F) + Average.LMP.Gestational.Age, filter(MCH.CDC.Data_Race, Year == 2019))
summary(avg_bw_mod2019)


MCH.CDC.Data_Race %>% filter(Year == 2013) %>%
  ggplot(aes(Average.LMP.Gestational.Age, Average.Birth.Weight, color = Mothers.Race)) + 
  geom_point() + geom_line(aes(y = predict(avg_bw_mod2013)))
MCH.CDC.Data_Race %>% filter(Year == 2010) %>%
  ggplot(aes(Average.LMP.Gestational.Age, Average.Birth.Weight, color = Mothers.Race)) + 
  geom_point() + geom_line(aes(y = predict(avg_bw_mod2010)))
MCH.CDC.Data_Race %>% filter(Year == 2007) %>%
  ggplot(aes(Average.LMP.Gestational.Age, Average.Birth.Weight, color = Mothers.Race)) + 
  geom_point() + geom_line(aes(y = predict(avg_bw_mod2007)))
MCH.CDC.Data_Race %>% filter(Year == 2019) %>%
  ggplot(aes(Average.LMP.Gestational.Age, Average.Birth.Weight, color = Mothers.Race)) + 
  geom_point() + geom_line(aes(y = predict(avg_bw_mod2019)))
