# California Department of Food and Agriculture 
# California Agriculture Statistics Review
# Top 10 Agriculture Counties

agro <- c("Kern", "Tulare", "Fresno", "Monetery", "Merced", "Stanislaus", 
          "San Joaquin", "Ventura", "Imperial")

#the top 10 agriculture counties acts as a proxy for pesticide usage
#almost all of the top 10 counties in the list were the highest ranked in pesticide
#usage, and most are located in the San Jaoquin Vallry

#only focused on 2019 to be consistent with other analyses
#don't have the knowledge to deal with longitudinal data yet

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

#combining the low birth weight and preterm birth data sets
lbw_pt <- full_join(lbw_regression, ptbirth_regression, by = c("Year", "County", "agricultural"))

#visualize difference in birth weight by race
MCH.CDC.Data_Race %>% mutate(agricultural = ifelse(County %in% agro, 1, 0)) %>%
  ggplot(aes(Year, Average.Birth.Weight, color = Mothers.Race)) + 
  geom_point()

mod1 <- lm(Average.Birth.Weight ~ factor(agricultural) + Average.LMP.Gestational.Age, mch_regression)
summary(mod1)

#includes interaction term with county and gestational age
mod2 <- lm(Average.Birth.Weight ~ factor(agricultural) + Average.LMP.Gestational.Age + factor(agricultural)*Average.LMP.Gestational.Age, mch_regression)
summary(mod2)

#includes quadratic term for gestational age
lbw_lmp_age2 = (mch_regression$Average.LMP.Gestational.Age)^2
mod3 <- lm(Average.Birth.Weight ~ factor(agricultural) + Average.LMP.Gestational.Age + lbw_lmp_age2, mch_regression)
summary(mod3)

#loess smoothers, show overall relationship
mch_regression %>%
  ggplot(aes(Average.LMP.Gestational.Age, Average.Birth.Weight, color = factor(agricultural))) + 
  geom_point() + geom_smooth(se = F)

#parallel lines, with agricultural counties being lower
mch_regression %>%
  ggplot(aes(Average.LMP.Gestational.Age, Average.Birth.Weight, color = factor(agricultural))) + 
  geom_point() + geom_line(aes(y = predict(mod1)))

#interaction term
mch_regression %>%
  ggplot(aes(Average.LMP.Gestational.Age, Average.Birth.Weight, color = factor(agricultural))) + 
  geom_point() + geom_line(aes(y = predict(mod2)))

#quadratic model, still parallel
mch_regression %>%
  ggplot(aes(Average.LMP.Gestational.Age, Average.Birth.Weight, color = factor(agricultural))) + 
  geom_point() + geom_line(aes(y = predict(mod3)))


avg_bw_mod2016 <- lm(Average.Birth.Weight ~ factor(Mothers.Race, ordered = F) + Average.LMP.Gestational.Age, mch_regression)
summary(avg_bw_mod2016)

#parallel lines, Black and Asian/Pacific Island populations fare the worst
mch_regression %>%
  ggplot(aes(Average.LMP.Gestational.Age, Average.Birth.Weight, color = Mothers.Race)) + 
  geom_point() + geom_line(aes(y = predict(avg_bw_mod2016)))

df2 %>% mutate(agricultural = ifelse(County %in% agro, 1, 0)) %>%
  filter(Year == 2019) %>%
  ggplot(aes(Average.LMP.Gestational.Age, Average.Birth.Weight, color = factor(agricultural))) + 
  geom_point()


#checking assumptions for the models, they seem to fit well, so thats good
plot(avg_bw_mod2016)
plot(mod1)
plot(mod2)
plot(mod3)

exstures0 <- rstudent(avg_bw_mod2016)
hist(exstures0, probability = TRUE, main = "Histogram of Externally Studentized Residuals", col = "pink")
curve(dnorm,from=-4,to=4,add=TRUE)

exstures1 <- rstudent(mod1)
hist(exstures1, probability = TRUE, main = "Histogram of Externally Studentized Residuals", col = "pink")
curve(dnorm,from=-4,to=4,add=TRUE)

exstures2 <- rstudent(mod2)
hist(exstures2, probability = TRUE, main = "Histogram of Externally Studentized Residuals", col = "pink")
curve(dnorm,from=-4,to=4,add=TRUE)

exstures3 <- rstudent(mod3)
hist(exstures3, probability = TRUE, main = "Histogram of Externally Studentized Residuals", col = "pink")
curve(dnorm,from=-4,to=4,add=TRUE)

# White mothers
white_mod1 <- lm(Average.Birth.Weight ~ factor(agricultural) + Average.LMP.Gestational.Age + factor(agricultural)*Average.LMP.Gestational.Age, filter(mch_regression, Mothers.Race == "White"))
summary(white_mod1)

mch_regression %>%
  filter(Mothers.Race == "White") %>%
  ggplot(aes(Average.LMP.Gestational.Age, Average.Birth.Weight, color = factor(agricultural))) + 
  geom_point() + geom_line(aes(y = predict(white_mod1)))

#american indian/alaska native
amerindian_mod1 <- lm(Average.Birth.Weight ~ factor(agricultural) + Average.LMP.Gestational.Age, filter(mch_regression, Mothers.Race == "American Indian or Alaska Native"))
summary(amerindian_mod1)

#parallel lines, Black and Asian/Pacific Island populations fare the worst
mch_regression %>%
  filter(Mothers.Race == "American Indian or Alaska Native") %>%
  ggplot(aes(Average.LMP.Gestational.Age, Average.Birth.Weight, color = factor(agricultural))) + 
  geom_point() + geom_line(aes(y = predict(amerindian_mod1)))

#asian mothers
asian_mod1 <- lm(Average.Birth.Weight ~ factor(agricultural) + Average.LMP.Gestational.Age+ factor(agricultural)*Average.LMP.Gestational.Age, filter(mch_regression, Mothers.Race == "Asian or Pacific Islander"))
summary(asian_mod1)

#parallel lines, Black and Asian/Pacific Island populations fare the worst
mch_regression %>%
  filter(Mothers.Race == "Asian or Pacific Islander") %>%
  ggplot(aes(Average.LMP.Gestational.Age, Average.Birth.Weight, color = factor(agricultural))) + 
  geom_point() + geom_line(aes(y = predict(asian_mod1)))

# black mothers
black_mod1 <- lm(Average.Birth.Weight ~ factor(agricultural) + Average.LMP.Gestational.Age+ factor(agricultural)*Average.LMP.Gestational.Age, filter(mch_regression, Mothers.Race == "Black or African American"))
summary(black_mod1)

#parallel lines, Black and Asian/Pacific Island populations fare the worst
mch_regression %>%
  filter(Mothers.Race == "Black or African American") %>%
  ggplot(aes(Average.LMP.Gestational.Age, Average.Birth.Weight, color = factor(agricultural))) + 
  geom_point() + geom_line(aes(y = predict(black_mod1)))


