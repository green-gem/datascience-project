mch_lbw_regression <- df2 %>% 
  filter(Year == 2016) %>%
  mutate(agricultural = ifelse(County %in% agro, 1, 0))

linmod <- lm(Average.Birth.Weight ~ Average.LMP.Gestational.Age, mch_regression)
summary(linmod)

mod1 <- lm(Average.Birth.Weight ~ factor(agricultural) + Average.LMP.Gestational.Age, mch_regression)
summary(mod1)

avg_bw_mod2016 <- lm(Average.Birth.Weight ~ factor(Mothers.Race, ordered = F) + Average.LMP.Gestational.Age, mch_regression)
summary(avg_bw_mod2016)

#parallel lines, Black and Asian/Pacific Island populations fare the worst
mch_regression %>%
  ggplot(aes(Average.LMP.Gestational.Age, Average.Birth.Weight, color = Mothers.Race)) + 
  geom_point() + geom_line(aes(y = predict(avg_bw_mod2016)))


plot(mod)
plot(mod1)
plot(mod2)
plot(mod3)

hist(rstudent(mod1), probability = TRUE, main = "Histogram of Externally Studentized Residuals", col = "pink")
curve(dnorm,from=-4,to=4,add=TRUE)
