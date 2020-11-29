library(BradleyTerry2)
library(purrr)

#Take HHI from previous scratch file.


f_bt <- f_data_sm %>% mutate(ht_w = ifelse(FTR == "H", 1,
                                  ifelse(FTR == "D", 0.5, 0)),
                    at_w = ifelse(FTR == "A", 1,
                                  ifelse(FTR == "D", 0.5, 0)))
head(f_bt)


f_bt1 <- f_bt %>%
  mutate(HOMETEAM = as.factor(HOMETEAM),
         AWAYTEAM = as.factor(AWAYTEAM)) %>%
  group_by(HOMETEAM, AWAYTEAM, SEASON) #%>%
  #mutate(ht = sum(ht_w), at=sum(at_w))

head(f_bt1)

f_bt1$HOMETEAM <- data.frame(team = f_bt1$HOMETEAM, at.home = 1)
f_bt1$AWAYTEAM <- data.frame(team = f_bt1$AWAYTEAM, at.home = 0)

head(f_bt1)

f_bt1 <- f_bt1 %>% group_by(COUNTRY, SEASON)
f_bt1 <- f_bt1 %>% do(ha = coefficients(BTm(cbind(ht_w, at_w), HOMETEAM, AWAYTEAM, formula = ~team + at.home, data = ., id="team"))["at.home"])

f_bt2 <- f_bt1

f_bt1$ha <- flatten_dbl(f_bt1$ha)

head(f_bt1)
head(HHI)

bt_cb <- left_join(f_bt1, HHI, by = c("COUNTRY", "SEASON"))
head(bt_cb)

without_sct <- bt_cb %>% filter(COUNTRY != "Scotland")

ggplot(bt_cb, aes(x = ha, y = HHI)) + geom_point() + geom_smooth(method = lm) + facet_wrap(~COUNTRY, nrow = 6, ncol = 5)
cor(without_sct$ha, without_sct$HHI)
