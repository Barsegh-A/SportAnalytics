library(SportsAnalytics270)
library(tidyr)
library(ggplot2)
library(dplyr)


data("f_data_sm")

football_long <- f_data_sm %>%
  select(SEASON,DATE,HOMETEAM,AWAYTEAM,FTSC, COUNTRY) %>%
  gather(key='flag', value = 'Team',-c(SEASON, FTSC,DATE, COUNTRY)) %>%
  separate(FTSC, into = c('g1','g2'),sep = '-') %>%
  mutate(scored=ifelse(flag=='HOMETEAM',as.numeric(g1),as.numeric(g2)),
         conceded=ifelse(flag=='HOMETEAM',as.numeric(g2),as.numeric(g1)),
         WL=ifelse(scored>conceded,'W',ifelse(scored<conceded,"L",'D')),
         Points=ifelse(scored>conceded,3,ifelse(scored<conceded,0,1))) %>%
  select(-c(g1,g2))



football <- football_long %>% group_by(SEASON, Team, COUNTRY) %>% 
  summarise(total_points = sum(Points), max_points = 3*n(), 
            total_scored = sum(scored), total_conceded = sum(conceded)) %>%
  group_by(SEASON, COUNTRY) %>% 
  mutate(Position = dense_rank(desc(rank(total_points, ties.method = "first")))) %>% 
  mutate(Perc = total_points/max_points, GD = total_scored - total_conceded)

football <- football %>% drop_na()



HHI <- football %>%
  group_by(SEASON, COUNTRY) %>%
  mutate(Perc = total_points / sum(total_points)) %>%
  summarise(HHI = sum(Perc^2))

head(HHI)

min(football$SEASON):max(football$SEASON)

coefs <- data.frame() 

for(k in 1994:2019){

  league <- f_data_sm %>%
    group_by(COUNTRY) %>% 
    filter(SEASON == k) %>%
    select(COUNTRY, HOMETEAM, AWAYTEAM, FTHG, FTAG, )

  l1 <- data.frame(league[, c("COUNTRY", "HOMETEAM", "AWAYTEAM", "FTHG")], Home = 1)
  l2 <- data.frame(league[, c("COUNTRY", "AWAYTEAM", "HOMETEAM", "FTAG")], Home = 0)
  colnames(l1) <- c("COUNTRY", "Team", "Opponent", "Goal", "Home")
  colnames(l2) <- c("COUNTRY", "Team", "Opponent", "Goal", "Home")

  l2 <- rbind(l1, l2)
  
  l2 <- l2 %>% group_by(COUNTRY) %>% 
    summarise(ha = exp(coefficients(glm(Goal ~ Team + Opponent + Home, data = ., family=poisson(link=log)))["Home"]))
  
  head(l2)
  
  #model <- glm(Goal ~ Team + Opponent + Home, data = l2, family=poisson(link=log))
  # coefficients(model)

  coefs <<- rbind(coefs, data_frame(year = k, country = ha = exp(coefficients(model)["Home"])))

}




ggplot(coefs, aes(x = ha, y = HHI)) + geom_point() + geom_smooth(method = lm)
