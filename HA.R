library(SportsAnalytics270)
library(dplyr)
library(tidyr)
library(ggplot2)



unique(f_data_sm$COUNTRY)



data("f_data_sm")

#year <- 2017
country <- "Portugal"

#unique(f_data_sm %>% filter(COUNTRY == "Belgium") %>% select(SEASON))

ha <- data.frame()

for(y in 1995 : 2019){

  belgium <- f_data_sm %>% filter(SEASON == y, COUNTRY == country)
  
  #belgium <- belgium %>% filter(!(HOMETEAM == "Partick" & AWAYTEAM == "Rangers") )
  
  belgium$ID = -1
  belgium$H_A = "A"

  j <- 1
  
  
  for(i in 1:dim(belgium)[1]){
    k <- belgium[belgium$HOMETEAM == belgium[i, "AWAYTEAM"] & belgium$AWAYTEAM == belgium[i, "HOMETEAM"],]
    
    #k <- k[1,]
    
    if(k$ID != -1){
      belgium$ID[i] = k$ID
      belgium$H_A[i] = "A"
    }else{
      belgium$ID[i] = j
      belgium$H_A[i] = "H"
      j <<- j + 1 
    }
    
    
  }
  
  
  
  belgium <- belgium %>% reshape(timevar = "H_A", idvar = "ID", direction = "wide", v.names = c("FTHG", "FTAG"))
  
  
  belgium <- belgium %>% mutate(C = sign((FTHG.H - FTAG.H) - (FTAG.A - FTHG.A)))
  
  head(belgium)
  
  
  
  
  
  #pbeta(1/2, 53, 98)
  
  belgium_new <- belgium %>% select(HOMETEAM, C)
  head(belgium_new)
  
  
  colnames(belgium_new) <- c("Team", "C")
  #belgium_new <- rbind(belgium_new, belgium_new1)
  
  belgium_new <- belgium_new %>% group_by(Team) %>% 
    count(C)
  
  head(belgium_new)
  
  #reshape(belgium_new, idvar = "Team", timevar = "C", direction = "wide")
  
  belgium_ha <- spread(belgium_new, key = "C", value = "n")
  colnames(belgium_ha) <- c("Team", "k_1", "k0", "k1")
  
  
  belgium_ha <- belgium_ha %>% mutate(k_1 = ifelse(is.na(k_1), 0, k_1), 
                        k0 = ifelse(is.na(k0), 0, k0),
                        k1 = ifelse(is.na(k1), 0, k1))
  
  all <- sum(belgium_ha$k_1) + sum(belgium_ha$k0) + sum(belgium_ha$k1)
  
  
  
  ha_y <- data.frame(SEASON = y, ha = 1 - pbeta(1/2, sum(belgium_ha$k1)/all + 1, sum(belgium_ha$k_1)/all + 1))
  ha <<- rbind(ha, ha_y)
  
}



#belgium_ha$ha <- 1 - pbeta(1/2, belgium_ha$k1 + 1, belgium_ha$k_1 + 1)

HHI <- football %>% filter(COUNTRY == country) %>% 
  group_by(SEASON) %>%
  mutate(Perc = total_scored / sum(total_scored)) %>%
  summarise(HHI = sum(Perc^2))


df <- left_join(ha, HHI)  
ggplot(df, aes(HHI, ha))  + geom_point() + geom_smooth(method = lm)

cor(df$ha, df$HHI)

mean(belgium_ha$ha)


###### ha_p is the home coefficinets calculated using Poisson model

ha_p <- l2 %>% filter(COUNTRY == "Scotland")

ggplot(ha_p, aes(SEASON, ha)) + geom_point() + geom_line()
ggplot(ha, aes(year,  ha)) + geom_point() + geom_line()
  







