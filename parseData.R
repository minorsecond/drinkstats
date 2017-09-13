library(dplyr)
library(ggplot2)

# Read and graph drink dates
drinks <- read.csv("./data/drinkLog.csv")
drinks$alcohol_vol_oz <- (drinks$ABV/100) * drinks$Volume
drinks$n_std_drinks <- drinks$alcohol_vol_oz / 0.6  # a standard drink is 0.6 oz of pure ethanol

# Get total alcohol consumed per day
alc_by_date <- aggregate(drinks$alcohol_vol_oz, by=list(Category=drinks$Date), FUN=sum)

alc_by_date <- drinks %>%
  group_by(Date) %>%
  summarise(alcohol_vol_oz = sum(alcohol_vol_oz), 
            avg_abv = mean(ABV),
            n_std_drinks = sum(n_std_drinks))

alc_vol_graph <- ggplot() +
  geom_line(data = alc_by_date, aes(y = alcohol_vol_oz, x = Date, group = 1)) +
  labs(title = "Alc. Oz by Date", y = "Oz. Alcohol")
