library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(splines)
library(MASS)
library(scales)

#x11()

myTheme <- theme(panel.grid.major = element_line(colour = "gray90", size = 0.3), 
                 panel.grid.minor = element_line(colour = "gray90", size = 0.2), 
                 axis.title = element_text(family = "Helvetica"), 
                 plot.title = element_text(family = "Helvetica", hjust = 0.5), 
                 panel.background = element_rect(fill = NA), 
                 plot.caption = element_text(family = "Helvetica", size = 7))

# Read and graph drink dates
drinks <- read.csv("./data/drinkLog.csv")
n.unique.drink.types <- length(unique(drinks$Type))
drink.type.colors <- brewer.pal(n.unique.drink.types, "Set1")
names(drink.type.colors) <- levels(drinks$Type)
drink.col.scale <- scale_color_manual(name = "Type", values = drink.type.colors)

drinks$Date <- as.Date(drinks$Date, '%m-%d-%Y')  # Format as a date object
drinks$alcohol_vol_oz <- (drinks$ABV/100) * drinks$Volume
drinks$n_std_drinks <- drinks$alcohol_vol_oz / 0.6  # a standard drink is 0.6 oz of pure ethanol

# Get total alcohol consumed per day
alc_by_date <- aggregate(drinks$alcohol_vol_oz, by=list(Category=drinks$Date), FUN=sum)
colnames(alc_by_date) <- c("Date", "alcohol_vol_oz")
alc_by_date$n_std_drinks <- alc_by_date$alcohol_vol_oz / 0.6

# Get beer types consumed per day
#type_by_date <- ddply(drinks$Type, names(drinks$Type), summarize, Freq=length(drinks$Type))

# Weekly data
drinks$week <- format(drinks$Date, "%W")
alc_by_date$week <- format(alc_by_date$Date, "%W")
sum_weekly_std_drinks <- aggregate(drinks$n_std_drinks, list(drinks$week), sum)
mean_daily_std_drinks <- aggregate(drinks$n_std_drinks, list(drinks$week), mean)

colnames(sum_weekly_std_drinks) <- c("WeekN", "tot.std.drinks")
colnames(mean_daily_std_drinks) <- c("WeekN", "mean.std.drinks")

#alc_by_date <- drinks %>%
#  group_by(Date) %>%
#  summarise(alcohol_vol_oz = sum(alcohol_vol_oz), 
#            avg_abv = mean(ABV),
#            n_std_drinks = sum(n_std_drinks))

alc_vol_graph <- ggplot(data = alc_by_date, aes(x = Date, y = alcohol_vol_oz)) +
  geom_point() +
  geom_line() +
  labs(title = "Alc. Oz by Date", y = "Oz. Alcohol")

alc_vol_graph_week <- ggplot(data = sum_weekly_std_drinks, aes(x = WeekN, y = tot.std.drinks, group=1)) +
  geom_line() +
  geom_point() +
  labs(title = "Standard Drinks by Week", y = "Standard Drinks")

std_drinks_graph <- ggplot() +
  geom_line(data = alc_by_date, aes(y = n_std_drinks, x = Date, group = 1)) +
  geom_point(data = alc_by_date, aes(y = n_std_drinks, x = Date, group = 1)) +
  labs(y = "N. Standard Drinks", title = "Standard Drinks by Date") +
  labs(caption = "1 Standard drink is the equivalent of 0.6 oz pure alcohol (https://www.cdc.gov/alcohol/faqs.htm#standard)") +
  myTheme

abv_boxplot <- ggplot() +
  geom_boxplot(data = drinks, aes(x=1, y=ABV, group=1)) +
  labs(y="Alcohol by Volume") +labs(title = "Alcohol Content of Beers I've Had") +
  myTheme

# Do beer types by avg temp
beer_type_rating <- ggplot(drinks, aes(x = Type, y = MyRating)) +
  geom_boxplot() +
  myTheme

# More frequent beers first in graph
beer.type.graph.data <- within(drinks,
                               Type <- factor(Type,
                                                  levels = names(sort(table(Type),
                                                                      decreasing = TRUE))))

beer_type_number <- ggplot(beer.type.graph.data, aes(x = Type, fill = Type)) +
  geom_bar() +
  myTheme +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none") + # hide legend here.
  labs(y = "Count", title = "Beer Type Frequency")

brewery_ratings <- ggplot(drinks, aes(x = Brewery, y = MyRating)) +
  geom_boxplot() +
  myTheme

# Beer IBU and Date
beer.type.date <- ggplot(drinks, aes(x = Date, y = IBU, size = ABV)) +
  geom_point(aes(color = Type)) +
  scale_x_date(labels = date_format('%d %b %Y')) +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 5, fx = F, bs = "tp"), size = 1,
              se = T, fullrange = TRUE, level = 0.95, linetype = "solid",
              show.legend = FALSE,
              alpha = 0.2) +
  drink.col.scale +
  myTheme +
  theme(legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill=NA)) +
  labs(title = "Beer International Bitterness Units and Alcohol Content Over Time")

ibu.density <- ggplot(drinks, aes(x = IBU)) +
  geom_density(alpha = 0.5, aes(fill = "#d8002a")) +
  myTheme +
  #scale_fill_brewer(palette = "Set1") +
  scale_fill_manual(values = "#d8002a") +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(title = "IBU Distribution")

date.density <- ggplot(drinks, aes(x = Date)) +
  geom_density(alpha = 0.5, aes(fill = "#d8002a")) +
  #scale_fill_brewer(palette = "Set1") +
  scale_fill_manual(values = "#d8002a") +
  theme(legend.position = "none") +
  myTheme +
  scale_x_date(labels = date_format('%d %b %Y')) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(title = "Drinking Frequency")

abv.density <- ggplot(drinks, aes(x = ABV)) +
  geom_density(alpha = 0.5,
               aes(fill = "#d8002a")) +
  #scale_fill_brewer(palette = "Set1") +
  scale_fill_manual(values = "#d8002a") +
  theme(legend.position = "none") +
  myTheme +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(title = "ABV Distribution")

blankPlot <- ggplot() +
  geom_blank(aes(1, 1)) +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
        ) 
fullPlot <- grid.arrange(ibu.density, abv.density, beer.type.date, date.density,
             ncol = 2, nrow = 2, widths = c(4, 1.4), heights = c(1.4, 4),
             top = textGrob("Beer Drinking Habits",
                            gp = gpar(fontface = "bold",
                                      fontsize = 16)),
             bottom = textGrob("Ross Wardrup",
                               x = .95,
                               y= 0.5,
                               just = 'center',
                               gp = gpar(fontsize = 8)))
ggsave(file="beerTime.png", fullPlot, width = 16, height = 12, units = 'in', dpi = 300)
