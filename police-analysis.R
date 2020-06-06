source('styleguide.R')
library(stringr)

protests <- read.csv("data/blm-protests.csv")
killings <- read.csv("data/police-killings.csv")

killings_by_race <- data.frame(killings)
races <- killings_by_race$victim_race
for(i in 1:length(races)){
  if(!(races[i] %in% c("White", "Black", "Asian", "Hispanic"))){
    races[i] <- "Other/Unknown"
  }
}
killings_by_race$Race <- races

ggplot(data=killings_by_race, aes(x=factor(year))) + 
  geom_bar(aes(fill = factor(Race, levels = rev(c("White", "Black", "Hispanic", "Asian", "Other/Unknown")))), stat = "count") + 
  scale_fill_manual(values = rev(monochrome), guide = guide_legend(reverse = T)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) + 
  ylim(c(0, 1200)) + 
  theme_hodp() + 
  xlab("Year") + 
  ylab("Killings") + 
  labs(title="Police Killings Per Year", fill = "Victim Race")

# Add Logo
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

unarmed_killings_by_race <- killings_by_race[killings_by_race$weapon == "unarmed",]

ggplot(data=unarmed_killings_by_race, aes(x=factor(year))) + 
  geom_bar(aes(fill = factor(Race, levels = rev(c("White", "Black", "Hispanic", "Asian", "Other/Unknown")))), stat = "count") + 
  scale_fill_manual(values = rev(monochrome), guide = guide_legend(reverse = T)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) + 
  ylim(c(0, 200)) + 
  theme_hodp() + 
  xlab("Year") + 
  ylab("Killings") + 
  labs(title="Unarmed Police Killings Per Year", fill = "Victim Race")

grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

change_stats <- read.csv("relevant_change.csv")
normalized_change_stats <- read.csv("relevant_change_normalized.csv")

ggplot(data=change_stats, aes(x=protests, y=change)) + 
  geom_point(size = 2.5, color = monochrome[4]) + 
  geom_smooth(method = lm, se = F, color = monochrome[2], size = 2) +
  theme_hodp() + 
  theme( plot.subtitle = element_text(size=14,  family="Helvetica", color="#717171", face = "italic", margin = margin(t = 0, r = 0, b = 10, l = 0))) +
  xlab("Protests") + 
  ylab("Change in killings\n(2013-2015 to 2016-2018)") + 
  labs(title="Police killings vs protests", subtitle = "N = 96, Slope = -0.294, R2 = 0.1488, p = 0.0001")

grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

ggplot(data=normalized_change_stats, aes(x=protests, y=change)) + 
  geom_point(size = 2.5, color = monochrome[4]) + 
  geom_smooth(method = lm, se = F, color = monochrome[2], size = 2) +
  theme_hodp() + 
  theme( plot.subtitle = element_text(size=14,  family="Helvetica", color="#717171", face = "italic", margin = margin(t = 0, r = 0, b = 10, l = 0))) +
  xlab("Protests") + 
  ylab("Normalized change in killings\n(2013-2015 to 2016-2018)") + 
  labs(title="Police killings vs protests\n(normalized)", subtitle = "N = 96, Slope = -0.0116, R2 = 0.0843, p = 0.0041")

grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

black_change_stats <- read.csv("black_relevant_change.csv")

ggplot(data=black_change_stats, aes(x=protests, y=change)) + 
  geom_point(size = 2.5, color = monochrome[4]) + 
  geom_smooth(method = lm, se = F, color = monochrome[2], size = 2) +
  theme_hodp() + 
  theme( plot.subtitle = element_text(size=14,  family="Helvetica", color="#717171", face = "italic", margin = margin(t = 0, r = 0, b = 10, l = 0))) +
  xlab("Protests") + 
  ylab("Change in black killings\n(2013-2015 to 2016-2018)") + 
  labs(title="Black police killings vs protests", subtitle = "N = 32, Slope = -0.2006, R2 = 0.0802, p = 0.0181")

grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

normalized_black_change_stats <- read.csv("black_relevant_change_normalized.csv")
ggplot(data=normalized_black_change_stats, aes(x=protests, y=change)) + 
  geom_point(size = 2.5, color = monochrome[4]) + 
  geom_smooth(method = lm, se = F, color = monochrome[2], size = 2) +
  theme_hodp() + 
  theme( plot.subtitle = element_text(size=14,  family="Helvetica", color="#717171", face = "italic", margin = margin(t = 0, r = 0, b = 10, l = 0))) +
  xlab("Protests") + 
  ylab("Normalized change in black killings\n(2013-2015 to 2016-2018)") + 
  labs(title="Black police killings vs protests\n(normalized)", subtitle = "N = 32, Slope = -0.0121, R2 = 0.1293, p = 0.0433")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

change_stats$had_protests <- change_stats$protests > 0

ggplot(data = change_stats, aes(x=factor(had_protests), y=change)) + 
  geom_bar(stat = "summary", fun = "mean", fill = c(monochrome[1], monochrome[2]))  + 
  scale_x_discrete(labels = c("No Protests", "Protests")) + 
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", size=6, vjust = c(-0.5, 1.5)) + 
  coord_cartesian(ylim=c(-2, 1)) + 
  theme_hodp() + 
  theme( plot.subtitle = element_text(size=14,  family="Helvetica", color="#717171", face = "italic", margin = margin(t = 0, r = 0, b = 10, l = 0))) +
  ylab("Average change") + 
  xlab("") + 
  labs(title="Change in police killings vs protests", subtitle = "N=96, R2=0.0229, p = 0.1407")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

normalized_change_stats$had_protests <- normalized_change_stats$protests > 0

ggplot(data = normalized_change_stats, aes(x=factor(had_protests), y=change)) + 
  geom_bar(stat = "summary", fun = "mean", fill = c(monochrome[1], monochrome[2]))  + 
  scale_x_discrete(labels = c("No Protests", "Protests")) + 
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", size=6, vjust = c(-0.5, 1.5)) + 
  coord_cartesian(ylim=c(-0.1, 0.1)) + 
  theme_hodp() + 
  theme( plot.subtitle = element_text(size=14,  family="Helvetica", color="#717171", face = "italic", margin = margin(t = 0, r = 0, b = 10, l = 0))) +
  ylab("Average change (normalized)") + 
  xlab("") + 
  labs(title="Change in police killings vs protests\n(normalized)", subtitle = "N=96, R2=0.0302, p =  0.0902")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

normalized_protests_avg <- mean(normalized_change_stats$change[normalized_change_stats$protests > 0])
normalized_no_protests_avg <- mean(normalized_change_stats$change[normalized_change_stats$protests == 0])

extract_state <- Vectorize(function(city_str){return(strsplit(city_str, ", ")[[1]][[2]])})
normalized_change_stats$state <- extract_state(normalized_change_stats$city)
change_stats$state <- extract_state(change_stats$city)

state_turnouts <- read.csv("2012-turnout.csv")
normalized_change_stats <- merge(x = normalized_change_stats, y = state_turnouts, by = "state")
change_stats <- merge(x = change_stats, y = state_turnouts, by = "state")

summary(lm(change ~ protests + pres_turnout, data = normalized_change_stats))
summary(lm(change ~ protests + pres_turnout, data = change_stats))
