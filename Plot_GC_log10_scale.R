
# PLOT INDIVIDUAL GROWTH CURVE IN LOG10 SCALE

> install.packages(ggplot2)
> library(ggplot2)

# For example: growth in well G1

# Subset column "time" and "G1", then plot
> G1 <- subset (d, select = c('time','G1'))

> ggplot(G1, aes(x=time, y=G1)) + 
  geom_point() + 
  scale_y_log10() + 
  labs(y="log(OD600)",x="Time (hour)")



# PLOT AVERAGE OF BIO REPS IN LOG10 SCALE

> install.packages("tidyr")
> library(tidyr)

# Subset column "time" and the target wells
> G1to5 <- subset(d, select = c('time', 'G1', 'G2', 'G3', 'G4', 'G5'))

# Convert to long version of data set
> long_G1to5 <- G1to5 %>% pivot_longer(cols = c('G1', 'G2', 'G3', 'G4', 'G5'), 
                                     names_to = 'well', 
                                     values_to = 'OD')

# Plot
> ggplot(long_G1to5, aes(time,OD)) + 
  stat_summary(geom='point', fun.y=mean) + 
  scale_y_log10() + 
  labs(x='Time (hour)', y='log(OD600)')


# Line 12-15 in detail:
# Plot regular scale growth
> plot_G1 <- ggplot(G1, aes(x = time, y = G1)) + geom_point()
# If want color (for example, red)
> plot_G1 <- ggplot(G1, aes(x = time, y = G1)) + geom_point(color = 'red')
# Turn into log scale
> plot_G1 + scale_y_log10()
# Change y axis label to log(OD600)
> print(plot_G1 + scale_y_log10() + labs(y = "log(OD600)"))
# Change x axis label to Time (hour)
print(plot_G1 + scale_y_log10() + labs(y = "log(OD600)", x = "Time (hour)"))



