# Summary table for group by month

dat2 <- as.data.frame(cleaned_dat2 %>% 
  group_by(month_year) %>% 
  summarise(Ottawa.y.fracture.y = sum(Ottawa.y.fracture.y == 1),
            Ottawa.n.fracture.y = sum(Ottawa.n.fracture.y == 1),
            Ottawa.y.fracture.n = sum(Ottawa.y.fracture.n == 1),
            Ottawa.n.fracture.n = sum(Ottawa.n.fracture.n == 1),
            total = n()))

# Inappropriate X rays would be those who didn't fit the Ottawa rules

dat2$inappropriate_xr <- (((dat2$Ottawa.n.fracture.n + dat2$Ottawa.n.fracture.y)/ dat2$total) * 100)

# Now, let's plot inappropriate x rays over time to see if there is any
# temporal trend

library(ggplot2)

plot1 <- ggplot(dat2, aes(x = as.Date(paste(month_year, "01", sep = "-")), y = inappropriate_xr)) +
  geom_line(lwd = 0.6, col='blue') +
  labs(x = "Year", y = "% of Unnecessary X Rays",
       title = "Line plot showing trend in % inappropriate X Ray requests under the 
Ottawa Ankle Rules at CUH 2015-2022") +
  theme_minimal() + 
  geom_smooth(col = 'black')


plot1

ggplotly(plot1)

# Convert the 'Date' column to a Date format
dat2$Date <- as.Date(paste0(dat2$month_year, "-01"), format = "%Y-%m-%d")  # Adding day to ensure proper Date format

# Extract the year from the 'Date' column
dat2$Year <- format(dat2$Date, "%Y")

# Summarize by year
summary_by_year <- dat2 %>%
  group_by(Year) %>%
  summarise(mean = mean(inappropriate_xr))

# View the summary
print(summary_by_year)
