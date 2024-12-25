# Load the ggplot2 package
library(ggplot2)
library(ggsci)

# Create a data frame with the exclusion reasons and their counts
exclusion_data <- data.frame(
  Reason = c("Aged younger than 2", "Forefoot presentation", "Post manipulation", 
             "Non-traumatic Presentation", "Duplicate records", "Other"),
  Count = c(128, 3990, 1805, 1204, 4518, 107)
)


order = c('Forefoot presentation', 'Non-traumatic Presentation', 'Duplicate records',
          'Post manipulation', 'Aged younger than 2', 'Other')

exclusion_data$Reason <- factor(exclusion_data$Reason, levels = order)

# Add the total excluded at the end for the visualization
total_excluded <- sum(exclusion_data$Count)

ggplot(exclusion_data, aes(x = Reason, y = Count, fill = Reason)) +
  geom_bar(stat = "identity") +  # Removed manual fill color
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = 'Bar chart showing excluded X-ray reports by reason',
       x = "Reason for Exclusion",
       y = "Count",
       fill = 'Reason for Exclusion') +
  scale_fill_jama() + 
  theme_minimal() +
  theme(axis.text.x = element_blank())



