# Exploring why some studies were excluded

dat$Why.is.this.study.excluded <- as.factor(dat$Why.is.this.study.excluded)

dat4 <- as.data.frame(table(dat$Why.is.this.study.excluded))

dat4 <- dat4[4:40,] # Remove the count for those requests which were not excluded

# Following tidying of data by ChatGPT

dat4.1 <- data.frame(
  Reason = c("age", "age fb", "duplicate", "fb", "forefoot", "forefoot age", "forefoot nc", "forefooy", 
           "nc", "non-traumatic", "other", "pm", "repeat"),
  Freq = c(157, 1, 110, 451, 4668, 1, 1, 1, 3141, 1403, 170, 2116, 647)
)

ggplot(dat4.1, aes(x = Reason, y = Freq, fill = Reason)) +
  geom_bar(stat = "identity") +
  labs(x = "Reason", y = "Frequency",
       title = "Bar Chart Showing Reasons for 
Excluding Requests from Database") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

ggplot(cleaned_dat) + geom_histogram(aes(x = Age.at.Exam), fill = 'lightblue', 
                             color = 'black', alpha = 0.7,
                             binwidth = 5) + 
  labs(x = 'Age of Patient',
       title = 'Histogram showing age of patients undergoing X rays
in CUH from 2015 to 2023') + theme_minimal()
