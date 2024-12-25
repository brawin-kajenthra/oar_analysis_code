# By health profession, but accounting for proportion of scans by 
# health profession

dat3 <- cleaned_dat2 %>% group_by(month_year, `Health Profession Group`) %>% 
  summarise(
    not_OAR = sum(not_OAR == 1),
    total = n(),
    percentage_not_oar = not_OAR / total
  )

ggplot(dat3, aes(x = as.Date(paste(month_year, "01", sep = "-")), y = percentage_not_oar)) +
  geom_line() + geom_smooth(se = F) + 
  facet_wrap(~ `Health Profession Group`) +
  labs(title = 'Trend in inappropriate X ray requests under the OAR at CUH 2015-2022, 
by health professional group',
       x = "Year",
       y = "% of inappropriate referrals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(labels = scales::percent_format())


table(cleaned_dat$not_OAR)
