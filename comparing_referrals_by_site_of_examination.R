# Summary table for group by month

dat5 <- as.data.frame(cleaned_dat2 %>% 
                        group_by(month_year, `Site of XR Examination`) %>% 
                        summarise(not_OAR = sum(not_OAR == 1),
                                  total = n(),
                                  percentage_not_oar = not_OAR / total))


ggplot(dat5, aes(x = as.Date(paste(month_year, "01", sep = "-")), y = percentage_not_oar)) +
  geom_line() + geom_smooth(se = F) + 
  facet_wrap(~ `Site of XR Examination`) +
  labs(title = 'Trend in inappropriate X ray requests under the OAR at CUH 2015-2022, 
by site of X-ray examination',
       x = "Year",
       y = "% of inappropriate referrals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(plotly)

ggplot(dat5, aes(x = as.Date(paste(month_year, "01", sep = "-")), y = percentage_not_oar)) +
  geom_line() + geom_smooth(se = F) + 
  facet_wrap(~ `Site of XR Examination`) +
  labs(title = 'Trend in inappropriate X ray requests under the OAR at CUH 2015-2023, 
by site of X-ray examination',
       x = "Year",
       y = "% of inappropriate referrals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

