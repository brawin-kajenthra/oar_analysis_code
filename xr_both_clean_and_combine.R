# Exploring the patients who have had both ankle and foot X-rays

xr_both <- readxl::read_xlsx('data/cleaned_xr_both.xlsx')

colnames(xr_both)

# Convert the 'Date' column to a Date format
xr_both$Date <- as.Date(paste0(xr_both$month_year, "-01"), format = "%Y-%m-%d")  # Adding day to ensure proper Date format

# Extract the year from the 'Date' column
xr_both$Year <- format(xr_both$Date, "%Y")

xr_both2 <- xr_both %>% filter(!Year %in%  c(2022, 2023, 2024))


table(xr_both2$Was.both.ankle.and.foot.scan.needed, useNA = 'ifany')

library(dplyr)

cleaned_xr_both <- xr_both2 %>%
  filter(Exclusion != 0)

xrb_dat1 <- cleaned_xr_both %>% 
  filter(!is.na(Was.both.ankle.and.foot.scan.needed))

table(xrb_dat1$Was.both.ankle.and.foot.scan.needed)

table(cleaned_xr_both$not_OAR)

table(cleaned_xr_both$Procedure, useNA = 'ifany')

writexl::write_xlsx(cleaned_xr_both, 'data/cleaned_xr_both.xlsx')

# Now, let's combine the datasets to create a complete clean dataset

complete_dat <- dplyr::bind_rows(cleaned_dat, cleaned_xr_both)

write.csv(complete_dat, 'data/complete_data_finalised.csv')
write_xlsx(complete_dat, 'data/complete_data_finalised.xlsx')


