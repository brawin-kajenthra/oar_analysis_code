# Create year group datasets 
split_data <- split(cleaned_dat, cleaned_dat$Year)


write.csv(split_data[["2015"]], 'data/by_year/dat_2015.csv')
write.csv(split_data[["2016"]], 'data/by_year/dat_2016.csv')
write.csv(split_data[["2017"]], 'data/by_year/dat_2017.csv')
write.csv(split_data[["2018"]], 'data/by_year/dat_2018.csv')
write.csv(split_data[["2019"]], 'data/by_year/dat_2019.csv')
write.csv(split_data[["2020"]], 'data/by_year/dat_2020.csv')
write.csv(split_data[["2021"]], 'data/by_year/dat_2021.csv')
write.csv(split_data[["2022"]], 'data/by_year/dat_2022.csv')
write.csv(split_data[["2023"]], 'data/by_year/dat_2023.csv')


