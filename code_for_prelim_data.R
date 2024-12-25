# After tidying the dataset, we have found 46467 reports. Now let's add the
# preliminary datasets to complete the combined dataframe. 
# We separated them as the preliminary datasets did not have an accession 
# number. 

prelim_files <- list.files('data/prelim_data/', pattern = "\\.csv$", 
                           full.names = T)

prelim_df <- lapply(prelim_files, read.csv, stringsAsFactors = FALSE)

# Let's create our final combined dataset. It contains 48035 X ray reports. 
combined_data <- bind_rows(combined_data, prelim_df)