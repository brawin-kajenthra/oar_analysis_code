# To facilitate analysis across all the data collected, we must bring them all
# into one dataframe. 

# Let's tackle the non-preliminary data and create an initial dataframe. We
# must ensure we remove any duplicate entries. We do this by keeping those
# which have a unique accession number. 

# List all CSV files in the directory
files <- list.files('data/raw_data/', pattern = "\\.csv$", full.names = T)

library(dplyr) # Useful package for data processing

# We use the bind_rows function from dplyr to combine the files. 

combined_data <- bind_rows(lapply(files, read.csv, stringsAsFactors = FALSE))

# Let's format our variables correctly and remove useless columns

colnames(combined_data)

dat <- combined_data %>% 
  mutate_at(c('Procedure', 'fracture.y.n', 'Patient.Class', 
              'Order.Clinician.Type', 'Ottawa.y.fracture.y', 
              'Ottawa.y.fracture.n', 'Ottawa.n.fracture.y',
              'Ottawa.n.fracture.n', 'Exclusion', 
              'OAR.met.in.clinical.indications...y.n'), as.factor) %>% 
  mutate(Exam.Date.Time = lubridate::dmy_hm(Exam.Date.Time)) %>% 
  mutate(month_year = format(Exam.Date.Time, "%Y-%m")) %>% 
  tidyr::drop_na(Ottawa.y.fracture.y) %>% 
  mutate('not_OAR' = case_when(Ottawa.n.fracture.y == 1 ~ 1,
                               Ottawa.n.fracture.n == 1 ~ 1,
                               TRUE ~ 0)) %>% 
  mutate_at('not_OAR', as.factor) %>% 
  filter(Order.Clinician.Type != "") %>% 
  filter(Procedure != "") %>%
  select(-c('IP.Admission.Date.Time', 'IP.Discharge.Date.Time', 
            'Alert.Begin.Instant', 'Alert.Begin.User.Name', 'Alert.Class',
            'Alert.Status', 'Alert.End.Instant', 'Alert.End.User.Name',
            'Alert.Message.Count', 'Alert.Message.Done.Count', 
            'First.Alert.Message.ID', 'First.Alert.Message.Instant',
            'First.Alert.Message.Status', 'First.Read.Acknowledged.Instant',
            'First.Read.Acknowledged.User', 'Checksum','Checksum.2', 'Order.ID')) 

# Convert the 'Date' column to a Date format
dat$Date <- as.Date(paste0(dat$month_year, "-01"), format = "%Y-%m-%d")

# Extract the year from the 'Date' column
dat$Year <- format(dat$Date, "%Y")

summary(dat)

# Remove data from 2022-2024

dat <- dat %>% 
  filter(!Year %in%  c(2022, 2023, 2024))

# Given the lack of standardisation in types of XRs requested, let's tidy it up.

group_procedures <- function(column) {
  column <- gsub('^XR  ankle Left$', 'XR Left Ankle', column)
  column <- gsub('^XR  ankle Right$', 'XR Right Ankle', column)
  column <- gsub('^XR Ankle$', 'XR Ankle Both', column)
  column <- gsub('^XR ankle Both$', 'XR Ankle Both', column)
  column <- gsub('^XR  ankle Both$', 'XR Ankle Both', column)
  column <- gsub('^XR ankle Both $', 'XR Ankle Both', column)
  column <- gsub('^XR Ankle Both $', 'XR Ankle Both', column)
  column <- gsub('^XR Ankle left$', 'XR Left Ankle', column)
  column <- gsub('^XR ankle Left$', 'XR Left Ankle', column)
  column <- gsub('^XR Ankle Left$', 'XR Left Ankle', column)
  column <- gsub('^XR ankle Lt$', 'XR Left Ankle', column)
  column <- gsub('^XR Ankle Lt$', 'XR Left Ankle', column)
  column <- gsub('^XR ankle Right$', 'XR Right Ankle', column)
  column <- gsub('^XR Ankle Right$', 'XR Right Ankle', column)
  column <- gsub('^XR ankle Rt$', 'XR Right Ankle', column)
  column <- gsub('^XR Ankle Rt$', 'XR Right Ankle', column)
  column <- gsub('^XR Foot $', 'XR Foot Both', column)
  column <- gsub('^XR Foot  Both$', 'XR Foot Both', column)
  column <- gsub('^XR Foot  Left$', 'XR Left Foot', column)
  column <- gsub('^XR Foot  Lt$', 'XR Left Foot', column)
  column <- gsub('^XR Food Rt$', 'XR Right Foot', column)
  column <- gsub('^XR Foot`Rt$', 'XR Right Foot', column)
  column <- gsub('^XR Foot$', 'XR Foot Both', column)
  column <- gsub('^XR Foot  Right$', 'XR Right Foot', column)
  column <- gsub('^XR Foot and ankle Both$', 'XR Foot and Ankle Both', column)
  column <- gsub('^XR Foot and ankle Left$', 'XR Left Foot and Ankle', column)
  column <- gsub('^XR Foot and ankle Lt$', 'XR Left Foot and Ankle', column)
  column <- gsub('^XR Foot and Ankle Lt$', 'XR Left Foot and Ankle', column)
  column <- gsub('^XR Foot and ankle Right$', 'XR Right Foot and Ankle', column)
  column <- gsub('^XR Foot and ankle Rt$', 'XR Right Foot and Ankle', column)
  column <- gsub('^XR Foot Ankle Rt$', 'XR Right Foot and Ankle', column)
  column <- gsub('^XR Foot Left$', 'XR Left Foot', column)
  column <- gsub('^XR Foot Left $', 'XR Left Foot', column)
  column <- gsub('^XR Foot Lt$', 'XR Left Foot', column)
  column <- gsub('^XR Foot Right$', 'XR Right Foot', column)
  column <- gsub('^XR Foot rt$', 'XR Right Foot', column)
  column <- gsub('^XR Foot Rt$', 'XR Right Foot', column)
  column <- gsub('^XR Foot Rt $', 'XR Right Foot', column)
  column <- gsub('^XR Foot left $', 'XR Left Foot', column)
  return(column)
}

dat$Procedure <- as.factor(sapply(dat$Procedure, group_procedures))
dat$Procedure <- relevel(dat$Procedure, ref = "XR Right Ankle")
table(dat$Procedure) # To check if the function has worked. 

# Given the number of different requesting clinicians, we have decided to group
# them to facilitate more effective analysis.

group_professions <- function(column) {
  column <- gsub('Advanced Practitioner', 'Advanced Non-Medical Practitioner', column)
  column <- gsub('Emergency Nurse Practitioner', 'Advanced Non-Medical Practitioner', column)
  column <- gsub('Emergency Care Practitioner', 'Advanced Non-Medical Practitioner', column)
  column <- gsub('Specialist Nurse', 'Advanced Non-Medical Practitioner', column)
  column <- gsub('Paramedic', 'Allied Health Professionals', column)
  column <- gsub('Chiropodist/Podiatrist', 'Allied Health Professionals', column)
  column <- gsub('Physiotherapist', 'Allied Health Professionals', column)
  column <- gsub('Consultant', 'Senior Physician', column)
  column <- gsub('Specialist Doctor', 'Senior Physician', column)
  column <- gsub('Specialty Doctor', 'Senior Physician', column)
  column <- gsub('GP Registrar', 'General Practitioner', column)
  column <- gsub('General Medical Practitioner', 'General Practitioner', column)
  column <- gsub('Specialty Trainee Doctor \\(ST3\\+\\)', 'Speciality Registrar', column)
  column <- gsub('ST3\\+ Trust Grade Doctor', 'Speciality Registrar', column)
  column <- gsub('Clinical Fellow', 'Senior House Officer', column)
  column <- gsub('Foundation Trainee Doctor', 'Senior House Officer', column)
  column <- gsub('Foundation Year 2', 'Senior House Officer', column)
  column <- gsub('Hospital Practitioner', 'Senior House Officer', column)
  column <- gsub('Junior Trust Grade Doctor', 'Senior House Officer', column)
  column <- gsub('Locum Doctor', 'Senior House Officer', column)
  column <- gsub('Specialty Trainee Doctor \\(CT1/2\\)', 'Senior House Officer', column)
  column <- gsub('Junior Sister/Charge Nurse', 'ED Nurse', column)
  column <- gsub('Modern Matron', 'ED Nurse', column)
  column <- gsub('Senior Sister/Charge Nurse', 'ED Nurse', column)
  column <- gsub('Staff Nurse', 'ED Nurse', column)
  return(column)
}

dat$`Health Profession Group` <- as.factor(sapply(dat$Order.Clinician.Type, group_professions, USE.NAMES = T))
unique(dat$`Health Profession Group`) # To check if our function has worked
dat$`Health Profession Group` <- relevel(dat$`Health Profession Group`, ref = "Senior Physician")


# Creating age groups for easier comparison for modelling

breaks <- c(0, seq(10, 90, by = 10), Inf)
labels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+")


dat$`Age Group` <- cut(dat$Age.at.Exam, breaks = breaks, labels = labels, right = FALSE)
dat$`Age Group` <- relevel(dat$`Age Group`, ref = '10-19')

# Let's create a dataset which doesn't have the excluded reports

cleaned_dat <- dat %>% 
  filter(Exclusion == 0) %>% 
  filter(Age.at.Exam >= 2) %>% 
  filter(Procedure != 'XR Foot and Ankle Both') %>% 
  distinct(Accession.Number, .keep_all = TRUE)

cleaned_dat$Procedure <- as.factor(sapply(cleaned_dat$Procedure, group_procedures))
cleaned_dat$Procedure <- relevel(cleaned_dat$Procedure, ref = "XR Right Ankle")
table(cleaned_dat$Procedure) # To check if the function has worked.

# Let's subset out the patients who have had both a foot and an ankle X-ray
# We'll create another column where they will be able to look at whether
# the patients in this cohort really needed both foot and ankle scans?

xr_both_patients <- cleaned_dat %>%
  filter(
    (Age.at.Exam == lag(Age.at.Exam, default = NA) | Age.at.Exam == lead(Age.at.Exam, default = NA)) & 
      (Report.Text == lag(Report.Text, default = NA) | Report.Text == lead(Report.Text, default = NA))
  ) %>% 
  mutate(Was.both.ankle.and.foot.scan.needed = NA)

# Now, let's remove the xr_both_patients from the cleaned_dat

cleaned_dat <- cleaned_dat %>%  filter(
  !(Age.at.Exam == lag(Age.at.Exam, default = NA) | Age.at.Exam == lead(Age.at.Exam, default = NA)) |
    !(Report.Text == lag(Report.Text, default = NA) | Report.Text == lead(Report.Text, default = NA))
)

# Now, let's write the xr_both_patients dataset to an Excel file

library(writexl) # Used to write Excel spreadsheets

write_xlsx(xr_both_patients, 'data/xr_both_patients.xlsx')

# Remove unnecessary files and keep those needed for analysis

rm('combined_data', 'dat', 'breaks', 'files', 
   'labels', 'group_procedures', 'group_professions')

# Let's filter out those who have had both foot and ankle, but have slipped through
# the net into the cleaned_dat spreadsheet. 

xr_both_patients_part_two <- cleaned_dat %>% 
  filter(Procedure %in% c('XR Left Foot and Ankle', 'XR Right Foot and Ankle')) %>% 
  mutate(Was.both.ankle.and.foot.scan.needed = NA)

write_xlsx(xr_both_patients_part_two, 'data/xr_both_part_two.xlsx')

# Now, let's filter that out from the cleaned_dat spreadsheet and write this
# to an Excel file

cleaned_dat <- cleaned_dat %>% 
  filter(!(Procedure %in% c('XR Left Foot and Ankle', 'XR Right Foot and Ankle')))

write_xlsx(cleaned_dat, 'data/cleaned_dat.xlsx')
