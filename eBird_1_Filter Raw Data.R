#######################
##  Filter Raw Data  ##
#######################

## We already have raw .txt data from eBird for all observations in NY history
## auk lets us filter these data into a smaller .txt file that we can work with

library(auk)

input_file <- "C:\\Users\\isaac\\Desktop\\eBird\\Data\\ebd_US-NY_relAug-2021.txt"
output_file <- "C:\\Users\\isaac\\Desktop\\eBird\\Data\\Long Island Jan 2010 - Aug 2021.txt"

input_file %>% 
  # 1. reference file
  auk_ebd() %>% 
  # 2. define filters
  auk_county(county = c("US-NY-103", "US-NY-059")) %>%
  auk_date(date = c("2010-01-01", "2021-08-31")) %>%
  # 3. run filtering and save
  auk_filter(file = output_file,
             overwrite = T)
