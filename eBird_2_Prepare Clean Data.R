##########################
##  Prepare Clean Data  ##
##########################

## Startup 
library(tidyverse)


## Load data
raw <- read_delim("C:\\Users\\isaac\\Desktop\\eBird\\Data\\Long Island Jan 2010 - Aug 2021.txt",
                  delim = "\t")
lifeList <- read_csv("C:\\Users\\isaac\\Desktop\\eBird\\Data\\211013 Life List.csv")


#WHERE ARE PIGEONS - Columba livia
findPigeons <- function(x) {
  x <- unique(x)
  i <- grepl("pigeon", x)
  return(x[i])
}

raw %>%
  filter(CATEGORY != "species") %>%
  count(CATEGORY, `COMMON NAME`) %>%
  arrange(-n)


## Clean data
#Remove non-LI birds from life list
lifeList <- lifeList %>%
  filter(!Species %in% c("Caspian Tern", 
                         "Sandhill Crane",
                         "Summer Tanager", 
                         "Broad-winged Hawk", 
                         "Pileated Woodpecker", 
                         "White-throated Magpie-Jay",
                         "Keel-billed Toucan",
                         "Great Curassow"))

#Clean df
df <- raw %>%
  filter(CATEGORY %in% c("species", "issf", "domestic")) %>%
  select(checklistID = `SAMPLING EVENT IDENTIFIER`,
         observerID = `OBSERVER ID`,
         date = `OBSERVATION DATE`,
         species = `COMMON NAME`,
         count = `OBSERVATION COUNT`,
         county = `COUNTY`,
         lat = `LATITUDE`,
         lon = `LONGITUDE`,) %>%
  mutate(lifer = species %in% lifeList$Species,
         count = count %>%
           na_if("X") %>%
           as.numeric())


## Save data
saveRDS(df,
        "C:\\Users\\isaac\\Desktop\\eBird\\Data\\Long Island Jan 2010 - Aug 2021.rds")
