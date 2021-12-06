library(tidyverse)

#source data: hadza-data.raw.xlsx, sheet=hadza-data-raw
#these data are not shared because they contain personal identifying information
#these data are stripped from datasets from three field seasons and are not truly 'raw'
#they are 'raw' for this project because they are constitute the full set of individuals with dental data
#this script is shared for transparency in the manipulation taking place prior to data sharing
hadza.source <- read.csv("1b-HadzaLEH_recode-ORIG.csv", sep=",",
                         strip.white = TRUE, check.names = TRUE, stringsAsFactors = FALSE)

#standardize column names
hadza.source  <- rename_with(hadza.source, ~ tolower(gsub("..", ".", .x, fixed = TRUE)))

#fix data entry problem
#age was classed character b/c 15.5 was entered as 15 5
#reclass to numeric
hadza.source$age <- as.numeric(hadza.source$age)
#replace NA for individual 424 with 15.5
hadza.source[hadza.source$id==424, "age"] <- 15.5

#remove 3 individuals with too much wear or damaged teeth
#4	teeth in bad shape
#316 teeth very worn, but not much enamel left
#267 uppers too worn to see any LEH
hadza.wear <- hadza.source %>% filter(id != "4")
hadza.wear <- hadza.wear %>% filter(id != "316")
hadza.wear <- hadza.wear %>% filter(id != "267")

#move 8 individuals under the age of 18
hadza.adult <- filter(hadza.wear, age > 17)

#strip personal info and keep only columns to be used for analysis
hadza.stripped <- dplyr::select(hadza.adult, year, yob, sex, age,
        h_rxi1, h_rxi2, h_rxc, h_lxi1, h_lxi2, h_lxc,
        h_rni1, h_lni1, h_rni2, h_lni2, h_rnc, h_lnc)

#save data and tidy
write.csv(hadza.stripped, "1-data-hadza-stripped.csv", quote=FALSE, row.names = FALSE)
rm(hadza.source, hadza.wear, hadza.adult, hadza.stripped)
