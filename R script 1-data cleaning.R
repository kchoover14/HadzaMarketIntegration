library(tidyverse)

#these data are de-identified and shared in the github repository
hadza <- read.csv("1-data-hadza-stripped.csv", stringsAsFactors = TRUE)

#recode sex as character male/female
hadza <- rename(hadza, sexn=sex)
hadza$Sex <- with(hadza, ifelse(sexn > 1, 'Woman', 'Man'))

#get year of birth using age when data were collected (year)
hadza$yob2 <- hadza$year - hadza$age
#if year of birth is NA, replace value with yob2
hadza <- hadza %>% mutate(yob = coalesce(yob,yob2))

#create new variable for yob before tourism/after tourism, 1975
hadza$BirthCohort <- with(hadza, ifelse(yob < 1974, '1974-', '1975+'))

#create new variables for hypoplasia frequency based on total LEH/total teeth
hadza$total.hypoplasia <-  rowSums(hadza[ , 5:16], na.rm=TRUE)
hadza$total.teeth <- rowSums(!is.na(hadza[5:16]))

#create new variable with average hypoplasia frequency across observed teeth
hadza$hypoplasia.frequency <- with(hadza, (total.hypoplasia/total.teeth))

#clean dataset
write.csv(hadza, "2-data-hadza-clean.csv", row.names = FALSE, quote=FALSE)

#tidy
rm(hadza)

#trim dataset to variables needed only for analysis
hadza.clean <- read.csv("2-data-hadza-clean.csv", stringsAsFactors = TRUE)
hadza.final <- dplyr::select(hadza.clean, age, yob, Sex, BirthCohort, total.hypoplasia, total.teeth, hypoplasia.frequency)

#final dataset (include only variables used for analysis)
write.csv(hadza.final, "3-data-hadza-final.csv", row.names = FALSE, quote=FALSE)

#tidy
rm(hadza.clean, hadza.final)

