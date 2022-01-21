# Load the tidyverse
setwd('/Users/fm/Public/HDS/Data Challenge/local data')
library(tidyverse)

# Load in Mar 2019 Data and select relevant columns only
mar2019 <- read_csv('epd_201903.csv') 
df <- mar2019 %>% 
  select(YEAR_MONTH, REGIONAL_OFFICE_NAME, BNF_CHEMICAL_SUBSTANCE, 
         BNF_CHAPTER_PLUS_CODE, QUANTITY, ITEMS, TOTAL_QUANTITY, ACTUAL_COST)
glimpse(mar2019)
glimpse(df)

# Filter related sections
filter1 <- filter(df, grepl("^0603", BNF_CHEMICAL_SUBSTANCE)) #228054 obs
filter2 <- filter(df, grepl("^010502", BNF_CHEMICAL_SUBSTANCE)) #7622 obs
filter3 <- filter(df, grepl("^100102", BNF_CHEMICAL_SUBSTANCE)) #14748 obs
combined <- rbind(filter1, filter2, filter3) 

# Calculate total quantity prescribed
# sum(combined$QUANTITY) #14398036 
sum(combined$ITEMS) #777337
# Excel 772843
# R combined data 747592

# ------------------------------------------------------------------------------

# Load in Mar 2020 Data and select relevant columns only
mar2020 <- read_csv('epd_202003.csv')
df2 <- mar2020 %>% 
  select(YEAR_MONTH, REGIONAL_OFFICE_NAME, BNF_CHEMICAL_SUBSTANCE, 
         BNF_CHAPTER_PLUS_CODE, QUANTITY, ITEMS, TOTAL_QUANTITY, ACTUAL_COST)
glimpse(df2)

# Filter related sections
filtera<- filter(df2, grepl("^0603", BNF_CHEMICAL_SUBSTANCE)) #239131 obs
filterb <- filter(df2, grepl("^010502", BNF_CHEMICAL_SUBSTANCE)) #7713 obs
filterc <- filter(df2, grepl("^100102", BNF_CHEMICAL_SUBSTANCE)) #10891 obs
combined2 <- rbind(filtera, filterb, filterc)

# Calculate total quantity prescribed
# sum(combined2$QUANTITY) #15668073 
sum(combined2$ITEMS) #908439
# Excel 904090
# R combined data 877945

# Save
save(combined, file="201903.RData")
save(combined2, file="202003.RData")
