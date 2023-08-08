Subdirectory:
  C:/Users/Cheryl/OneDrive/Desktop/DSRP-2023-Greenleaf

library(readr)
chs2020_codebook <- read_csv("chs2020-codebook.csv")
View(chs2020_codebook)



install.packages("sas7bdat")
install.packages("haven")
install.packages("curl")

library(haven)
library(curl)

#google link of the dataset (sas)
nyc2019 <- read_sas(curl("https://www.nyc.gov/assets/doh/downloads/sas/episrv/chs2019_public.sas7bdat"))

View(nyc2019)






