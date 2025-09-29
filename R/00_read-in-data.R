# this script reads in the data file and converts it and saves it as an R dataframe

library(tidyverse)
library(haven)

data <- read_sas("//cdc/project/CCHIS_NCHS_OAEHP/Linkage Staff Only/_staff_/CZhang/SESUG_2024/temp_puf_1218.sas7bdat")

save(data, file = "data/df.Rda")
