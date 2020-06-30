library(ggplot2)
library(dplyr)
library(readr)
library(zoo)
library(ronats)


# us <- ronats_pull()
# nc <- ronats_pull("nc")
# ca <- ronats_pull("ca")
us <- read_csv("tests/testdata/us_2020-06-30.csv")
nc <- read_csv("tests/testdata/nc_2020-06-30.csv")
ca <- read_csv("tests/testdata/ca_2020-06-30.csv")


us_hosp <- ronats_plot(us, hospitalizedCurrently)
us_hosp

nc_hosp <- ronats_plot(nc, hospitalizedCurrently)
nc_hosp

ca_hosp <- ronats_plot(ca, hospitalizedCurrently)
ca_hosp
