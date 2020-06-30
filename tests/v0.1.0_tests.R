US <- ronats_pull()
NC <- ronats_pull(state = "nc")

us_hosp <- ronats_plot(US, hospitalizedCurrently)
us_hosp

nc_hosp <- ronats_plot(NC, hospitalizedCurrently)
nc_hosp
