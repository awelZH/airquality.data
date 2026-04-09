library(devtools)
library(airquality.methods)
library(rOstluft)
library(zoo)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

devtools::load_all(path = "R")



# fundamentals for general air quality analysis
# compiling air quality monitoring data from several sites in Eastern Switzerland by Ostluft and NABEL monitoring networks

# read datasets ...
# ---
# => read NABEL monitoring airquality data (y1 & h1)
data_monitoring_nabel_y1 <- airquality.methods::read_local_csv("inst/extdata/nabel_ib_y1.csv")
data_monitoring_nabel_d1 <- lapply(c("inst/extdata/nabel_zue_d1.csv", "inst/extdata/nabel_due_d1.csv"), airquality.methods::read_local_csv)
data_monitoring_nabel_h1 <- lapply(c("inst/extdata/nabel_zue_h1.txt", "inst/extdata/nabel_due_h1.txt"), function(x) airquality.methods::read_local_csv(x, delim = "\t"))

# => read Ostluft monitoring airquality data (y1 & h1, d1)
data_monitoring_ostluft_y1 <- airquality.methods::read_local_csv("inst/extdata/ostluft_airmo_y1.csv", locale = readr::locale(encoding = "UTF-8"), col_names = FALSE)
data_monitoring_ostluft_d1 <- airquality.methods::read_local_csv("inst/extdata/ostluft_airmo_d1.csv", locale = readr::locale(encoding = "UTF-8"), col_names = FALSE)
data_monitoring_ostluft_h1 <- airquality.methods::read_local_csv("inst/extdata/ostluft_airmo_h1.csv", locale = readr::locale(encoding = "UTF-8"), col_names = FALSE)

# => read pre-compiled Ostluft y1 monitoring data for nitrogen deposition to sensitive ecosystems into separate dataset
# data_monitoring_ndep <- airquality.methods::read_local_csv("inst/extdata/ostluft_compiled_ndep_y1.csv", locale = readr::locale(encoding = "UTF-8"))
# TODO: replace when respective analysis is online or remove and hyperlink separately

# => read NABEL & Ostluft monitoring site metadata
site_meta_nabel <- airquality.methods::read_local_csv("inst/extdata/nabel_ib_y1.csv", col_select = c("Station", "Ost Y", "Nord X", "Höhe", "Zonentyp", "Stationstyp"))
site_meta_ostluft <- airquality.methods::read_local_csv("inst/extdata/ostluft_site_metadata.csv", delim = ",", locale = readr::locale(encoding = "UTF-8"))


# prepare datasets ...
# ---
# => merge, simplify & finalise site metadata
site_meta <- prepare_monitoring_meta(site_meta_ostluft, site_meta_nabel)

# => restructure NABEL & calculate O3 peak season from h1 data
data_monitoring_nabel_y1 <-
  data_monitoring_nabel_y1 |>
  prepare_monitoring_nabel_y1()

data_monitoring_nabel_y1 <-
  data_monitoring_nabel_h1 |>
  prepare_monitoring_nabel_h1() |>
  dplyr::bind_rows(data_monitoring_nabel_y1)

data_monitoring_nabel_d1 <-
  purrr::map(data_monitoring_nabel_d1, restructure_monitoring_nabel_d1) |>
  dplyr::bind_rows() |>
  dplyr::mutate(source = "NABEL (BAFU & Empa)") |>
  dplyr::filter(!(parameter %in% c("RainSum", "StrGlo", "T"))) # only air pollutants

# => restructure Ostluft & calculate O3 peak season from h1 data
data_monitoring_ostluft_y1 <- prepare_monitoring_ostluft_y1(data_monitoring_ostluft_y1)
data_monitoring_ostluft_y1 <-
  data_monitoring_ostluft_h1 |>
  prepare_monitoring_ostluft_h1() |>
  dplyr::bind_rows(data_monitoring_ostluft_y1)

data_monitoring_ostluft_d1 <-
  data_monitoring_ostluft_d1 |>
  restructure_monitoring_ostluft(na.rm = FALSE) |>
  remove_duplicate_d1() |>
  dplyr::mutate(source = "Ostluft")

# => merge & finalise datasets
data_monitoring_aq_y1 <-
  data_monitoring_nabel_y1 |>
  dplyr::bind_rows(data_monitoring_ostluft_y1) |>
  prepare_monitoring_aq(site_meta)

data_monitoring_aq_d1 <-
  data_monitoring_ostluft_d1 |>
  dplyr::bind_rows(data_monitoring_nabel_d1) |>
  prepare_monitoring_aq(site_meta, interval = "d1")

# data_monitoring_ndep <-
#   data_monitoring_ndep |>
#   dplyr::mutate(source = "Ostluft")


# save datasets
# ---
usethis::use_data(data_monitoring_aq_y1, overwrite = TRUE)
usethis::use_data(data_monitoring_aq_d1, overwrite = TRUE)








# MeteoSchweiz meteo monitoring data:
# as input for air quality trend analysis

# read datasets ...
# ---
# => verfügbare MeteoSchweiz Bodenmessstationen im Kanton Zürich
stations <- airquality.methods::read_local_csv("https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_stations.csv")
# dplyr::filter(stations, station_canton == "ZH" & station_type_de == "Automatische Wetterstationen")

# => ausgewählte Staionen im Kanton Zürich
sites_meteo <- c("KLO", "REH", "SMA", "HOE")
stations <-
  stations |>
  dplyr::filter(station_abbr %in% !!sites_meteo) |>
  dplyr::select(station_abbr, station_name)


# => d1 Messdaten von sites_meteo
urls <- airquality.methods:::get_geo_admin_metadata("ch.meteoschweiz.ogd-smn", filter = "_d_historical.csv")
urls <- as.character(sapply(tolower(sites_meteo), function(x) urls[which(stringr::str_detect(urls, x))]))
urls <- urls[urls != "character(0)"]
data_monitoring_met_d1 <- airquality.methods::read_local_csv(urls)


# # => y1 Messdaten von sites_meteo
# urls <- airquality.methods:::get_geo_admin_metadata("ch.meteoschweiz.ogd-smn", filter = "_y.csv")
# urls <- as.character(sapply(tolower(sites_meteo), function(x) urls[which(stringr::str_detect(urls, x))]))
# urls <- urls[urls != "character(0)"]
# data_monitoring_met_y1 <- airquality.methods::read_local_csv(urls)


# prepare datasets ...
# ---
# => Messparameter
parameters <- airquality.methods::read_local_csv("https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_parameters.csv")
parameters <- dplyr::select(parameters, parameter_shortname, parameter_unit, parameter_description_de)
# View(parameters)
selected_parameters <- c("WD" = "Windrichtung; Tagesmittel", "WVs" = "Windgeschwindigkeit skalar; Tagesmittel in m/s", "Föhnindex" = "Föhnindex, Tageswert",
                         "StrGlo" = "Globalstrahlung; Tagesmittel", "Lrad" = "Langwellige Ausstrahlung; Tagesmittel", "p" = "Luftdruck auf Barometerhöhe (QFE); Tagesmittel",
                         "T" = "Lufttemperatur 2 m über Boden; Tagesmittel", "T_max_min10" = "Lufttemperatur 2 m über Boden; Tagesmaximum",
                         "RainSum" = "Niederschlag; Tagessumme 0 UTC - 0 UTC", "Hr" = "Relative Luftfeuchtigkeit 2 m über Boden; Tagesmittel")
parameters <-
  parameters |>
  dplyr::filter(parameter_description_de %in% !!selected_parameters) |>
  dplyr::mutate(parameter = dplyr::recode(parameter_description_de, !!!setNames(names(selected_parameters), selected_parameters))) |>
  dplyr::rename(unit = parameter_unit) |>
  dplyr::select(-parameter_description_de)

data_monitoring_met_d1 <-
  data_monitoring_met_d1 |>
  dplyr::left_join(stations, by = "station_abbr") |>
  dplyr::rename(
    site = station_name,
    starttime = reference_timestamp
  ) |>
  dplyr::select(-station_abbr) |>
  tidyr::gather(parameter_shortname, value, -starttime, -site) |>
  dplyr::left_join(parameters, by = "parameter_shortname") |>
  dplyr::filter(parameter_shortname %in% !!parameters$parameter_shortname) |>
  dplyr::select(-parameter_shortname) |>
  dplyr::mutate(
    starttime = lubridate::fast_strptime(starttime, format = "%d.%m.%Y %H:%S", tz = "Etc/GMT-1", lt = FALSE), # oder in UTC? Lässt sich in dem Fall jedenfalls nicht einfach auf UTC+1 beziehen..
    interval = "d1",
    source = "MeteoSchweiz"
  ) |>
  dplyr::mutate_if(is.character, factor) |>
  dplyr::select(starttime, interval, site, parameter, unit, value, source)


# save datasets
# ---
usethis::use_data(data_monitoring_met_d1, overwrite = TRUE)





# # Ostluft meteo monitoring data:
# # as input for air quality trend analysis
#
# # read datasets ...
# # ---
# data_monitoring_ostluft_met_d1 <- airquality.methods::read_local_csv("inst/extdata/ostluft_airmo_meteo_d1.csv", locale = readr::locale(encoding = "UTF-8"), col_names = FALSE)
#
#
# # prepare datasets ...
# # ---
# data_monitoring_ostluft_met_d1 <-
#   data_monitoring_ostluft_met_d1 |>
#   restructure_monitoring_ostluft(na.rm = TRUE) |>
#   dplyr::mutate(source = "Ostluft")
#
# # => merge & finalise datasets
# data_monitoring_met_d1 <-
#   data_monitoring_ostluft_met_d1 |>
#   prepare_monitoring_aq(site_meta, interval = "d1") |>
#   dplyr::select(-pollutant, -metric)
#
#
# # save datasets
# # ---
# usethis::use_data(data_monitoring_met_d1, overwrite = TRUE)









# fundamentals for nitrogen deposition analysis:
# compiling monitoring period data and site metadata in Ostluft by Ostluft and NABEL monitoring networks

# read datasets ...
# ---
# => read ndep Ostluft & NABEL monitoring data based on sample periods (about one month)
data_monitoring_ndep <- airquality.methods::read_local_csv("inst/extdata/ostluft_ndep_periods.csv", locale = readr::locale(encoding = "UTF-8"))

# => read ndep monitoring site metadata
site_meta_ndep <- airquality.methods::read_local_csv("inst/extdata/ostluft_site_ndep_metadata.csv", locale = readr::locale(encoding = "UTF-8"))


# prepare datasets ...
# ---
# => restructure site metadata
site_meta_ndep <-
  site_meta_ndep |>
  dplyr::rename(site = msNameAirMo) |>
  dplyr::mutate(siteclass_nh3 = siteclass_nh3(gve_5km, n_austrag_5km)) |>
  dplyr::left_join(dplyr::select(site_meta, site, site_long, canton, x, y, masl, siteclass), by = "site") |>
  dplyr::select(site, site_long, fubcode, canton, x, y, masl, siteclass, siteclass_nh3, gve_5km, n_austrag_5km,
                oekosystem1, oekosystem2, oekosystem_bafu, oekosystem_detail, cln_fun_oekosystem1, cln_fun_oekosystem2)

# TODO: data_monitoring_ndep straight away with msNameAirMo...
# => msNameAirMo as primary key
data_monitoring_ndep <-
  site_meta_ndep |>
  dplyr::select(site, site_long, fubcode) |>
  dplyr::right_join(data_monitoring_ndep, by = "fubcode") |>
  dplyr::mutate(
    starttime = lubridate::fast_strptime(starttime, format = "%d.%m.%Y %H:%M", tz = "Etc/GMT+1", lt = FALSE),
    endtime = lubridate::fast_strptime(endtime, format = "%d.%m.%Y %H:%M", tz = "Etc/GMT+1", lt = FALSE),
    interval = "period"
  ) |>
  dplyr::mutate_if(is.character, factor) |>
  dplyr::select(site, site_long, starttime, endtime, interval, parameter, value, unit, method, source)


# save datasets
# ---
usethis::use_data(site_meta_ndep, overwrite = TRUE)
usethis::use_data(data_monitoring_ndep, overwrite = TRUE)



