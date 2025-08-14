devtools::load_all()


# fundamentals for general air quality analysis
# compiling air quality monitoring data from several sites in the Canton of Zürich by Ostluft and NABEL monitoring networks

# read datasets ...
# ---
# => read NABEL monitoring airquality data (y1 & h1)
data_monitoring_nabel <- read_local_csv("inst/extdata/nabel_ib_y1.csv")
data_monitoring_nabel_h1 <- lapply(c("inst/extdata/nabel_zue_h1.txt", "inst/extdata/nabel_due_h1.txt"), function(x) read_local_csv(x, delim = "\t"))

# => read Ostluft monitoring airquality data (y1 & h1)
data_monitoring_ostluft <- read_local_csv("inst/extdata/ostluft_ktzh_y1.csv", locale = readr::locale(encoding = "UTF-8"), col_names = FALSE)
data_monitoring_ostluft_h1 <- read_local_csv("inst/extdata/ostluft_ktzh_h1.csv", locale = readr::locale(encoding = "UTF-8"), col_names = FALSE)

# => read pre-compiled Ostluft y1 monitoring data for nitrogen deposition to sensitive ecosystems into separate dataset
data_monitoring_ndep <- read_local_csv("inst/extdata/ostluft_compiled_ndep_y1.csv", locale = readr::locale(encoding = "UTF-8"))
# TODO: replace when respective analysis is online

# => read NABEL & Ostluft monitoring site metadata
site_meta_nabel <- read_local_csv("inst/extdata/nabel_ib_y1.csv", col_select = c("Station", "Ost Y", "Nord X", "Höhe", "Zonentyp", "Stationstyp"))
site_meta_ostluft <- read_local_csv("inst/extdata/ostluft_site_metadata.csv", delim = ",", locale = readr::locale(encoding = "UTF-8"))


# prepare datasets ...
# ---
# => merge, simplify & finalise site metadata
site_meta <- prepare_monitoring_meta(site_meta_ostluft, site_meta_nabel)

# => restructure NABEL & calculate O3 peak season from h1 data
data_monitoring_nabel <-
  data_monitoring_nabel |>
  prepare_monitoring_nabel_y1()

data_monitoring_nabel <-
  data_monitoring_nabel_h1 |>
  prepare_monitoring_nabel_h1() |>
  dplyr::bind_rows(data_monitoring_nabel) |>
  dplyr::filter(site %in% c("Zürich-Kaserne", "Dübendorf-EMPA")) # the only NABEL-sites in Canton Zürich

# => restructure Ostluft & calculate O3 peak season from h1 data
data_monitoring_ostluft <- prepare_monitoring_ostluft_y1(data_monitoring_ostluft)
data_monitoring_ostluft <-
  data_monitoring_ostluft_h1 |>
  prepare_monitoring_ostluft_h1() |>
  dplyr::bind_rows(data_monitoring_ostluft)

# => merge & finalise datasets
data_monitoring_aq <-
  data_monitoring_nabel |>
  dplyr::bind_rows(data_monitoring_ostluft) |>
  prepare_monitoring_aq(site_meta)

data_monitoring_ndep <-
  data_monitoring_ndep |>
  dplyr::mutate(source = "Ostluft")


# save datasets
# ---
usethis::use_data(data_monitoring_aq, overwrite = TRUE)






# fundamentals for nitrogen deposition analysis:
# compiling monitoring period data and site metadata in Ostluft by Ostluft and NABEL monitoring networks

# read datasets ...
# ---
# => read ndep Ostluft & NABEL monitoring data based on sample periods (about one month)
data_monitoring_ndep <- read_local_csv("inst/extdata/ostluft_ndep_periods.csv", locale = readr::locale(encoding = "UTF-8"))

# => read ndep monitoring site metadata
site_meta_ndep <- read_local_csv("inst/extdata/ostluft_site_ndep_metadata.csv", locale = readr::locale(encoding = "UTF-8"))


# prepare datasets ...
# ---
# => restructure site metadata
site_meta_ndep <-
  site_meta_ndep |>
  dplyr::rename(site = msNameAirMo) |>
  dplyr::mutate(siteclass = siteclass_nh3(gve_5km, n_austrag_5km))

# => msNameAirMo as primary key
data_monitoring_ndep <-
  site_meta_ndep |>
  dplyr::select(site, fubcode) |>
  dplyr::right_join(data_monitoring_ndep, by = "fubcode") |>
  dplyr::mutate(
    starttime = lubridate::fast_strptime(starttime, format = "%d.%m.%Y %H:%M", tz = "Etc/GMT+1", lt = FALSE),
    endtime = lubridate::fast_strptime(endtime, format = "%d.%m.%Y %H:%M", tz = "Etc/GMT+1", lt = FALSE),
    interval = "period"
  ) |>
  dplyr::mutate_if(is.character, factor) |>
  dplyr::select(site, starttime, endtime, interval, parameter, value, unit, method, source)


# save datasets
# ---
usethis::use_data(site_meta_ndep, overwrite = TRUE)
usethis::use_data(data_monitoring_ndep, overwrite = TRUE)



