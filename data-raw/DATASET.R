# compiling air quality monitoring data from several sites in the Canton of Zürich by Ostluft and NABEL monitoring networks


# read datasets ...
# ---
# => read NABEL monitoring airquality data (y1 & h1)
data_monitoring_nabel <- read_local_csv("inst/extdata/nabel_ib_y1.csv")
data_monitoring_nabel_h1 <- lapply(c("inst/extdata/nabel_zue_h1.txt", "inst/extdata/nabel_due_h1.txt"), function(x) read_local_csv(x, delim = "\t"))

# => read Ostluft monitoring airquality data (y1 & h1)
data_monitoring_ostluft <- read_local_csv("inst/extdata/ostluft_airmo_y1.csv", locale = readr::locale(encoding = "UTF-8"), col_names = FALSE)
data_monitoring_ostluft_h1 <- read_local_csv("inst/extdata/ostluft_airmo_h1.csv", locale = readr::locale(encoding = "UTF-8"), col_names = FALSE)

# => read pre-compiled Ostluft y1 monitoring data for nitrogen deposition to sensitive ecosystems into separate dataset
data_monitoring_ndep <- read_local_csv("inst/extdata/ostluft_compiled_ndep_y1.csv", locale = readr::locale(encoding = "UTF-8"))

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
usethis::use_data(data_monitoring_aq)
usethis::use_data(data_monitoring_ndep)


