#' Merge RSD data with corresponding metadata
#'
#' @param data
#' @param meta
#'
#' @keywords internal
merge_restructure_rsd <- function(data, meta) {

  meta <-
    meta |>
    dplyr::filter(is.na(as.numeric(vehicle_euronorm))) |>
    dplyr::select(-source, -remark) |>
    tidyr::spread(parameter, value)

  data <-
    data |>
    dplyr::mutate(vehicle_euronorm = dplyr::recode(vehicle_euronorm, !!!c("Euro5a" = "Euro5", "Euro5b" = "Euro5"))) |> # merge both sub-Euro5 norms since they are quite similar
    dplyr::left_join(meta, by = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm")) |>
    dplyr::mutate(
      vehicle_type = factor(vehicle_type, levels = c("passenger car", "light duty vehicle")),
      vehicle_fuel_type = factor(vehicle_fuel_type, levels = c("gasoline", "diesel"))
    )

  return(data)
}


#' Filter RSD dataset using filter criteria
#'
#' @param data
#' @param filters
#'
#' @keywords internal
filter_rsd <- function(data, filters) {

  data <-
    data |>
    dplyr::filter(
      vehicle_model_year %in% filters$min[filters$parameter == "vehicleyears"]:filters$max[filters$parameter == "vehicleyears"] &
        (acceleration >= filters$min[filters$parameter == "accelerationrange"] & acceleration <= filters$max[filters$parameter == "accelerationrange"]) &
        (velocity >= filters$min[filters$parameter == "velocityrange"] & velocity <= filters$max[filters$parameter == "velocityrange"]) &
        (vehicle_specific_power >= filters$min[filters$parameter == "vsprange"] & vehicle_specific_power <= filters$max[filters$parameter == "vsprange"]) &
        vehicle_unloaded_weight <= filters$max[filters$parameter == "weightmax"] &
        !is.na(value)
    ) |>
    tidyr::spread(parameter, value) |>
    dplyr::filter(!is.na(NO + CO2 + CO + HC)) # all concentrations are nessecary for NOx emission calculation

  return(data)
}


#' Add calculated vehicle specific power to RSD dataset
#'
#' @param data
#'
#' @keywords internal
prep_vehicle_specific_power <- function(data){

  data_vsp <-
    data |>
    dplyr::filter(parameter %in% c("acceleration", "velocity") & !is.na(value)) |>
    dplyr::select(id, site_roadgrade, parameter, value) |>
    tidyr::spread(parameter, value) |>
    dplyr::mutate(vehicle_specific_power = calc_vsp(velocity * 1000 / 60^2, acceleration * 1000 / 60^2, site_roadgrade)) |>  # also convert velocity from km/h into m/s and acceleration from km/h/s into m/s2
    dplyr::select(id, acceleration, velocity, vehicle_specific_power) # vehicle_specific_power in kW/t

  return(data_vsp)
}


#' Calculate vehicle specific power following Jiménez
#'
#' @param speed
#' @param accel
#' @param slope
#' @param vsp.a
#' @param vsp.b
#' @param vsp.c
#' @param vsp.g
#'
#' @keywords internal
calc_vsp <- function(speed, accel, slope, # speed in m/s, accel in m/s/s, slope as ratio, mass = 3.5 in t
                     vsp.a = 1.1, vsp.b = 0.132, vsp.c = 0.000302, vsp.g = 9.81) {

  vsp <- speed * (vsp.a * accel + (vsp.g * slope) + vsp.b) + (vsp.c * speed^3)

  return(vsp)
}


#' Calculate RSD NOx emissions in g/kg fuel
#'
#' @param NO
#' @param p
#' @param CO2
#' @param CO
#' @param HC
#'
#' @keywords internal
calc_rsd_nox_emission <- function(NO, p, CO2, CO, HC) { # all concentrations in mixing ratios as percent

  Q <- CO / CO2
  Q1 <- HC / CO2
  Q2 <- NO / CO2
  NO_emission <- 30 * Q2 * 860 / ((1 + Q + 6 * Q1) * 12)
  NOx_emission <- NO_emission * 46 / (30 * (1 - p))

  return(NOx_emission)
}


#' Recode Ostluft air quality monitoring urban site classification
#'
#' @description
#' ... to be roughly in line with https://www.bafu.admin.ch/bafu/de/home/themen/luft/publikationen-studien/publikationen/immissionsmessung-von-luftfremdstoffen.html
#' however, the Ostluft site classes are - as categories - not entirely consistent with the current Immissionsmessempfehlung. We will need to put future effort in a reclassification.
#'
#' @param zone
#'
#' @keywords internal
recode_ostluft_meta_zone <- function(zone) {

  zone <-
    dplyr::case_when(
      as.numeric(stringr::str_remove(zone, "H")) %in% c(21:23, 31:33) ~ "städtisch", # OSTLUFT: > 20'000 Gesamteinwohner; BAFU: > 1500 Einwohner/km2 und Gesamteinwohnerzahl > 50 000
      as.numeric(stringr::str_remove(zone, "H")) %in% 11:13 ~ "klein-/vorstädtisch", # OSTLUFT: > 1'000 Gesamteinwohner; BAFU: > 300 Einwohner/km2 im \u00fcberbauten Gebiet und Gesamteinwohnerzahl > 5000
      as.numeric(stringr::str_remove(zone, "H")) == 0 ~ "ländlich", # OSTLUFT: < 1'000 Gesamteinwohner; BAFU: Gebiete mit geringer Siedlungsdichte (< 300 Einwohner/km2) oder kleinere Ortschaften (< 5000 Einwohner)
      TRUE ~ zone
    )

  return(zone)
}


#' Recode Ostluft air quality monitoring traffic site classification
#'
#' @description
#' ... to be roughly in line with https://www.bafu.admin.ch/bafu/de/home/themen/luft/publikationen-studien/publikationen/immissionsmessung-von-luftfremdstoffen.html
#' however, the Ostluft site classes are - as categories - not entirely consistent with the current Immissionsmessempfehlung. We will need to put future effort in a reclassification.
#'
#' @param type
#'
#' @keywords internal
recode_ostluft_meta_type <- function(type) {

  type <-
    dplyr::case_when(
      as.numeric(stringr::str_remove(type, "S")) %in% c(10:13, 20:23, 30:33) ~ "verkehrsbelastet", # OSTLUFT: DTV_S > 10'000; BAFU: has a finer scale that begins at DTV > 3'000 and cerctain max distance to street
      as.numeric(stringr::str_remove(type, "S")) == 0 ~ "Hintergrund", # OSTLUFT: DTV_S < 10'000 & street more than 50m (in cities) or 300m (outside of cities) away; BAFU: see above
      TRUE ~ type
    )

  return(type)
}


#' Convert NABEL y1 dataset into standard long format
#'
#' @param data
#' @param keep_incomplete
#'
#' @keywords internal
restructure_monitoring_nabel_y1 <- function(data, keep_incomplete = FALSE) {

  col_names <- range(as.numeric(names(data)), na.rm = TRUE)
  data <- dplyr::mutate_if(data, is.numeric, as.character)
  data_long <- tidyr::pivot_longer(
    data,
    cols = as.character(min(col_names):max(col_names)),
    names_to = "starttime"
  )
  data_long_clean <-
    data_long |>
    dplyr::mutate(
      value = dplyr::case_when(
        keep_incomplete ~ as.numeric(gsub("\\*|\\;", "", value)),
        TRUE ~ as.numeric(value)
      )
    )

  return(data_long_clean)
}


#' Convert NABEL h1 dataset into standard format
#'
#' @param data
#' @param tz
#'
#' @keywords internal
restructure_monitoring_nabel_h1 <- function(data, tz = "Etc/GMT-1") {

  header <- dplyr::slice(data, 1:which(dplyr::pull(data, 1) == "Einheit"))
  data <- dplyr::slice(data, (which(dplyr::pull(data, 1) == "Einheit") + 1):nrow(data))
  colnames(data)[1] <- "endtime"
  data <- dplyr::mutate(data, endtime = lubridate::parse_date_time(.data$endtime, c("dmyHMS", "dmyHM", "dmy"), tz = tz))
  # FIXME! generalise for more complicated files
  data <-
    data |>
    tidyr::gather(site, value, -endtime) |>
    dplyr::mutate(
      starttime = endtime - lubridate::hours(1),
      parameter = dplyr::pull(header,2)[dplyr::pull(header,1) == "Messwert"],
      interval = "h1",
      unit = dplyr::pull(header,2)[dplyr::pull(header,1) == "Einheit"],
      site = dplyr::recode(site, DUE = "Dübendorf-Empa", ZUE = "Zürich-Kaserne", BAS = "Basel-Binningen", BRM = "Beromünster", CHA = "Chaumont",
                           DAV = "Davos-Seehornwald", HAE = "Härkingen-A1", JUN = "Jungfraujoch", LAE = "Lägeren", LAU = "Lausanne-César-Roux",
                           LUG = "Lugano-Università", MAG = "Magadino-Cadenazzo", PAY = "Payerne", RIG = "Rigi-Seebodenalp", SIO = "Sion-Aéroport-A9",
                           TAE = "Tänikon"),
      unit = stringr::str_replace(unit, "ug", "µg")
    )
  data <- dplyr::mutate(data, value = as.numeric(value))
  data <- dplyr::mutate_if(data, is.character, factor)
  data <- dplyr::select(data, starttime, site, parameter, interval, unit, value)

  return(data)
}


#' Convert Ostluft dataset into standard long format
#'
#' @param data
#' @param keep_incomplete
#' @param tz
#' @param na.rm
#'
#' @keywords internal
restructure_monitoring_ostluft <- function(data, keep_incomplete = FALSE, tz = "Etc/GMT-1", na.rm = TRUE) {

  header <- dplyr::slice(data, 1:which(dplyr::pull(data, 1) == "Startzeit"))
  header <- dplyr::select(header, -1)
  data <- dplyr::slice(data, (which(dplyr::pull(data, 1) == "Startzeit") + 1):nrow(data))
  colnames(data)[1] <- "starttime"
  data <- dplyr::mutate(data, starttime = lubridate::parse_date_time(.data$starttime, c("dmYHMS", "dmYHM", "dmY"), tz = tz))

  col_ids <- rlang::names2(data)[-1]
  # FIXME: kann das vereinfacht werden mit pivot_longer? sollte eigentlich möglich sein
  sites <- c(header[1, ], recursive = TRUE)
  sites <- rlang::set_names(sites, col_ids)
  parameters <- c(header[which(dplyr::pull(header,1) %in% c("NO2","NO2_PS","O3","PM10","PM10h","PM2.5","PM2.5h")),], recursive = TRUE) #! ... Liste vervollständigen
  parameters <- rlang::set_names(parameters, col_ids)
  intervals <- c(header[which(dplyr::pull(header,1) %in% c("y1","m1","d1","h1","min30","min10")),], recursive = TRUE)
  intervals <- rlang::set_names(intervals, col_ids)
  units <- c(header[which(dplyr::pull(header,1) %in% c("µg/m3","ppb","ppm","ppt","°C","hPa","%","W/m2")),], recursive = TRUE) #! ... Liste vervollständigen
  units <- rlang::set_names(units, col_ids)


  data_long <- tidyr::gather(data, "id", "value", -"starttime", na.rm = na.rm, factor_key = TRUE)
  data_long <- dplyr::mutate(data_long,
                             site = dplyr::recode(.data$id, !!!sites),
                             parameter = dplyr::recode(.data$id, !!!parameters),
                             interval = dplyr::recode(.data$id, !!!intervals),
                             unit = dplyr::recode(.data$id, !!!units)
  )
  data_long <- dplyr::select(data_long, "starttime", "site", "parameter", "interval", "unit", "value")

  data_long_clean <-
    data_long |>
    dplyr::mutate(
      value = dplyr::case_when(
        keep_incomplete ~ as.numeric(gsub("\\*|\\;", "", value)),
        TRUE ~ as.numeric(value)
      )
    ) |>
    dplyr::mutate_if(is.character, factor)

  return(data_long_clean)
}


#' Make sure that there are no duplicate measurements per site / year / unit in Ostluft dataset
#'
#' @description
#' Function to make sure that there are no duplicate measurements per site / year / unit for data with interval = y1 in format rOstluft::format_rolf()
#' in case there have been NO2 monitor and passive sampler measurements (prefer monitor data = reference method);
#' same for PM10 monitor and high volume sampler measurements (prefer high-volume-sampler data = reference method);
#' same for PM2.5 monitor and high volume sampler measurements (prefer high-volume-sampler data = reference method)
#'
#' @param data
#'
#' @keywords internal
remove_duplicate_y1 <- function(data){

  replace_no2_ps <- function(parameter, value){
    if (sum(c("NO2", "NO2_PS") %in% parameter) == 2) {
      if (!is.na(value[which(parameter == "NO2")])){
        value[which(parameter == "NO2_PS")] <- NA
      }
    }
    return(value)
  }

  replace_pm10 <- function(parameter, value){
    if (sum(c("PM10", "PM10h") %in% parameter) == 2) {
      if (!is.na(value[which(parameter == "PM10h")])){
        value[which(parameter == "PM10")] <- NA
      }
    }
    return(value)
  }

  replace_pm25 <- function(parameter, value){
    if (sum(c("PM2.5", "PM2.5h") %in% parameter) == 2) {
      if (!is.na(value[which(parameter == "PM2.5h")])){
        value[which(parameter == "PM2.5")] <- NA
      }
    }
    return(value)
  }

  data <-
    data |>
    dplyr::group_by(starttime, site, unit) |>
    dplyr::mutate(
      value = replace_no2_ps(parameter, value),
      value = replace_pm10(parameter, value),
      value = replace_pm25(parameter, value)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(parameter = dplyr::recode_factor(parameter, !!!c("NO2_PS" = "NO2", "PM10h" = "PM10", "PM2.5h" = "PM2.5")))

  return(data)
}


#' Restructure Ostluft site metadata
#'
#' @param meta
#'
#' @keywords internal
prep_site_meta_ostluft <- function(meta) {

  meta <-
    meta |>
    dplyr::filter(msKT == "ZH" & !is.na(msNameAirMo) & !is.na(scSiedlungsgroesse) & !is.na(scVerkehrslage)) |>
    dplyr::mutate(
      site_long = paste(msOrt, msOrtsteil, sep = " - "),
      scSiedlungsgroesse = stringr::str_trim(scSiedlungsgroesse),
      scVerkehrslage = stringr::str_trim(scVerkehrslage)
    ) |>
    dplyr::select(msNameAirMo, site_long, spXCoord, spYCoord, spHoehe, scSiedlungsgroesse, scVerkehrslage) |>
    dplyr::rename(
      site = msNameAirMo,
      x = spXCoord,
      y = spYCoord,
      masl = spHoehe,
      zone = scSiedlungsgroesse,
      type = scVerkehrslage
    ) |>
    dplyr::mutate(
      zone = recode_ostluft_meta_zone(zone),
      type = recode_ostluft_meta_type(type),
      source = "Ostluft"
    )

  return(meta)
}


#' Restructure NABEL site metadata
#'
#' @param meta
#'
#' @keywords internal
prep_site_meta_nabel <- function(meta) {

  meta <- dplyr::distinct(meta, Station, `Ost Y`, `Nord X`, Höhe, Zonentyp, Stationstyp)
  meta <- dplyr::mutate(meta, Zonentyp = tolower(Zonentyp))
  meta <- dplyr::rename(meta,
                        site = Station,
                        y = `Ost Y`,
                        x = `Nord X`,
                        masl = Höhe,
                        zone = Zonentyp,
                        type = Stationstyp
  )
  meta <- dplyr::mutate(meta,
                        ifelse(zone == "vorstädtisch", "klein-/vorstädtisch", zone),
                        site_long = site,
                        source = "NABEL (BAFU & Empa)"
  )
  meta <- dplyr::select(meta, site, site_long, x, y, masl, zone, type, source)

  return(meta)
}


#' Copy from rOstluft::convert_interval()
#'
#' @param interval
#'
#' @keywords internal
convert_interval2 <- function(interval) {

  num <- stringr::str_extract(interval, "[:digit:]+")
  units <- stringr::str_extract(interval, "[:alpha:]+")
  units <- stringr::str_to_lower(units)
  if (is.na(num)) num <- "1"
  if (units == "m") units <- "month"
  if (units == "y") units <- "year"

  stringr::str_c(num, units, sep = " ")
}


#' Copy from rOstluft::pad_serie()
#'
#' @param serie
#' @param start_date
#' @param end_date
#' @param drop_last
#'
#' @keywords internal
pad_serie2 <- function(serie, start_date = NULL, end_date = NULL, drop_last = FALSE) {

  if (is.null(start_date)) {
    start_date <- min(serie$starttime)
  }

  if (is.null(end_date)) {
    end_date <- max(serie$starttime)
    drop_last <- FALSE
  }

  # by joining the data we insert rows with NA values for site, parameter, interval, unit, value
  # we need to fill this with the values from the supplied df
  fill.values <- dplyr::slice(serie, 1)
  fill.values <- as.list(dplyr::select(fill.values, -"starttime", -"value"))

  interval <- convert_interval2(fill.values$interval)

  all.dates <- tibble::tibble(
    starttime = seq(start_date, end_date, interval)
  )

  if (isTRUE(drop_last)) {
    all.dates <- utils::head(all.dates, -1)
  }

  padded <- dplyr::full_join(all.dates, serie, by = "starttime")
  tidyr::replace_na(padded, replace = fill.values)
}


#' Copy from rOstluft::pad() => because this is the only function we need from this package
#'
#' @param data
#' @param start_date
#' @param end_date
#' @param drop_last
#'
#' @keywords internal
pad2 <- function(data, start_date = NULL, end_date = NULL, drop_last = FALSE) {

  data.grouped <- dplyr::group_by(data, .data$site, .data$parameter, .data$interval, .data$unit)
  data.grouped <- dplyr::do(data.grouped, pad_serie2(.data, start_date, end_date, drop_last))

  return(dplyr::ungroup(data.grouped))
}


#' Identify relevant months per year for metric calculation based on O3 monthly mean data (used in O3 peak-season calculation)
#'
#' @param starttime
#' @param o3_m1
#'
#' @keywords internal
consecutive_months <- function(starttime, o3_m1) {

  data <- tibble::tibble(starttime, O3 = o3_m1)
  data <- dplyr::arrange(data, starttime)
  data <- dplyr::mutate(data,
                        O3_runmean = zoo::rollapply(.data$O3, 6, mean, fill = NA, align = "left"),
                        month_start = lubridate::month(data$starttime),
                        month_end = pmin(lubridate::month(data$starttime) + 5, 12),
                        n_months = month_end - month_start + 1
  )
  complete_months <- dplyr::filter(data, n_months == 6)
  peak_start <- dplyr::pull(dplyr::slice(complete_months, which.max(.data$O3_runmean)), starttime)
  if (length(peak_start) == 0) {
    peak_season <- rep(NA, nrow(data))
  } else {
    peak_season <- data$starttime %in% (peak_start + months(0:5))
    peak_season <- ifelse(data$n_months == 6 & is.na(data$O3_runmean), NA, peak_season)
  }

  return(peak_season)
}


#' Calculate daily maximum 8h running-mean O3 concentration based on O3 1h data in rOstluft::format_rolf() (used in O3 peak-season calculation)
#'
#' @param data
#'
#' @keywords internal
max_mean_h8gl <- function(data) { # how to solve data coverage?

  data <-
    data |>
    dplyr::group_by(date = lubridate::as_date(starttime), site, parameter, unit) |>
    dplyr::mutate(value = zoo::rollapply(.data$value, 8, mean, fill = NA, align = "right")) |>
    dplyr::slice(which.max(.data$value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      starttime = starttime - lubridate::hours(7),
      parameter = factor(paste0(parameter, "_max_mean_h8gl")),
      interval = factor("d1")
    ) |>
    dplyr::arrange(site, parameter, starttime) |>
    dplyr::select(starttime, site, parameter, interval, unit, value)

  return(data)
}


#' Calculate O3 peak-season concentration per year and site, based on data as hourly means in rOstluft::format_rolf()
#' @param data
#' @param min_coverage
#'
#' @keywords internal
calc_O3_peakseason <- function(data, min_coverage = 9/12) { # min_coverage: data coverage in months per year (9/12 because in early times, they used to not measure O3 during winter months)

  # calculate O3 monthly means to derive peak season
  data_m1 <- rOstluft::resample(data, statistic = "mean", new_interval = "m1", data_thresh = 0.8)

  # calculate O3 daily maximum 8 hour mean
  # data_h8gl <- rOstluft::resample(data, statistic = "mean", new_interval = "h8gl")
  # data_max_mean_h8gl <- rOstluft::resample(data_h8gl, statistic = "max", new_interval = "d1")
  data_max_mean_h8gl <- max_mean_h8gl(data)

  # make sure years are sufficiently data-covered
  coverage <-
    data_m1 |>
    dplyr::group_by(year = lubridate::year(lubridate::floor_date(starttime, unit = "1 year")), site) |>
    dplyr::summarise(n = sum(!is.na(value))) |>
    dplyr::ungroup()

  # identify peak-season 6 consecutive months per year and site
  peak_season <-
    data_m1 |>
    dplyr::mutate(year = lubridate::year(lubridate::floor_date(starttime, unit = "1 year"))) |>
    dplyr::left_join(coverage, by = c("year", "site")) |>
    dplyr::filter(n/12 >= min_coverage) |>
    dplyr::group_by(year = lubridate::floor_date(starttime, unit = "1 year"), site) |>
    dplyr::mutate(
      peak_season = consecutive_months(starttime, value),
      n = sum(peak_season, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(starttime, site, peak_season, n)

  # join with "max_mean_h8gl" data and filter only days within relevant consecutive months
  # calc mean per year (or more precisely peak-season) and site (= WHO metric peak-season)
  data_peakseason <-
    data_max_mean_h8gl |>
    dplyr::mutate(starttime = lubridate::floor_date(starttime, unit = "1 month")) |>
    dplyr::left_join(peak_season, by = c("starttime", "site")) |>
    dplyr::filter(peak_season) |>
    dplyr::group_by(starttime = lubridate::floor_date(starttime, unit = "1 year"), site, unit) |>
    dplyr::mutate(value = ifelse(n < 6, NA, value)) |>
    dplyr::summarise(value = mean(value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      parameter = "O3_peakseason_mean_d1_max_mean_h8gl",
      interval = "y1",
    ) |>
    dplyr::mutate_if(is.character, factor) |>
    dplyr::select(starttime, site, parameter, interval, unit, value) |>
    dplyr::arrange(site, parameter, starttime)

  return(data_peakseason)
}


#' Average stars raster data to another stars grid as mean values
#'
#' @param data
#' @param grid
#' @param method
#' @param na_val
#'
#' @keywords internal
average_to_grid <- function(data, grid, method = "average", na_val = -999) {

  parameter <- names(data)
  data <- stars::st_warp(data, grid, method = method, use_gdal = TRUE, no_data_value = na_val)
  names(data) <- parameter

  return(data)
}


#' Average stars raster data to the grid of BFS statpop dataset
#'
#' @param x
#' @param y
#'
#' @keywords internal
average_to_statpop <- function(x, y) {

    grid <- dplyr::select(x, RELI)
    data_avg <- purrr::map(y, function(data) average_to_grid(data, grid))

  return(data_avg)
}


#' Convert rasterdata into long format tibble
#'
#' @param data
#'
#' @keywords internal
simplify_aq_rasterdata <- function(data) {

  data <- purrr::map(names(data), function(pollutant) tibble::as_tibble(data[[pollutant]]))
  data <-
    data |>
    dplyr::bind_rows() |>
    tidyr::gather(pollutant, concentration, -x, -y) |>
    dplyr::filter(!is.na(concentration))

  return(data)
}


#' Merge air quality and statpop rasterdata with municipilty boundaries and convert to a common tibble
#'
#' @param data_raster
#' @param data_municip
#'
#' @keywords internal
merge_statpop_with_municipalities <- function(data_raster, data_municip) {

  municip_raster <-
    data_municip |>
    dplyr::select(bfs) |>
    stars::st_rasterize(data_raster)
  #TODO: terra::rasterize(..., cover = TRUE, touches = TRUE)

  data <-
    dplyr::left_join(
      tibble::as_tibble(municip_raster),
      tibble::as_tibble(data_raster),
      by = c("x","y")) |>
    dplyr::filter(!is.na(RELI) & RELI != 0 & !is.na(population))

  data <-
    data_municip |>
    st_drop_geometry() |>
    dplyr::select(bfs, gemeindename) |>
    dplyr::right_join(data, by = "bfs") |>
    dplyr::rename(bfsnr = bfs)

  return(data)
}


#' calculate specific health outcome
#'
#' @param conc_increment
#' @param crf_per_concunit
#' @param deathrate_per_person
#' @param population
#'
#' @keywords internal
calc_outcome <- function(conc_increment, crf, crf_conc_increment, cases) {

  # see:
  # Castro, A., Kutlar Joss, M., Röösli, M. (2023). Quantifizierung des Gesundheitsnutzens der neuen
  # Luftqualitätsleitlinien der Weltgesundheitsorganisation in der Schweiz. Im Auftrag vom Bundesamt für Umwelt.

  CB <- conc_increment
  C0 <- 0 # set to 0 here since concentration increment is already directly provided by function input
  CA <- crf_conc_increment
  EEA <- crf
  GD <- cases

  EEB <- exp(log(EEA) * (CB - C0) / CA)

  A <- GD * (1 - 1 / EEB)

  return(A)
}


#' Calculate range of health outcomes from input-dataset (most likely value, lower and upper confidence intervals crf)
#'
#' @param data
#' @param conc_threshold
#'
#' @keywords internal
calculate_all_outcomes <- function(data, conc_threshold = "lower_conc_threshold") {

  data <-
    data |>
    dplyr::mutate(
      conc_incr = pmax(0, population_weighted_mean - !!rlang::sym(conc_threshold)),
      outcome = calc_outcome(conc_incr, crf, crf_conc_increment, number_of_deaths),
      outcome_lower = calc_outcome(conc_incr, crf_lower, crf_conc_increment, number_of_deaths),
      outcome_upper = calc_outcome(conc_incr, crf_upper, crf_conc_increment, number_of_deaths),
    ) |>
    dplyr::select(-conc_incr)

  return(data)
}

#' Get year of health-outcome base scenario: either provided year or a provided function
#'
#' @param base
#' @param ...
#'
#' @keywords internal
get_base_scenario_year <- function(base = "min", ...) {

  if (is.character(base)) {
    fun <- function(x) get(base)(x, ...)
  } else {
    fun <- function(x) base
  }

  return(fun)
}


