#' Prepare and merge monitoring site metadata from Ostluft and NABEL for use in scripts
#'
#' @param meta_ostluft ...
#' @param meta_nabel ...
#'
#' @keywords internal
prepare_monitoring_meta <- function(meta_ostluft, meta_nabel) {

  meta_ostluft <- prep_site_meta_ostluft(meta_ostluft)
  meta_nabel <- prep_site_meta_nabel(meta_nabel)

  meta <-
    meta_ostluft |>
    dplyr::bind_rows(meta_nabel) |>
    dplyr::mutate(siteclass = paste(zone, type, sep = " - ")) |>
    dplyr::select(-zone, -type)

  return(meta)
}


#' Prepare usable tibble with air pollutant year-statistics exported and read from https://www.arias.ch/ibonline/ib_online.php and restructure the data similar to a standard long-format (see rOstluft::format_rolf())
#'
#' @param data ...
#' @param keep_incomplete ...
#' @param tz ...
#'
#' @keywords internal
prepare_monitoring_nabel_y1 <- function(data, metrics = list(Jahresmittel = c("NO2", "PM10", "PM2.5", "EC / Russ"), "höchster 98%-Wert eines Monats" = "O3"), keep_incomplete = FALSE, tz = "Etc/GMT-1") {

  metrics <-
    tibble::as_tibble(metrics) |>
    tidyr::gather(metric, pollutant) |>
    dplyr::distinct(metric, pollutant) |>
    dplyr::mutate(expr = paste0("(Messparameter == '", metric, "' & Schadstoff == '", pollutant, "')"))

  metrics_filter <- paste(metrics$expr, collapse = " | ")

  data <-
    data |>
    dplyr::filter(eval(rlang::parse_expr(metrics_filter))) |>
    restructure_monitoring_nabel_y1() |>
    dplyr::mutate(
      interval = "y1",
      Schadstoff = dplyr::case_when(
        Messparameter == "höchster 98%-Wert eines Monats" ~ "O3_max_98p_m1",
        Messparameter == "Anzahl Stundenmittel > 120 µg/m3" ~ "O3_nb_h1>120",
        Messparameter == "Dosis AOT40f" ~ "O3_AOT40",
        Schadstoff == "Partikelanzahl" ~ "PN",
        Schadstoff == "EC / Russ" ~ "eBC",
        TRUE ~ Schadstoff
      ),
      Einheit = ifelse(Einheit == "ppm·h", "ppm*h", Einheit),
      starttime = as.POSIXct(paste0(starttime, "-01-01"), tz = tz),
      source = factor("NABEL (BAFU & Empa)")
    ) |>
    dplyr::select(
      starttime,
      site = Station,
      parameter = Schadstoff,
      interval,
      unit = Einheit,
      value,
      source
    ) |>
    dplyr::mutate_if(is.character, as.factor)

  return(data)
}


#' Restructure Ostluft monitoring data from database export and remove duplicate NO2 and PM values per site due to different measurement methods
#'
#' @param data ...
#' @param keep_incomplete ...
#' @param tz ...
#'
#' @keywords internal
prepare_monitoring_ostluft_y1 <- function(data, keep_incomplete = FALSE, tz = "Etc/GMT-1") {

  data <- restructure_monitoring_ostluft(data, keep_incomplete = keep_incomplete, tz = tz, na.rm = TRUE)

  # remove duplicate parameters in Ostluft data => use preferred method (NO2 passive samplers vs. monitor & PM10/PM2.5 HVS vs. monitor)
  data <- remove_duplicate_y1(data)

  data <- dplyr::mutate(data, source = factor("ostluft"))

  return(data)
}


#' Calculate O3 peak-season metric from Ostluft monitoring h1 data
#'
#' @param data ...
#' @param keep_incomplete ...
#' @param tz ...
#'
#' @keywords internal
prepare_monitoring_ostluft_h1 <- function(data, keep_incomplete = FALSE, tz = "Etc/GMT-1") {

  data <-
    data |>
    restructure_monitoring_ostluft(keep_incomplete = keep_incomplete, tz = tz, na.rm = TRUE) |>
    calc_O3_peakseason() |>
    dplyr::mutate(source = factor("Ostluft"))

  return(data)
}


#' Calculate O3 peak-season metric from NABEL monitoring h1 data
#'
#' @param data ...
#' @param keep_incomplete ...
#' @param tz ...
#'
#' @keywords internal
prepare_monitoring_nabel_h1 <- function(data, keep_incomplete = FALSE, tz = "Etc/GMT-1") {

  data <-
    data |>
    lapply(restructure_monitoring_nabel_h1) |>
    dplyr::bind_rows() |>
    calc_O3_peakseason() |>
    dplyr::mutate(source = factor("NABEL (BAFU & Empa)"))

  return(data)
}


#' Merge monitoring dataset with site metadata, derive pollutant and metric variable and prepare final dataset
#'
#' @param data ...
#' @param meta ...
#'
#' @keywords internal
prepare_monitoring_aq <- function(data, meta) {

  data <-
    data |>
    pad2() |>
    dplyr::mutate(
      year = lubridate::year(starttime),
      parameter = dplyr::recode(parameter, "O3_max_98%_m1" = "O3_max_98p_m1") # for technical reasons
    ) |>
    dplyr::select(-source) |>
    dplyr::left_join(meta, by = "site") |>
    dplyr::filter(!is.na(siteclass)) |>
    dplyr::arrange(site, parameter, starttime) |>
    dplyr::mutate(
      pollutant = shortpollutant(parameter),
      metric = longmetric(parameter)
    ) |>
    dplyr::rename(concentration = value) |>
    dplyr::select(year, site, site_long, siteclass, x, y, masl, pollutant, metric, parameter, concentration, unit, source)

  return(data)
}

