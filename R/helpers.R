#' Recode parameter string to short pollutant string
#'
#' @param x ...
#'
#' @keywords internal
shortpollutant <- function(x) {

  long <- dplyr::case_when(
    x == "O3_max_98p_m1" ~ "O3",
    x == "O3_peakseason_mean_d1_max_mean_h8gl" ~ "O3",
    x == "O3_nb_h1>120" ~ "O3",
    TRUE ~ x
  )

  return(long)
}


#' Recode short pollutant/parameter string to long metric string
#'
#' @param x ...
#'
#' @keywords internal
longmetric <- function(x) {

  long <- dplyr::case_when(
    x == "PM10" ~ "Jahresmittel",
    x == "PM2.5" ~ "Jahresmittel",
    x == "NO2" ~ "Jahresmittel",
    x == "NMVOC" ~ "Jahresmittel",
    x == "NH3" ~ "Jahresmittel",
    x == "CO" ~ "Jahresmittel",
    x == "SO2" ~ "Jahresmittel",
    x == "NOx" ~ "Jahresmittel",
    x == "eBC" ~ "Jahresmittel",
    x == "O3" ~ "Jahresmittel",
    x == "O3_max_98p_m1" ~ "typische Spitzenbelastung",
    x == "O3_peakseason_mean_d1_max_mean_h8gl" ~ "mittlere Sommertagbelastung",
    # x == "O3_max_98p_m1" ~ "höchstes monatl. 98%-Perzentil der ½-Stundenmittel",
    # x == "O3_peakseason_mean_d1_max_mean_h8gl" ~ "mittlere sommerliche Tagesbelastung",
    x == "O3_nb_h1>120" ~ "Anzahl Stundenmittel > 120 μg/m3",
    TRUE ~ x
  )

  return(long)
}


#' Derive NH3 emission surrounding siteclass from GVE & N-Austrag within 5km
#'
#' @param x ...
#'
#' @keywords internal
siteclass_nh3 <- function(gve, nfert) {

  siteclass <-
    dplyr::case_when(
      nfert <= 50 & gve <= 5000 ~ "tief",
      (nfert > 50 & nfert <= 100 & gve <= 5000) | (gve > 5000 & gve <= 10000 & nfert <= 100) ~ "mittel",
      nfert > 100 | gve > 10000  ~ "hoch",
      TRUE ~ NA
    )

  siteclass = factor(siteclass, levels = c("tief", "mittel", "hoch"))

  return(siteclass)
}
