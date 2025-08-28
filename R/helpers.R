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
