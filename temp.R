

stac_version = "0.9"
id = "ch.meteoschweiz.ogd-smn"

metadata_url <- paste0("https://data.geo.admin.ch/api/stac/v",stac_version,"/collections/",id,"/items")
metadata <- rjson::fromJSON(file = metadata_url)
url <- unlist(purrr::map(metadata$features, function(x) x$assets[which(grepl("csv", x$assets))][[1]]$href))



# MeteoSchweiz Bodenmessstationen im Kanton Zürich
stations <- airquality.methods::read_local_csv("https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_stations.csv")
# dplyr::filter(stations, station_canton == "ZH" & station_type_de == "Automatische Wetterstationen") |> View()
sites_meteo <- c("KLO", "FLU", "HOE")
stations <- dplyr::filter(stations, station_abbr %in% !!sites_meteo)

# Messparameter
parameters <- airquality.methods::read_local_csv("https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_parameters.csv")
parameters <- dplyr::select(parameters, parameter_shortname, parameter_unit, parameter_description_de)

# d1 Messdaten von sites_meteo
urls <- airquality.methods:::get_geo_admin_metadata("ch.meteoschweiz.ogd-smn", filter = "csv")
urls <- sapply(tolower(sites_meteo), function(x) urls[stringr::str_detect(urls, x)])
data_met_d1 <- airquality.methods::read_local_csv(urls)
