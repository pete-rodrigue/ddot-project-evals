

## This file is for loading all the data, including:
#' The crash data (cd)
#' the protected bike lane data (pbl)
#' the CaBi data (cabi)
#' the roadway segment data (rs)
#' 

load_crash_data <- function() {
  # load crash data
  # data downloaded from here:
  # https://opendata.dc.gov/datasets/crashes-in-dc/about
  cd <- readr::read_csv("data/Crashes_in_DC.csv", show_col_types = F) |>
    filter(!is.na(X), !is.na(Y)) |>
    st_as_sf(coords = c("X", "Y"), crs = 3857) |>  # tell sf the points are in Web Mercator
    st_transform(4326) |>                          # reproject to WGS84
    mutate(
      crash_lon = st_coordinates(geometry)[, 1],
      crash_lat = st_coordinates(geometry)[, 2],
      # create year and month of crash variables:
      year  = as.numeric(substr(x = REPORTDATE, start = 1, stop = 4)),
      month = as.numeric(substr(x = REPORTDATE, start = 6, stop = 7))
    ) |> 
  # remove rows with no date information:
  filter(!is.na(year))
  
  # this data has crashes going back to the 2000s, but the data doesn't look super complete until 2014 or so: table(cd$year)
  # According to DDOT, the location information in the data set became much more accurate starting in 2016:
  # https://ddotwiki.atlassian.net/wiki/spaces/GIS0225/pages/2053603429/Crash+Data
  
  # subset to just variables we care about:
  cd <- 
    cd %>%
    mutate(
           date                = make_date(year, month, day = 1L),
          `Any Injury`         = MAJORINJURIES_BICYCLIST    + MINORINJURIES_BICYCLIST   +
                                 UNKNOWNINJURIES_BICYCLIST  + FATAL_BICYCLIST           +
                                 MAJORINJURIES_DRIVER       + MINORINJURIES_DRIVER      +
                                 UNKNOWNINJURIES_DRIVER     + FATAL_DRIVER              +
                                 MAJORINJURIES_PEDESTRIAN   + MINORINJURIES_PEDESTRIAN  +
                                 UNKNOWNINJURIES_PEDESTRIAN + FATAL_PEDESTRIAN,
          `Vehicle Crashes`    = if_else(TOTAL_VEHICLES     > 0, 1, 0),
          `Bike Crashes`       = if_else(TOTAL_BICYCLES     > 0, 1, 0),
          `Pedestrian Crashes` = if_else(TOTAL_PEDESTRIANS  > 0, 1, 0),
          
          `Driver Injury`      = MAJORINJURIES_DRIVER       + MINORINJURIES_DRIVER      + UNKNOWNINJURIES_DRIVER,
          `Bicyclist Injury`   = MAJORINJURIES_BICYCLIST    + MINORINJURIES_BICYCLIST   + UNKNOWNINJURIES_BICYCLIST,
          `Pedestrian Injury`  = MAJORINJURIES_PEDESTRIAN   + MINORINJURIES_PEDESTRIAN  + UNKNOWNINJURIES_PEDESTRIAN,
          
          `Driver Serious Injury`      = MAJORINJURIES_DRIVER,
          `Bicyclist Serious Injury`   = MAJORINJURIES_BICYCLIST,
          `Pedestrian Serious Injury`  = MAJORINJURIES_PEDESTRIAN,
    ) %>%
    filter(!is.na(date)) %>%
    select(CRIMEID, year, month, date, crash_lon, crash_lat, ends_with(" Crashes"), ends_with(" Injury")) %>%
  
  return(cd)
}


# load labeled PBL data that has the road subblock IDs and the year in which 
# PBLs were installed on those subblocks (we hand labeled them)
load_pbl_data <- function(PBLS_ONLY) {
  pbl <- 
    st_read("data/Protected-Buffered-Bike-Lanes.geojson", quiet=T) %>%
    filter(!is.na(install_year)) %>% 
    filter(install_year != 99) %>%
    rename(build_year = install_year) 
  
  if (PBLS_ONLY) {
    pbl <- filter(pbl, (!is.na(BIKELANE_PROTECTED)) | (!is.na(BIKELANE_DUAL_PROTECTED)))
  }
  
  pbl
}





# go to this other file to download and clean the Cabi data if you need to:
# source("clean_cabi_data.R")

load_cabi_data <- function() {
  # otherwise just load the cleaned data:
  cabi <- readr::read_csv("data/cabi_both_years_clean.csv", show_col_types = F)
  
  cabi <-
    cabi |>
    mutate(
      n_trips_ended_2018 = if_else(is.na(n_trips_ended_2018), 0, n_trips_ended_2018),
      n_trips_ended_2025 = if_else(is.na(n_trips_ended_2025), 0, n_trips_ended_2025),
      change_n_trips     = n_trips_ended_2025 - n_trips_ended_2018
    ) %>%
    filter(!is.na(end_lat))
  
  # # Diverging palette centered at 0 — red = fewer trips, blue = more trips
  # pal <- colorNumeric(
  #   palette = "RdBu",
  #   domain  = cabi$change_n_trips,
  #   reverse = FALSE
  # )
  # leaflet(cabi) |>
  #   addProviderTiles(providers$CartoDB.Positron) |>
  #   addCircleMarkers(
  #     lng          = ~end_lng,
  #     lat          = ~end_lat,
  #     radius       = 6,
  #     color        = "white",
  #     weight       = 0.5,
  #     fillColor    = ~pal(change_n_trips),
  #     fillOpacity  = 0.85,
  #     label = ~paste0(
  #       "<b>", `End station number`, "</b><br>",
  #       "2018 trips: ", scales::comma(n_trips_ended_2018), "<br>",
  #       "2025 trips: ", scales::comma(n_trips_ended_2025), "<br>",
  #       "Change: ", scales::comma(change_n_trips)
  #     ) |> lapply(htmltools::HTML),
  #     labelOptions = labelOptions(textsize = "12px", direction = "auto")
  #   ) |>
  #   addLegend(
  #     position = "bottomright",
  #     pal      = pal,
  #     values   = ~change_n_trips,
  #     title    = "Change in<br>annual trips",
  #     labFormat = labelFormat(big.mark = ","),
  #     opacity  = 0.9
  #   ) |>
  #   addScaleBar(position = "bottomleft")
  
  # Convert cabi to sf object
  cabi_sf <- cabi |>
    filter(!is.na(end_lat), !is.na(end_lng)) |>
    st_as_sf(coords = c("end_lng", "end_lat"), crs = 4326) |>
    st_transform(st_crs(4326))
  
  
  # add rough CaBi ride totals by year:
  annual_cabi_rides <-
    data.frame(
      "year"       = c(2017, 2024, 2025),
      "cabi_rides" = c(3.8 , 6.1 , 6.6)
    )
  
  return(list(cabi_sf, annual_cabi_rides))
}




# load all roadway sub-block data, from here:
# https://opendata.dc.gov/datasets/df571ab7fea446e396bf2862d0ab6833_162/explore?location=38.927746%2C-77.024575%2C18
# This will be our main data set we merge other data onto.
load_subblocks <- function() {
  st_read("data/Roadway_SubBlock.geojson", quiet=T) %>%
    filter(ROADTYPE == 1) %>%             # keep just streets, remove service roads, ramps, alleys, driveways, trails, and walkways
    st_zm(drop = TRUE, what = "ZM") %>%   # drops Z (and M if present) → pure XY
    st_transform(4326) %>%                # Leaflet requires WGS84 (EPSG:4326) — reproject if needed
    st_cast("MULTILINESTRING")
}