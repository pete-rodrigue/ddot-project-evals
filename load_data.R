

## This file is for loading all the data, including:
#' The crash data (cd)
#' the protected bike lane data (pbl)
#' the CaBi data (cabi)
#' the roadway segment data (rs)
#' 


# load crash data
# data downloaded from here:
# https://opendata.dc.gov/datasets/crashes-in-dc/about
cd <- readr::read_csv("data/Crashes_in_DC.csv", show_col_types = F)

# create year and month of crash variables:
cd$year  <- as.numeric(substr(x = cd$REPORTDATE, start = 1, stop = 4))
cd$month <- as.numeric(substr(x = cd$REPORTDATE, start = 6, stop = 7))

# remove rows with no date information:
cd <- filter(cd, !is.na(year))

# this data has crashes going back to the 2000s, but the data doesn't look super complete until 2014 or so:
# table(cd$year)
# According to DDOT, the location information in the data set became much more accurate starting in 2016:
# https://ddotwiki.atlassian.net/wiki/spaces/GIS0225/pages/2053603429/Crash+Data

# project crash data:
cd <- cd |>
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# injury variables we care about:
injury_vars <- c(
  "MAJORINJURIES_BICYCLIST",   "MINORINJURIES_BICYCLIST",
  "UNKNOWNINJURIES_BICYCLIST", "FATAL_BICYCLIST",
  "MAJORINJURIES_DRIVER",      "MINORINJURIES_DRIVER",
  "UNKNOWNINJURIES_DRIVER",    "FATAL_DRIVER",
  "MAJORINJURIES_PEDESTRIAN",  "MINORINJURIES_PEDESTRIAN",
  "UNKNOWNINJURIES_PEDESTRIAN","FATAL_PEDESTRIAN",
  "TOTAL_VEHICLES", "TOTAL_BICYCLES", "TOTAL_PEDESTRIANS"
)

# subset to just variables we care about:
cd <- select(cd, CRIMEID, year, month, injury_vars)

coords <- st_coordinates(cd)
cd$crash_lon <- coords[, "X"]
cd$crash_lat <- coords[, "Y"]
rm(coords)

# load labeled PBL data that has the road subblock IDs and the year in which 
# PBLs were installed on those subblocks (we hand labeled them)
pbl <- 
  st_read("data/Protected-Buffered-Bike-Lanes.geojson", quiet=T) %>%
  filter(!is.na(install_year)) %>% 
  filter(install_year != 99) %>%
  rename(build_year = install_year) 

if (PBLS_ONLY) {
  pbl <- filter(pbl, (!is.na(BIKELANE_PROTECTED)) | (!is.na(BIKELANE_DUAL_PROTECTED)))
}





# go to this other file to download and clean the Cabi data if you need to:
# source("clean_cabi_data.R")

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



# load all roadway sub-block data, from here:
# https://opendata.dc.gov/datasets/df571ab7fea446e396bf2862d0ab6833_162/explore?location=38.927746%2C-77.024575%2C18
# This will be our main data set we merge other data onto.
rs <- 
  st_read("data/Roadway_SubBlock.geojson", quiet=T) %>%
  filter(ROADTYPE == 1) %>%             # keep just streets, remove service roads, ramps, alleys, driveways, trails, and walkways
  st_zm(drop = TRUE, what = "ZM") %>%   # drops Z (and M if present) → pure XY
  st_transform(4326) %>%                # Leaflet requires WGS84 (EPSG:4326) — reproject if needed
  st_cast("MULTILINESTRING")