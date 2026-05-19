
setwd("~/GitHub/ddot-project-evals")

# Downloads and row binds Capital Bikeshare trip data for a given year,
# returning a data frame with all the data from that year.
#
# Data source: https://capitalbikeshare.com/system-data
# Note: CaBi publishes one zip file per month containing a CSV of individual
# trips. 
# Args: year (int): The calendar year to retrieve data for (e.g. 2023).
# Returns: A tibble with one row per trip
#
# Example: cabi_2023 <- get_annual_cabi_data(2025)

get_annual_cabi_data <- function(year) {

  # Build one URL per month using CaBi's S3 naming convention:
  # YYYYMM-capitalbikeshare-tripdata.zip
  urls <- sprintf(
    "https://s3.amazonaws.com/capitalbikeshare-data/%d%02d-capitalbikeshare-tripdata.zip",
    year, 1:12
  )

  trips <- map(urls, \(url) {
    
    print("#")

    # Use a unique temp file and directory per month to avoid
    # collisions when running in parallel or across multiple calls
    tmp   <- tempfile(fileext = ".zip")
    exdir <- tempfile()
    dir.create(exdir)

    tryCatch({

      download.file(url, tmp, mode = "wb", quiet = TRUE)
      unzip(tmp, exdir = exdir)

      # Some months nest CSVs inside subdirectories within the zip,
      # so use recursive = TRUE to find them wherever they land
      csv_files <- list.files(exdir, pattern = "\\.csv$",
                              full.names = TRUE, recursive = TRUE)

      # Some months ship multiple CSVs (one per week); bind them all
      map_dfr(csv_files, read_csv, show_col_types = FALSE)

    }, error = \(e) {
      # Silently skip months that don't exist yet (e.g. future months)
      # or fail to download, but surface the error message for debugging
      message("Failed to retrieve: ", url, "\n", e$message)
      NULL
    })

  }) |>
    compact() |>   # drop NULLs from failed months
    bind_rows()

  trips
}

# download all the trip data:
raw_cabi_2025 <- get_annual_cabi_data(2025)
raw_cabi_2018 <- get_annual_cabi_data(2018)

# Aggregate to station level, keeping location coordinates for
# spatial joining downstream
cabi_2025 <-
  raw_cabi_2025 |>
  filter(!is.na(end_station_id)) |>
  group_by(end_station_id, end_lat, end_lng) |>
  summarise(n_trips_ended = n(), .groups = "drop") |>
  arrange(desc(n_trips_ended))

# write.csv(x = cabi_2025, file = "cabi_data_2025.csv", row.names = F)

cabi_2018 <-
  raw_cabi_2018 |>
  filter(!is.na(`End station number`)) |>
  group_by(`End station number`) |>
  summarise(n_trips_ended = n(), .groups = "drop") |>
  arrange(desc(n_trips_ended))

# write.csv(x = cabi_2018, file = "cabi_data_2018.csv", row.names = F)

# cabi_2018 <- readr::read_csv("cabi_data_2018.csv")
# cabi_2025 <- readr::read_csv("cabi_data_2025.csv")

# merge the 2018 and 2025 data based on the end station ID
cabi <-
  dplyr::full_join(
    cabi_2018 |>
      rename(n_trips_ended_2018 = n_trips_ended)
      ,
    select(cabi_2025, end_station_id, end_lat, end_lng, n_trips_ended) |>
      rename(n_trips_ended_2025 = n_trips_ended)
    ,
    by = c("End station number"="end_station_id")
    )

add_coords <- function(df, station_id, lat_lon) {

  df$end_lat[df$`End station number`==station_id] <- lat_lon[1]
  df$end_lng[df$`End station number`==station_id] <- lat_lon[2]

  df
}
# add lat lon for 2018 stations that were later removed but had at least
# 1000 trips end at that station in 2018
cabi <- add_coords(cabi, 31613, c(38.88429087871622, -76.99578835580621))
cabi <- add_coords(cabi, 31614, c(38.900116296819704, -76.99149861721462))
cabi <- add_coords(cabi, 31103, c(38.926263953061635, -77.03661594753648))
cabi <- add_coords(cabi, 31008, c(38.8629439551263, -77.05208676187382))

# write.csv(cabi, "data/cabi_both_years_clean.csv", row.names = F)












# Downloads Capital Bikeshare trip data one month at a time, summarising to
# station-level counts before accumulating, to avoid holding a full year of
# raw trip records in memory simultaneously.
#
# Args:
#   year        (int): Calendar year to retrieve (e.g. 2023).
#   summarise_fn (fn): A function that accepts a raw monthly tibble and
#                      returns a station-level summary tibble. Callers supply
#                      this so the schema differences between years (e.g. 2018
#                      vs 2025 column names) are handled outside the download
#                      logic.
# Returns: A tibble with one row per station (monthly counts summed).
get_annual_cabi_data <- function(year, summarise_fn) {
  urls <- sprintf(
    "https://s3.amazonaws.com/capitalbikeshare-data/%d%02d-capitalbikeshare-tripdata.zip",
    year, 1:12
  )
  
  monthly_summaries <- vector("list", length(urls))
  
  for (i in seq_along(urls)) {
    url <- urls[[i]]
    message("Downloading: ", url)
    tmp   <- tempfile(fileext = ".zip")
    exdir <- tempfile()
    dir.create(exdir)
    
    monthly_summaries[[i]] <- tryCatch({
      download.file(url, tmp, mode = "wb", quiet = TRUE)
      unzip(tmp, exdir = exdir)
      csv_files <- list.files(exdir, pattern = "\\.csv$",
                              full.names = TRUE, recursive = TRUE)
      # Read all CSVs for this month, summarise immediately, discard raw rows
      raw <- map_dfr(csv_files, read_csv, show_col_types = FALSE)
      summarise_fn(raw)                # <-- only the tiny summary is kept
    }, error = \(e) {
      message("Failed to retrieve: ", url, "\n", e$message)
      NULL
    }, finally = {
      # Clean up temp files after every month regardless of success/failure
      unlink(tmp)
      unlink(exdir, recursive = TRUE)
    })
  }
  
  bind_rows(compact(monthly_summaries))
}

# ── Per-year summarisation functions ──────────────────────────────────────────
# These encode the schema differences so get_annual_cabi_data stays generic.

summarise_2025 <- function(df) {
  df |>
    filter(!is.na(end_station_id)) |>
    group_by(end_station_id, end_lat, end_lng) |>
    summarise(n_trips_ended = n(), .groups = "drop")
}

summarise_2018 <- function(df) {
  df |>
    filter(!is.na(`End station number`)) |>
    group_by(`End station number`) |>
    summarise(n_trips_ended = n(), .groups = "drop")
}

# ── Download and aggregate ────────────────────────────────────────────────────
# get_annual_cabi_data() returns one row per station *per month*; the
# group_by + sum below collapses those into annual totals.

cabi_2025 <-
  get_annual_cabi_data(2025, summarise_2025) |>
  group_by(end_station_id, end_lat, end_lng) |>
  summarise(n_trips_ended = sum(n_trips_ended), .groups = "drop") |>
  arrange(desc(n_trips_ended))

cabi_2018 <-
  get_annual_cabi_data(2018, summarise_2018) |>
  group_by(`End station number`) |>
  summarise(n_trips_ended = sum(n_trips_ended), .groups = "drop") |>
  arrange(desc(n_trips_ended))

# ── Join 2018 and 2025 ────────────────────────────────────────────────────────
cabi <-
  dplyr::full_join(
    cabi_2018 |> rename(n_trips_ended_2018 = n_trips_ended),
    cabi_2025 |>
      select(end_station_id, end_lat, end_lng, n_trips_ended) |>
      rename(n_trips_ended_2025 = n_trips_ended),
    by = c("End station number" = "end_station_id")
  )


add_coords <- function(df, station_id, lat_lon) {
  
  df$end_lat[df$`End station number`==station_id] <- lat_lon[1]
  df$end_lng[df$`End station number`==station_id] <- lat_lon[2]
  
  df
}
# add lat lon for 2018 stations that were later removed but had at least
# 1000 trips end at that station in 2018
cabi <- add_coords(cabi, 31613, c(38.88429087871622, -76.99578835580621))
cabi <- add_coords(cabi, 31614, c(38.900116296819704, -76.99149861721462))
cabi <- add_coords(cabi, 31103, c(38.926263953061635, -77.03661594753648))
cabi <- add_coords(cabi, 31008, c(38.8629439551263, -77.05208676187382))

# write.csv(cabi, "data/cabi_both_years_clean.csv", row.names = F)







