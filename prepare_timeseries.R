

prepare_timeseries <- function() {
  # create an X meter buffer around each subblock to count the number of nearby 
  # cabi trips
  rs_cabi_buf <-
    rs |>
    st_transform(6487) |>          # EPSG:6487 — NAD83 / Maryland (metres)
    st_buffer(dist = 500) |>              # X m buffer
    st_transform(st_crs(cd))       # reproject back to match cd's CRS
  
  # Spatial join stations to buffers, then summarise by subblock
  rs_cabi_change <- rs_cabi_buf |>
    st_join(cabi_sf, join = st_contains) |>
    st_drop_geometry() |>
    group_by(BLOCKKEY, SUBBLOCKKEY) |>
    summarise(
      total_change_trips = sum(change_n_trips, na.rm = TRUE),
      n_stations         = sum(!is.na(change_n_trips)),
      .groups = "drop"
    )
  
  rm(rs_cabi_buf); quietly(gc())
  
  # OK now let's join the crashes to the subblocks, which we'll do with anther,
  # smaller, X meter buffer:
  rs_radius <- (rs$TOTALPARKINGLANES + rs$TOTALTRAVELLANES)*4 / 2 + 4
  rs_crash_buf <-
    rs |>
    st_transform(6487) |>                                       # EPSG:6487 — NAD83 / Maryland (meters)
    st_buffer(dist = rs_radius, endCapStyle = "FLAT") |>        # X m buffer
    st_transform(st_crs(cd)) |>                                 # reproject back to match cd's CRS
    select(ROUTEID, ROUTENAME, BLOCKKEY, SUBBLOCKKEY, 
           TOTALTRAVELLANES, TOTALPARKINGLANES, TOTALTRAVELLANEWIDTH,
           WARD_ID, DCFUNCTIONALCLASS
    ) %>%
    mutate(area_m = st_area(.))
  
  units(rs_crash_buf$area_m) <- NULL
  
  cd_assigned <- 
    cd |> 
    # Join crashes to buffers — one crash can match multiple buffers, since the buffers can overlap
    st_join(rs_crash_buf, join = st_within, left = FALSE) |>
    st_drop_geometry() |>
    # For crashes in multiple buffers, randomly keep one. this will add noise but that's probably OK.
    group_by(CRIMEID) |>
    slice_sample(n = 1) |>         # randomly pick one buffer per crash
    ungroup()
  
  
  # count up the number of crashes in each subblock
  cd_assigned_clean <-
    cd_assigned %>%
    mutate(WARD_ID = as.numeric(WARD_ID)) %>%
    filter(year %in% ANALYSIS_YEARS) %>%
    group_by(!!!sym(ANALYSIS_LEVEL), year) %>%
    summarise(
      across(all_of(injury_vars), \(x) sum(x, na.rm = TRUE)),
      n_crashes            = n(),
      TOTALTRAVELLANES     = first(TOTALTRAVELLANES),
      TOTALPARKINGLANES    = first(TOTALPARKINGLANES),
      TOTALTRAVELLANEWIDTH = first(TOTALTRAVELLANEWIDTH),
      WARD_ID              = first(WARD_ID),
      DCFUNCTIONALCLASS    = first(DCFUNCTIONALCLASS),
      area_m               = first(area_m),
      .groups   = "drop"
    ) %>%
    ungroup() %>%
    mutate(
      crashes_per_acre     = n_crashes / area_m * 4046.86
    ) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))
  
  rm(cd_assigned); gc()
  
  # ensures the data frame is a balanced panel
  # i.e., that every subblock has a row for every year from 2016 to 2025, with no gaps.
  cd_assigned_clean <- cd_assigned_clean |>
    complete(
      !!!syms(ANALYSIS_LEVEL),
      year = 2015:2025,
      fill = list(
        n_crashes            = 0,
        TOTALTRAVELLANES     = NA_real_,
        TOTALPARKINGLANES    = NA_real_,
        TOTALTRAVELLANEWIDTH = NA_real_,
        WARD_ID              = NA_real_,
        area_m               = NA_real_,
        DCFUNCTIONALCLASS    = NA_real_,
        crashes_per_acre     = 0,
        # injury vars
        MAJORINJURIES_BICYCLIST   = 0,
        MINORINJURIES_BICYCLIST   = 0,
        UNKNOWNINJURIES_BICYCLIST = 0,
        FATAL_BICYCLIST           = 0,
        MAJORINJURIES_DRIVER      = 0,
        MINORINJURIES_DRIVER      = 0,
        UNKNOWNINJURIES_DRIVER    = 0,
        FATAL_DRIVER              = 0,
        MAJORINJURIES_PEDESTRIAN  = 0,
        MINORINJURIES_PEDESTRIAN  = 0,
        UNKNOWNINJURIES_PEDESTRIAN= 0,
        FATAL_PEDESTRIAN          = 0,
        TOTAL_VEHICLES            = 0,
        TOTAL_BICYCLES            = 0,
        TOTAL_PEDESTRIANS         = 0
      )
    )
  
  # fill the time invariant data into the gaps:
  cd_assigned_clean <- cd_assigned_clean |>
    arrange(!!!sym(ANALYSIS_LEVEL), year) |>
    group_by(!!!sym(ANALYSIS_LEVEL)) |>
    fill(TOTALTRAVELLANES, TOTALPARKINGLANES, TOTALTRAVELLANEWIDTH, DCFUNCTIONALCLASS,
         WARD_ID, area_m, .direction = "downup") |>
    ungroup()
  
  # (finally!) join on the protected and buffered bike lane info:
  cd_pbl_clean <- 
    dplyr::left_join(cd_assigned_clean,
                     pbl %>% 
                       st_drop_geometry() %>%
                       select(!!!sym(ANALYSIS_LEVEL), build_year),
                     by=ANALYSIS_LEVEL
    ) %>% 
    filter(year %in% ANALYSIS_YEARS) %>%
    # clean up our data, remove COVID years. 
    # Our last 2 years (2024 and 2025) will serve as our post-treatment years for most subblocks.
    mutate(build_year = if_else(build_year == 99, NA, build_year)) %>%
    mutate(post_install = if_else(year >= 2024, 1, 0)) %>%
    # filter out crash counts that are probably data errors.
    # it seems unlikely that this one stretch of road in ward 7 had over 250 crashes in a year
    filter(n_crashes < 250) %>%
    # label treatment group types:
    group_by(!!!sym(ANALYSIS_LEVEL)) %>%
    mutate(treatment    = if_else(!is.na(build_year), 1, 0),
           never_taker  = if_else(treatment == 0    , 1, 0),
           always_taker = case_when(
             treatment == 0          ~ 0,
             build_year < 2020       ~ 1,
             TRUE                    ~ 0
           ),
           switcher     = case_when(
             treatment == 0                      ~ 0,
             build_year %in% seq(2020, 2023, 1)  ~ 1,
             TRUE                                ~ 0
           ),
           late_taker   = case_when(
             treatment == 0                 ~ 0,
             build_year %in% c(2024, 2025)  ~ 1,
             TRUE                           ~ 0
           ),
           check_sum    = never_taker + always_taker + switcher + late_taker) %>%
    ungroup()
  
  cd_pbl_clean <-
    cd_pbl_clean %>%
    mutate(
      n_injury_crashes = 
        MAJORINJURIES_BICYCLIST + MINORINJURIES_BICYCLIST + UNKNOWNINJURIES_BICYCLIST + FATAL_BICYCLIST +
        MAJORINJURIES_DRIVER + MINORINJURIES_DRIVER + UNKNOWNINJURIES_DRIVER + FATAL_DRIVER +
        MAJORINJURIES_PEDESTRIAN + MINORINJURIES_PEDESTRIAN + UNKNOWNINJURIES_PEDESTRIAN + FATAL_PEDESTRIAN,
      n_driver_injury_crashes = 
        MAJORINJURIES_DRIVER + MINORINJURIES_DRIVER + UNKNOWNINJURIES_DRIVER + FATAL_DRIVER,
      n_serious_driver_injury_crashes = 
        MAJORINJURIES_DRIVER + FATAL_DRIVER,
      n_bike_injury_crashes = 
        MAJORINJURIES_BICYCLIST + MINORINJURIES_BICYCLIST + UNKNOWNINJURIES_BICYCLIST + FATAL_BICYCLIST,
      n_serious_bike_injury_crashes = 
        MAJORINJURIES_BICYCLIST + FATAL_BICYCLIST,
      n_ped_injury_crashes = 
        MAJORINJURIES_PEDESTRIAN + MINORINJURIES_PEDESTRIAN + UNKNOWNINJURIES_PEDESTRIAN + FATAL_PEDESTRIAN,
      n_serious_ped_injury_crashes = 
        MAJORINJURIES_PEDESTRIAN + FATAL_PEDESTRIAN,
      
      injury_crashes_per_acre                = n_injury_crashes / area_m * 4046.86,
      driver_injury_crashes_per_acre         = n_driver_injury_crashes / area_m * 4046.86,
      serious_driver_injury_crashes_per_acre = n_serious_driver_injury_crashes / area_m * 4046.86,
      bike_injury_crashes_per_acre           = n_bike_injury_crashes / area_m * 4046.86,
      serious_bike_injury_crashes_per_acre   = n_serious_bike_injury_crashes / area_m * 4046.86,
      ped_injury_crashes_per_acre            = n_ped_injury_crashes / area_m  * 4046.86,
      serious_ped_injury_crashes_per_acre    = n_serious_ped_injury_crashes / area_m  * 4046.86,
      
      total_vehicles_per_acre                = TOTAL_VEHICLES / area_m  * 4046.86,
      total_bikes_per_acre                   = TOTAL_BICYCLES / area_m  * 4046.86,
      total_peds_per_acre                    = TOTAL_PEDESTRIANS / area_m  * 4046.86,
      
      treatment = if_else(is.na(build_year), 0, 1)
    ) %>%
    group_by(!!!sym(ANALYSIS_LEVEL)) %>%
    mutate(
      mean_crashes_pre2019 = mean(crashes_per_acre[year < 2019], na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(mean_crashes_pre2019 = na_if(mean_crashes_pre2019, NaN)) %>%
    group_by(!!!sym(ANALYSIS_LEVEL)) %>%
    mutate(mean_crashes_pre2019 = mean(mean_crashes_pre2019, na.rm=T)) %>%
    ungroup()
  
  
  cd_pbl_clean <- dplyr::left_join(cd_pbl_clean, rs_cabi_change, by=ANALYSIS_LEVEL)
  cd_pbl_clean <- dplyr::left_join(cd_pbl_clean, annual_cabi_rides, by="year")
  
  
  cd_pbl_clean_simplified <-
    cd_pbl_clean %>%
    group_by(!!!sym(ANALYSIS_LEVEL), post_install) %>%
    summarize(
      TOTALTRAVELLANEWIDTH = mean(TOTALTRAVELLANEWIDTH, na.rm=T),
      TOTALPARKINGLANES    = mean(TOTALPARKINGLANES, na.rm=T),
      DCFUNCTIONALCLASS    = mean(DCFUNCTIONALCLASS, na.rm=T),
      WARD_ID              = mean(WARD_ID, na.rm=T),
      total_change_trips   = mean(total_change_trips, na.rm=T),
      cabi_rides           = mean(cabi_rides, na.rm=T),
      
      switcher             = first(switcher),
      late_taker           = first(late_taker),
      never_taker          = first(never_taker),
      always_taker         = first(always_taker),
      
      across(ends_with("_per_acre"), \(x) mean(x, na.rm = TRUE)),
      
      treatment                            = mean(treatment, na.rm=T),
      mean_crashes_pre2019                 = mean(mean_crashes_pre2019, na.rm=T)
    ) %>%
    ungroup()
  
  
  
  return(list(cd_pbl_clean_simplified, cd_pbl_clean))
}
