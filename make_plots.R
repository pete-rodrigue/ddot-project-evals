## This file has functions that plot charts


# leaflet() |>
#   addProviderTiles(providers$CartoDB.Positron) |>
#   addPolygons(
#     data = rs_crash_buf %>% 
#             st_crop(xmin = -77.015, ymin = 38.94, xmax = -77.01, ymax = 38.944), 
#     fillColor = "blue", 
#     stroke=F) |>
#   addCircleMarkers(
#     data = cd %>% 
#       st_crop(xmin = -77.015, ymin = 38.94, xmax = -77.010, ymax = 38.944) %>%
#       filter(year==2025),
#     lat=~crash_lat, 
#     lng=~crash_lon, 
#     stroke=F, color="red", radius = 4)


plot_leaflet <- function(points_sf = NULL, polygons_sf = NULL, bbox = NULL) {
  
  # Validate that at least one layer is provided
  if (is.null(points_sf) && is.null(polygons_sf)) {
    stop("At least one of points_sf or polygons_sf must be provided.")
  }
  
  # Leaflet requires WGS84
  if (!is.null(points_sf))   points_sf   <- st_transform(points_sf,   4326)
  if (!is.null(polygons_sf)) polygons_sf <- st_transform(polygons_sf, 4326)
  
  if (!is.null(bbox)) {
    crop_box <- st_bbox(
      c(xmin = bbox["xmin"], ymin = bbox["ymin"],
        xmax = bbox["xmax"], ymax = bbox["ymax"]),
      crs = 4326
    )
    if (!is.null(points_sf))   points_sf   <- st_crop(points_sf,   crop_box)
    if (!is.null(polygons_sf)) polygons_sf <- st_crop(polygons_sf, crop_box)
  }
  
  m <- leaflet() |>
    addProviderTiles(providers$CartoDB.Positron)
  
  if (!is.null(polygons_sf)) {
    m <- m |>
      addPolygons(
        data        = polygons_sf,
        fillColor   = "blue",
        fillOpacity = 0.4,
        stroke      = FALSE
      )
  }
  
  if (!is.null(points_sf)) {
    m <- m |>
      addCircleMarkers(
        data        = points_sf,
        color       = "red",
        fillColor   = "red",
        fillOpacity = .7,
        stroke      = FALSE,
        radius      = 4
      )
  }
  
  m
}

# Usage:
# plot_leaflet(points_sf = my_points, polygons_sf = my_polygons)
# plot_leaflet(points_sf = my_points)
# plot_leaflet(polygons_sf = my_polygons, bbox = c(xmin = -77.044, ymin = 38.909,
#                                                   xmax = -77.010, ymax = 38.944))


make_density_plots <- function(data, group_col, log_transform = FALSE) {
  
  plot_data <- data |>
    filter(.data[[group_col]] == 1) |>
    select(SUBBLOCKKEY, post_install, all_of(plot_vars)) |>
    pivot_longer(
      cols      = all_of(plot_vars),
      names_to  = "variable",
      values_to = "value"
    ) |>
    mutate(
      period   = if_else(post_install == 1, "Post", "Pre"),
      variable = factor(variable, levels = plot_vars, labels = var_labels[plot_vars]),
      value    = if (log_transform) log(value + 0.001) else value
    )
  
  plot_data |>
    ggplot(aes(x = value, fill = period, color = period)) +
    geom_density(alpha = 0.35, linewidth = 0.6) +
    facet_wrap(~ variable, scales = "free", ncol = 2) +
    scale_fill_manual(values  = c("Pre" = "#2166ac", "Post" = "#d7191c")) +
    scale_color_manual(values = c("Pre" = "#2166ac", "Post" = "#d7191c")) +
    labs(
      title    = group_labels[[group_col]],
      subtitle = "Pre vs. post period crash rate distributions by subblock",
      x        = if (log_transform) "log(Crashes per acre + 0.001)" else "Crashes per acre",
      y        = "Density",
      fill     = "Period",
      color    = "Period"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position  = "top",
      strip.text       = element_text(face = "bold", size = 9),
      plot.title       = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}



make_ecdf_plots <- function(data, group_col) {
  
  data |>
    filter(.data[[group_col]] == 1) |>
    select(SUBBLOCKKEY, post_install, all_of(plot_vars)) |>
    pivot_longer(
      cols      = all_of(plot_vars),
      names_to  = "variable",
      values_to = "value"
    ) |>
    mutate(
      period   = if_else(post_install == 1, "Post", "Pre"),
      variable = factor(variable, levels = plot_vars, labels = var_labels[plot_vars])
    ) |>
    ggplot(aes(x = value, color = period)) +
    stat_ecdf(linewidth = 0.7) +
    facet_wrap(~ variable, scales = "free_x", ncol = 2) +
    scale_color_manual(values = c("Pre" = "#2166ac", "Post" = "#d7191c")) +
    labs(
      title    = group_labels[[group_col]],
      subtitle = "Empirical CDF of crash rates by subblock, pre vs. post period",
      x        = "Crashes per acre",
      y        = "Proportion of subblocks at or below value",
      color    = "Period"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position  = "top",
      strip.text       = element_text(face = "bold", size = 9),
      plot.title       = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}






summarise_outcomes_by_period <- function(data) {
  
  stats <- c("min", "p25", "median", "mean", "p75", "p90", "p95", "max")
  
  summarise_one_period <- function(df) {
    df |>
      summarise(across(
        all_of(plot_vars),
        list(
          min    = \(x) min(x,            na.rm = TRUE),
          p25    = \(x) quantile(x, 0.25, na.rm = TRUE),
          median = \(x) median(x,          na.rm = TRUE),
          mean   = \(x) mean(x,            na.rm = TRUE),
          p75    = \(x) quantile(x, 0.75, na.rm = TRUE),
          p90    = \(x) quantile(x, 0.90, na.rm = TRUE),
          p95    = \(x) quantile(x, 0.95, na.rm = TRUE),
          max    = \(x) max(x,            na.rm = TRUE)
        )
      )) |>
      pivot_longer(
        everything(),
        names_to  = c("variable", "stat"),
        names_pattern = "^(.+)_(min|p25|median|mean|p75|p90|p95|max)$",
        values_to = "value"
      ) |>
      mutate(variable = var_labels[variable])
  }
  
  pre  <- data |> filter(post_install == 0) |> summarise_one_period() |> rename(pre  = value)
  post <- data |> filter(post_install == 1) |> summarise_one_period() |> rename(post = value)
  
  left_join(pre, post, by = c("variable", "stat")) |>
    mutate(stat = factor(stat, levels = stats)) |>
    pivot_wider(
      names_from  = stat,
      values_from = c(pre, post),
      names_glue  = "{stat}_{.value}"
    ) |>
    # Explicitly select columns in the right order
    select(variable, map(stats, \(s) c(paste0(s, "_pre"), paste0(s, "_post"))) |> unlist()) |>
    arrange(match(variable, var_labels))
}

# Usage:
# summarise_outcomes_by_period(cd_pbl_clean_simplified)

# Switchers only:
# cd_pbl_clean_simplified |>
#   filter(switcher == 1) |>
#   summarise_outcomes_by_period()
