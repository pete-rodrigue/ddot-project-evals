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


plot_leaflet <- function(points_sf = NULL, polygons_sf = NULL, lines_sf = NULL, bbox = NULL) {
  
  # Validate that at least one layer is provided
  if (is.null(points_sf) && is.null(polygons_sf) && is.null(lines_sf)) {
    stop("At least one of points_sf, polygons_sf, or lines_sf must be provided.")
  }
  
  # Leaflet requires WGS84
  if (!is.null(points_sf))   points_sf   <- st_transform(points_sf,   4326)
  if (!is.null(polygons_sf)) polygons_sf <- st_transform(polygons_sf, 4326)
  if (!is.null(lines_sf))    lines_sf    <- st_transform(lines_sf,    4326)
  
  if (!is.null(bbox)) {
    crop_box <- st_bbox(
      c(xmin = bbox["xmin"], ymin = bbox["ymin"],
        xmax = bbox["xmax"], ymax = bbox["ymax"]),
      crs = 4326
    )
    if (!is.null(points_sf))   points_sf   <- st_crop(points_sf,   crop_box)
    if (!is.null(polygons_sf)) polygons_sf <- st_crop(polygons_sf, crop_box)
    if (!is.null(lines_sf))    lines_sf    <- st_crop(lines_sf,    crop_box)
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
  
  if (!is.null(lines_sf)) {
    m <- m |>
      addPolylines(
        data   = lines_sf,
        color  = "green",
        weight = 3,
        opacity = 0.8
      )
  }
  
  if (!is.null(points_sf)) {
    m <- m |>
      addCircleMarkers(
        data        = points_sf,
        color       = "red",
        fillColor   = "red",
        fillOpacity = 0.7,
        stroke      = FALSE,
        radius      = 4
      )
  }
  
  m
}

# Usage:
# plot_leaflet(lines_sf = my_lines)
# plot_leaflet(points_sf = my_points, lines_sf = my_lines)
# plot_leaflet(points_sf = my_points, polygons_sf = my_polys, lines_sf = my_lines,
#              bbox = c(xmin = -77.044, ymin = 38.909, xmax = -77.010, ymax = 38.944))


plot_crashes_over_time <- function(data, smoothing_factor = 1,
                                   period_1 = NULL, period_2 = NULL,
                                   title = NULL) {
  
  series_levels <- c("All Crashes", "Any Injury",
                     "Vehicle Crashes", "Bike Crashes",      "Pedestrian Crashes",
                     "Driver Injury",   "Bicyclist Injury",  "Pedestrian Injury")
  
  series_colors <- c(
    "All Crashes"        = "#636363",
    "Any Injury"         = "#bdbdbd",
    "Vehicle Crashes"    = "#2166ac",
    "Driver Injury"      = "#74add1",
    "Bike Crashes"       = "#1a7837",
    "Bicyclist Injury"   = "#7fbf7b",
    "Pedestrian Crashes" = "#d7191c",
    "Pedestrian Injury"  = "#f4a582"
  )
  
  panel_map <- c(
    "All Crashes"        = "Overall",
    "Any Injury"         = "Overall",
    "Vehicle Crashes"    = "Vehicle",
    "Driver Injury"      = "Vehicle",
    "Bike Crashes"       = "Bike",
    "Bicyclist Injury"   = "Bike",
    "Pedestrian Crashes" = "Pedestrian",
    "Pedestrian Injury"  = "Pedestrian"
  )
  
  # ── 1. Compute series ───────────────────────────────────────────────────────
  plot_data <- data |>
    select(date, all_of(series_levels)) |>
    pivot_longer(-date, names_to = "series", values_to = "n") |>
    arrange(series, date) |>
    group_by(series) |>
    mutate(value_smooth = rollmean(n, k = smoothing_factor, fill = NA, align = "right")) |>
    ungroup() |>
    mutate(
      series = factor(series, levels = series_levels),
      panel  = factor(panel_map[as.character(series)],
                      levels = c("Overall", "Vehicle", "Bike", "Pedestrian"))
    )
  
  # ── 2. Build plot ───────────────────────────────────────────────────────────
  p <- ggplot(plot_data, aes(x = date, y = value_smooth, color = series)) +
    geom_line(linewidth = 0.7, alpha = 0.8, na.rm = TRUE) +
    facet_wrap(~ panel, ncol = 1, scales = "free_y") +
    scale_color_manual(values = series_colors) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    labs(x = NULL, y = "Count", color = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position  = "bottom",
      strip.text       = element_text(face = "bold", size = 10),
      axis.text.x      = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    ) +
    guides(color = guide_legend(nrow = 2)) +
    ggtitle(title) +
    theme(legend.position="none")
  
  # ── 3. Add period shading ───────────────────────────────────────────────────
  p <- p +
    annotate("rect",
             xmin = as.Date(period_1$start), xmax = as.Date(period_1$end),
             ymin = -Inf, ymax = Inf,
             fill = "#2166ac", alpha = 0.08
    ) 
  
  p <- p +
    annotate("rect",
             xmin = as.Date(period_2$start), xmax = as.Date(period_2$end),
             ymin = -Inf, ymax = Inf,
             fill = "#d7191c", alpha = 0.08
    ) 
  
  # ── 4. Summary table: outcomes per month in each period ─────────────────────
  summarise_period <- function(p) {
    n_months <- interval(as.Date(p$start), as.Date(p$end)) %/% months(1) + 1
    
    plot_data |>
      filter(date >= as.Date(p$start), date <= as.Date(p$end)) |>
      group_by(series) |>
      summarise(total = sum(n, na.rm = TRUE), .groups = "drop") |>
      mutate(
        label        = p$label,
        n_months     = n_months,
        per_month    = total / n_months
      )
  }
  
  summary_data <- bind_rows(
    summarise_period(period_1),
    summarise_period(period_2)
  ) |>
    select(series, label, total, n_months, per_month) |>
    pivot_wider(
      names_from  = label,
      values_from = c(total, n_months, per_month),
      names_glue  = "{label}_{.value}"
    ) |>
    select(series, ends_with("_per_month")) |>
    mutate(
      pct_change = round((!!sym(paste0(period_2$label, "_per_month")) -
                            !!sym(paste0(period_1$label, "_per_month"))) /
                           !!sym(paste0(period_1$label, "_per_month")) * 100, 1),
      across(ends_with("_per_month"), round, digits = 2)
    )
  
  
  return(list(p, plot_data, summary_data))
}






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
