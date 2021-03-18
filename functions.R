# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Functions for for HNC placements dashboard
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Placed gauge

placed_gauge = function(df_func){
  x = df_func %>%
  filter(placement_req == "yes")

  round(100 * (1 - (sum(x$not_placed, na.rm = T) / sum(x$students))), 1) %>%
    gauge(min = 0, max = 100, symbol = '%', gaugeSectors(
      success = c(100, 80), warning = c(25, 79), danger = c(0, 24)
  ))
}


# -------------------------------------------------------------------------
# Placements map

placements_map = function(df_func){
  x = df_func %>%
    filter(placement_req == "yes") %>%
    group_by(centre_name, easting, northing) %>%
    summarise(students = sum(students),
              not_placed = sum(not_placed),
              not_placed_log = sum(not_placed_log)) %>%
    ungroup()

  pal <- colorNumeric(
    palette = "YlGnBu",
    domain = x$not_placed
  )

  y = x %>%
    mutate(tooltip = glue('<b>College:</b> {centre_name} <br><b>Placement students: {students} <br><b>Placements needed:</b> {not_placed}'))

  x %>%
    drop_na() %>%
    st_as_sf(coords = c("easting", "northing"),
             crs = 27700) %>%
    st_transform(CRS("+init=epsg:4326")) %>%
    leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng = -3, lat = 58, zoom = 5) %>%
    addCircleMarkers(radius = x$not_placed_log * 5,
                     fillColor = ~pal(not_placed),
                     color = "black",
                     stroke = T, fillOpacity = 0.8,
                     popup = y$tooltip) %>%
    addLegend("bottomleft", pal = pal, values = ~not_placed,
              title = "Placements needed",
              opacity = 0.8)
}


# -------------------------------------------------------------------------
# Proportion plot

proportion_plot = function(df_func){
  df_func %>%
    filter(placement_req == "yes") %>%
    group_by(centre_name) %>%
    summarise(students = sum(students),
              not_placed = sum(not_placed)) %>%
    ungroup() %>%
    mutate(centre_name = fct_reorder(centre_name, not_placed),
           Placed = students - not_placed) %>%
    select(-students, College = centre_name) %>%
    pivot_longer(!College, names_to = "Status", values_to = "Students") %>%
    mutate(Status = replace(Status, Status == "not_placed",
                            "Placements needed")) %>%
    ggplot(aes(College, Students,
               fill = Status)) +
    geom_col() +
    scale_fill_brewer(type = "qual", palette = "Dark2") +
    coord_flip() +
    labs(title = "",
         x = "",
         y = "Students",
         fill = "") +
    theme_bw() +
    theme(text = element_text(size = 14))
}

