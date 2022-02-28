
snap_gps_traces <- function(network,
                          gps,
                          match_method = "fmm",
                          delta = 3000,
                          candidates = 8,
                          radius = 300,
                          gps_error = 50,
                          gps_point = TRUE,
                          use_omp = FALSE,
                          ubodt_config = NULL,
                          match_config = NULL) {
  # transform data
  network <- .transform_network(network)
  gps <- .transform_gps(gps)

  # create temp directory and export transformed data
  tmp_dir <- dir_create(file_temp())

  write_delim(gps, path(tmp_dir, "trips.csv"), delim = ";")
  st_write(network, path(tmp_dir, "edges.shp"))

  # define I/O files
  network_file <- path(tmp_dir, "edges.shp")
  gps_file <- path(tmp_dir, "trips.csv")
  mm_out_file <- path(tmp_dir, "mr.txt")

  # perform map matching
  if (match_method == "fmm") {
    # generate ubodt file
    ubodt_config <- c(delta = delta, ubodt_config)
    ubodt_out_file <- path(tmp_dir, "ubodt.txt")
    ubodt_config_xml <- .define_ubodt_config(
      network_file, ubodt_out_file, use_omp = use_omp, !!!ubodt_config
    )

    write_xml(ubodt_config_xml, path(tmp_dir, "ubodt-config.xml"))
    system2("ubodt_gen", path(tmp_dir, "ubodt-config.xml"))

    # run fmm tool
    fmm_config <- c(k = candidates, r = radius, e = gps_error, match_config)
    fmm_config_xml <- .define_map_match_config(
      network_file, gps_file, mm_out_file,
      match_method = match_method,
      ubodt_file = ubodt_out_file,
      gps_point = gps_point,
      use_omp = use_omp,
      !!!fmm_config
    )

    write_xml(fmm_config_xml, path(tmp_dir, "fmm-config.xml"))
    system2("fmm", path(tmp_dir, "fmm-config.xml"))

  } else if (match_method == "stmatch") {
    # run stmatch tool directly (no need to compute ubodt file)
    stmatch_config <- c(k = candidates, r = radius, e = gps_error, match_config)
    stmatch_config_xml <- .define_map_match_config(
      network_file, gps_file, mm_out_file,
      match_method = match_method,
      gps_point = gps_point,
      use_omp = use_omp,
      stmatch_config
    )

    write_xml(stmatch_config_xml, path(tmp_dir, "stmatch-config.xml"))
    system2("stmatch", path(tmp_dir, "stmatch-config.xml"))
  }

  # import matched result
  read_delim(path(tmp_dir, "mr.txt"), ";", col_types = cols(.default = "c"))
}

.transform_network <- function(network, output = "network") {
  # convert roads to a spatial network
  network <- network %>%
    st_cast("LINESTRING") %>%
    as_sfnetwork() %>%
    convert(to_spatial_subdivision)

  # original edges
  edges_orig <- st_as_sf(network, "edges") %>%
    rename(source = from, target = to) %>%
    relocate(road_id) %>%
    select(-.tidygraph_edge_index)

  # original edges with reverse directions
  edges_reversed <- edges_orig %>%
    st_reverse() %>%
    relocate(target, .before = source) %>%
    rename(source = target, target = source)

  # bidirectional edges
  edges_rbind <- bind_rows(edges_orig, edges_reversed) %>%
    arrange(road_id, source) %>%
    mutate(id = row_number(), .before = 1)

  # return output
  if (output == "network") {
    return(select(edges_rbind, -road_id))
  } else if (output == "join_key") {
    return(select(edges_rbind, id, road_id) %>% st_drop_geometry())
  } else {
    list(
      network = select(edges_rbind, -road_id),
      join_key = select(edges_rbind, id, road_id) %>% st_drop_geometry()
    )
  }
}

.transform_gps <- function(gps) {
  gps %>%
    st_coordinates() %>%
    as_tibble() %>%
    mutate(id = 1, .before = 1) %>%
    rename(x = X, y = Y)
}

.define_ubodt_config <- function(network_file, output_file, use_omp = FALSE, ...) {
  # default ubodt argument values
  ubodt_args <- list(
    network = network_file, output = output_file, # I/O
    network_id = "id", source = "source", target = "target", # network header
    delta = 3000, log_level = 2
  )

  # allow users to change optional ubodt argument values
  ubodt_args <- ubodt_args %>% list_modify(...)

  # define ubodt config
  ubodt_config <- list(
    config = list(
      input = list(
        network = list(
          file = list(ubodt_args$network),
          id = list(ubodt_args$network_id),
          source = list(ubodt_args$source),
          target = list(ubodt_args$target)
        )
      ),
      parameters = list(
        delta = list(ubodt_args$delta)
      ),
      output = list(
        file = list(ubodt_args$output)
      ),
      other = list(
        log_level = list(ubodt_args$log_level)
      )
    )
  )

  # run in multiple threads for speed?
  if(isTRUE(use_omp)) {
    ubodt_config$config$other$use_omp <- list()
  }

  # return ubodt config
  ubodt_config %>%
    as_xml_document()
}

.define_map_match_config <- function(network_file,
                                gps_file,
                                output_file,
                                match_method = "fmm",
                                ubodt_file = NULL,
                                gps_point = TRUE,
                                use_omp = FALSE,
                                ...) {
  # default map matching (mm) argument values
  mm_args <- list(
    network = network_file, gps = gps_file, # input
    output = output_file, output_fields = "all", # output
    network_id = "id", source = "source", target = "target", # network header
    gps_id = "id", gps_x = "x", gps_y = "y", # gps header
    gps_timestamp = "timestamp", # gps header
    k = 8, r = 300, e = 50, # three key arguments
    log_level = 2, step = 100
  )

  # allow users to change optional mm argument values
  mm_args <- mm_args %>% list_modify(...)

  # define mm config
  mm_config <- list(
    config = list(
      input = list(
        network = list(
          file = list(mm_args$network),
          id = list(mm_args$network_id),
          source = list(mm_args$source),
          target = list(mm_args$target)
        ),
        gps = list(
          file = list(mm_args$gps),
          id = list(mm_args$gps_id),
          x = list(mm_args$gps_x),
          y = list(mm_args$gps_y),
          timestamp = list(mm_args$gps_timestamp)
        )
      ),
      parameters = list(
        k = list(mm_args$k),
        r = list(mm_args$r),
        gps_error = list(mm_args$e)
      ),
      output = list(
        file = list(mm_args$output),
        fields = list(
          all = list()
        )
      ),
      other = list(
        log_level = list(mm_args$log_level),
        step = list(mm_args$step)
      )
    )
  )

  # only fmm method needs a ubodt file
  if (match_method == "fmm") {
    mm_config$config$input$ubodt$file <- list(ubodt_file)
  }

  # one gps trajectory point per row?
  if(isTRUE(gps_point)) {
    mm_config$config$input$gps$gps_point <- list()
  }

  # run in multiple threads for speed?
  if(isTRUE(use_omp)) {
    mm_config$config$other$use_omp <- list()
  }

  # return mm config
  mm_config %>%
    as_xml_document()
}

# Visualization -----------------------------------------------------------
viz_snap_output_geom <- function(gps, snap_output) {
  mapview(st_geometry(gps)) +
    mapview(
      st_as_sf(snap_output, wkt = "mgeom", crs = 26915) %>% st_geometry(),
      layer.name = "mgeom"
    ) +
    mapview(
      st_as_sf(snap_output, wkt = "pgeom", crs = 26915) %>%
        st_cast("POINT") %>%
        st_geometry(),
      layer.name = "pgeom_points"
    ) +
    mapview(
      st_as_sf(snap_output, wkt = "pgeom", crs = 26915) %>% st_geometry(),
      layer.name = "pgeom_path"
    )
}

.viz_transformed_network <- function(network) {
  transformed_network <- .transform_network(network) %>%
    as_sfnetwork()

  nodes <- transformed_network %>% st_as_sf("nodes")
  edges <- transformed_network %>% st_as_sf("edges")

  sync(
    mapview(network),
    mapview(list(nodes, edges))
  )
}

viz_snap_output_num <- function(snap_output) {
  num_fields <- c("error", "length", "offset", "spdist", "ep", "tp") %>%
    set_names(
      c(
        "GPS error (meter)", "Edge length (meter)", "Offset (meter)",
        "SP length (meter)", "Emission probability", "Transit probability"
      )
    )

  init_y <- snap_output %>%
    pluck("error") %>%
    str_split(",") %>%
    simplify() %>%
    as.numeric()
  init_x <- seq(1, length(init_y))

  plot_ly(
    x = init_x,
    y = init_y,
    mode = "lines+markers"
  ) %>%
    layout(
      xaxis = list(title = list(text = "Point index")),
      yaxis = list(title = list(text = names(num_fields[1]))),
      updatemenus = list(
        list(
          buttons = .define_buttons(snap_output, num_fields),
          showactive = TRUE,
          x = 0.11,
          xanchor = "left",
          y = 1.1,
          yanchor = "top",
          pad = list(l = 0)
        )
      ),
      annotations = list(
        list(
          text = "Feature:",
          showarrow = FALSE,
          x = 0,
          y = 1.08,
          yref = "paper",
          align = "left"
        )
      )
    )
}

.define_buttons <- function(snap_output, num_fields) {
  map2(
    unname(num_fields),
    names(num_fields),
    ~ list(
      method = "update",
      args = list(
        list(
          y = list(
            pluck(snap_output, .x) %>%
              str_split(",") %>%
              simplify() %>%
              as.numeric()
          )
        ),
        list(yaxis = list(title = list(text = .y)))
      ),
      label = .y
    )
  )
}
