# r5_network <- tar_read(r5_network)
# routing_points <- tar_read(routing_points, branches = 1)[[1]]
# mode <- "WALK"
calculate_ttm <- function(r5_network, routing_points, mode = "WALK") {
  # we should not even need to have r5_network as a dependency, since we can
  # figure out the urban population for which we are calculating the travel
  # times using routing_points' names. however, we want to recalculate the
  # matrices every time the r5 networks change, so we use them as a dependency
  # anyway. the downside of the current approach is that we need to recalculate
  # the matrices for all urban pops even if a single network alone has been
  # changed. but networks are quite rarely changed by themselves, and any
  # changes to them to be a result of systematic changes in the upstream
  # targets, which would require us to recalculate the matrices anyway. so I
  # don't think the current approach is that bad
  
  all_networks <- unlist(r5_network)
  
  n_units <- length(routing_points)
  
  paths <- vapply(
    1:n_units,
    FUN.VALUE = character(1),
    FUN = function(i) {
      points <- routing_points[[i]]
      points <- sf::st_transform(points, 4326)
      
      infos <- names(routing_points[i])
      pop_unit <- basename(infos)
      year <- basename(fs::path_dir(infos))
      res <- fs::path_dir(fs::path_dir(infos))
      
      network_path <- all_networks[grepl(pop_unit, all_networks)]
      
      r5r_core <- r5r::setup_r5(fs::path_dir(network_path))
      
      # using capture.output() to suppress R5 errors related to transit layer
      # being loaded but timezone unknown. that happens because our networks
      # don't contain a GTFS
      
      capture.output(
        ttm <- r5r::travel_time_matrix(
          r5r_core,
          origins = points,
          destinations = points,
          mode = mode,
          departure_datetime = as.POSIXct(
            "28-12-2023 14:00:00",
            format = "%d-%m-%Y %H:%M:%S"
          ),
          time_window = 1,
          max_walk_time = Inf,
          max_bike_time = Inf,
          max_car_time = Inf,
          max_trip_duration = 120,
          walk_speed = 3.6,
          bike_speed = 12,
          max_lts = 2,
          draws_per_minute = 1,
          n_threads = getOption("TARGETS_N_CORES"),
        ),
        type = "message",
        file = tempfile()
      )
      
      suppressMessages(r5r::stop_r5(r5r_core))
      
      ttm_dir <- file.path(
        "../../data/acesso_oport_v2/travel_time_matrix",
        res,
        year,
        tolower(mode)
      )
      if (!dir.exists(ttm_dir)) dir.create(ttm_dir, recursive = TRUE)
      
      ttm_path <- file.path(ttm_dir, paste0(pop_unit, ".rds"))
      
      saveRDS(ttm, ttm_path)
      
      ttm_path
    }
  )
  
  return(paths)
}