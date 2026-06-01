
# setup ---------------------------------------------------------------------------------------

options(
  TARGETS_SHOW_PROGRESS = TRUE,
  TARGETS_N_CORES = 35,
  timeout = 600,
  java.parameters = "-Xmx50G"
)

suppressPackageStartupMessages({
  library(targets)
  library(crew)
  library(dplyr)
  library(duckspatial)
  library(geoarrow)
  library(ggplot2)
  library(gtfstools)
  library(sf)
})

tar_option_set(
  controller = crew_controller_local(workers = floor(.75*parallelly::freeCores()[1])),
  retrieval = "worker",
  storage = "worker",
  trust_timestamps = TRUE,
  workspace_on_error = TRUE 
  )

tar_source()



# targets list --------------------------------------------------------------------------------

list(
  ## parameters  ------------------------------------------------------------------------------
  tar_target(name = h3_resolutions, command = 7:9),
  tar_target(name = n_batches, command = 35),
  tar_target(
    name = pop_units_dataset,
    command = "../../data/acesso_oport_v3/pop_units.rds",
    format = "file"
  ),
  tar_target(
    name = brazil_pbf, 
    command = get_pbf(save_dir = "../../data-raw/osm"),
    format = "file"
  ),
  
  
  # 1_prep ------------------------------------------------------------------------------------
  tar_target(
    name = pop_units, 
    command = readRDS(pop_units_dataset) |> 
      mutate(label_pop_unit = paste(code_pop_unit, treated_name, sep = "_")),
    iteration = "group"
  ),
  tar_target(
    batches_by_pop_unit_area,
    get_batches_by_area(pop_units, n_batches),
    iteration = "list"
  ),
  tar_target(paths_list, get_grid_paths(pop_units, h3_resolutions)),
  # tar_target(
  #   batches,
  #   get_batches_indices(paths_list, n_batches),
  #   iteration = "list"
  # ),
  tar_target(
    routing_points,
    get_points(paths_list, batches),
    pattern = map(batches),
    retrieval = "worker",
    storage = "worker",
    iteration = "list"
  ),
  tar_target(filtered_brazil_pbf, filter_pbf(brazil_pbf), format = "file"),
  
  
  # 2_r5r_file_structure ----------------------------------------------------------------------
  
  tar_target(r5_dirs, create_r5_dirs(pop_units)),
  tar_target(
    elevation_data,
    download_elevation_data(pop_units, r5_dirs, batches_by_pop_unit_area),
    format = "file",
    pattern = map(batches_by_pop_unit_area),
    retrieval = "worker",
    storage = "worker",
    iteration = "list"
  ),
  tar_target(
    pbf_data,
    crop_pbf_data(filtered_brazil_pbf, pop_units, batches_by_pop_unit_area),
    format = "file",
    pattern = map(batches_by_pop_unit_area),
    retrieval = "worker",
    storage = "worker",
    iteration = "list"
  ),
  
  
  # 2b_bypass_grid ----------------------------------------------------------------------------
    
  tar_target(
    name = grid_filtered,
    command = filter_grid_by_pbf(grid_path = paths_list, pbf_paths = unlist(pbf_data), buffer = 250),
    pattern = map(paths_list)
  ),
  tar_target(
    name = n_cells, 
    command = count_cells(grid_filtered),
    pattern = map(grid_filtered)
  ),
  tar_target(
    name = batches,
    command = get_batches_indices(cell_counts = n_cells, n_batches = n_batches),
    iteration = "list"
  ),
  
  
  # 2c_gtfs -----------------------------------------------------------------------------------

  tar_target(
    name = feeds_meta,
    command = arrow::read_parquet("../../data/acesso_oport_v3/feeds_metadata.parquet") |> 
      filter(is.na(action) | !stringr::str_detect(action, "fallback|discard")),
    format = "parquet"
  ),
  tar_target(
    name = transit_areas,
    command = distinct(feeds_meta, code_pop_unit) |> 
      inner_join(pop_units) |> 
      select(label_pop_unit, code_pop_unit) |> 
      tibble::deframe()
  ),
  tar_target(
    name = feed_paths,
    command = import_feeds(feeds_meta = feeds_meta, pop_unit = transit_areas, 
                           areas_sf = pop_units, overwrite = F),
    pattern = map(transit_areas),
    format = "file"
  ),
  tar_target(
    name = feeds_adjusted,
    command = adjust_feeds(code_unit = transit_areas, feeds_meta = feeds_meta, 
                           feed_paths = feed_paths),
    pattern = map(transit_areas),
    format = "file"
  ),
  
  
  # 3_routing ---------------------------------------------------------------------------------
  tar_target(
    r5_network,
    build_r5_network(elevation_data, pbf_data, feeds_adjusted),
    format = "file",
    pattern = map(elevation_data, pbf_data),
    retrieval = "worker",
    storage = "worker",
    iteration = "list",
    error = "null"
  ),
  tar_target(
    walk_matrix,
    calculate_ttm(r5_network, routing_points, mode = "WALK"),
    format = "file",
    pattern = map(routing_points)
  ),
  tar_target(
    bike_matrix,
    calculate_ttm(r5_network, routing_points, mode = "BICYCLE"),
    format = "file",
    pattern = map(routing_points)
  )
)
