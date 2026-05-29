
# get_batches_by_area -------------------------------------------------------------------------

# pop_units <- tar_read(pop_units)
# n_batches <- tar_read(n_batches)
get_batches_by_area <- function(pop_units, n_batches) {
  # order pop_units by their area and balance batches with small and large units
  
  areas <- as.numeric(sf::st_area(pop_units))
  names(areas) <- 1:nrow(pop_units)
  
  areas <- areas[order(areas)]
  
  ordered_units <- as.integer(names(areas))
  
  batches_indices <- lapply(
    1:n_batches,
    function(b) {
      assigned_batch <- 1:length(areas) %% n_batches
      assigned_current_group <- ordered_units[assigned_batch == (b - 1)]
      assigned_current_group
    }
  )
  
  return(batches_indices)
}



# get_grid_paths -----------------------------------------------------------------------------

# pop_units <- tar_read(pop_units)
# h3_resolutions <- tar_read(h3_resolutions)
get_grid_paths <- function(pop_units, h3_resolutions, census_year = 2022,
                           grids_dir = "../../data/acesso_oport_v3/hex_grids_with_data") {
  resolutions_dir <- file.path(grids_dir, paste0("res_", h3_resolutions))
  census_dir <- file.path(resolutions_dir, census_year)
  
  combinations <- expand.grid(
    basename = paste0(pop_units$label_pop_unit, ".parquet"),
    dir = census_dir,
    stringsAsFactors = FALSE
  )
  
  filepaths <- file.path(combinations$dir, combinations$basename)
  
  return(filepaths)
}



# # get_batches_indices -------------------------------------------------------------------------
# 
# # grid_paths <- tar_read(paths_list)
# # n_batches <- tar_read(n_batches)
# get_batches_indices <- function(grid_paths, n_batches) {
#   # we have too many grids with which we want to route. as a result, we have to
#   # run the targets in batches, otherwise the workflow gets too heavy and slow,
#   # which makes it hard to run/add new targets and to visualize changes with
#   # tar_visnetwork().
#   # to decide which grid goes in which batches, we order them by the number of
#   # non-empty cells and "balance" the batches with small and large grids
#   
#   indices_cuts <- cut(1:length(grid_paths), breaks = n_batches)
#   indices <- split(1:length(grid_paths), indices_cuts)
#   
#   future::plan(future.callr::callr, workers = n_batches)
#   
#   non_empty_cells <- furrr::future_map(
#     indices,
#     non_empty_cells_count,
#     grid_paths = grid_paths,
#     .options = furrr::furrr_options(seed = TRUE)
#   )
#   
#   future::plan(future::sequential)
#   
#   non_empty_cells <- unlist(non_empty_cells)
#   names(non_empty_cells) <- as.character(1:length(grid_paths))
#   non_empty_cells <- non_empty_cells[order(non_empty_cells)]
#   
#   ordered_ids <- as.integer(names(non_empty_cells))
#   
#   batches_indices <- lapply(
#     1:n_batches,
#     function(b) {
#       assigned_batch <- 1:length(non_empty_cells) %% n_batches
#       assigned_current_group <- ordered_ids[assigned_batch == (b - 1)]
#       assigned_current_group
#     }
#   )
#   
#   return(batches_indices)
# }
# 
# 
# 
# # non_empty_cells_count -----------------------------------------------------------------------
# 
# non_empty_cells_count <- function(is, grid_paths) {
#   requireNamespace("sf", quietly = TRUE)
#   
#   vapply(
#     is,
#     FUN.VALUE = integer(1),
#     FUN = function(i) {
#       grid <- readRDS(grid_paths[i])
#       routing_grid <- dplyr::filter(grid, pop_total > 0)
#       nrow(routing_grid)
#     }
#   )
# }
# 
# 
# 
# # get_points ----------------------------------------------------------------------------------
# 
# # paths <- tar_read(paths_list)
# # batch_indices <- tar_read(batches)[[1]]
# get_points <- function(paths, batch_indices) {
#   grid_paths <- paths[batch_indices]
#   
#   routing_points <- lapply(grid_paths, routing_points_from_path)
#   
#   list_names <- stringr::str_extract(grid_paths, "res_\\d.*")
#   list_names <- sub("\\.rds$", "", list_names)
#   names(routing_points) <- list_names
#   
#   return(routing_points)
# }
# 
# 
# 
# # routing_points_from_path --------------------------------------------------------------------
# 
# # path <- grid_paths[1]
# routing_points_from_path <- function(path) {
#   grid <- readRDS(path)
#   
#   routing_grid <- dplyr::filter(grid, pop_total > 0)
#   
#   # suppressed warning:
#   # - st_centroid assumes attributes are constant over geometries
#   
#   routing_points <- suppressWarnings(sf::st_centroid(routing_grid))
#   routing_points <- dplyr::select(routing_points, id = h3_address, geometry)
#   
#   return(routing_points)
# }



# pbf -----------------------------------------------------------------------------------------

# save_dir = "../../data-raw/osm"; time_stamp = TRUE; overwrite = FALSE; ts_level = 'year'

get_pbf <- function(save_dir, time_stamp = TRUE, overwrite = FALSE, ts_level = c('year', 'month')) {
  
  ts_level <- rlang::arg_match(ts_level)
  if(!dir.exists(save_dir)) dir.create(save_dir)  
  
  if(time_stamp) {
    ts <- Sys.Date() |> 
      stringr::str_remove_all("-") 
    
    ts <- if(ts_level == "year") {
      stringr::str_sub(ts, 1, 4)
    } else if(ts_level == "month") {
      stringr::str_sub(ts, 1, 6)
    }
    ts <- paste0("_", ts)
  } else {
    ts <- ""
  }
  
  save_path <- file.path(save_dir, paste0("brazil", ts, ".osm.pbf"))
  
  if(!overwrite) {
    if(file.exists(save_path)) {
      return(save_path)
    }
  }
  
  temp_path <- osmextract::oe_download(
    file_url = osmextract::oe_match("brazil", "openstreetmap_fr")[[1]],
    provider = "openstreetmap_fr",
    download_directory = save_dir
  )
  
  file.rename(temp_path, save_path)
  
  return(save_path)
}

# pbf_path <- targets::tar_read(brazil_pbf)
filter_pbf <- function(pbf_path) {
  filtered_path <- stringr::str_replace(pbf_path, ".osm.pbf$", "_filtered.osm.pbf")
  
  rosmium::tags_filter(
    pbf_path,
    filters = paste(
      "w/highway w/park_ride w/public_transport=platform w/railway=platform",
      "r/type=restriction"
    ),
    output_path = filtered_path,
    overwrite = TRUE
  )
}