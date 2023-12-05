# grids_paths <- tar_read(paths_list)
# n_batches <- tar_read(n_batches)
get_batches_indices <- function(grids_paths, n_batches) {
  # we have too many grids with which we want to route. as a result, we have to
  # run the targets in batches, otherwise the workflow gets too heavy and slow,
  # which makes it hard to run/add new targets and to visualize changes with
  # tar_visnetwork().
  # to decide which grid goes in which batches, we order them by the number of
  # non-empty cells and "balance" the batches with small and large grids
  
  indices_cuts <- cut(1:length(grids_paths), breaks = n_batches)
  indices <- split(1:length(grids_paths), indices_cuts)
  
  future::plan(future.callr::callr, workers = n_batches)
  
  non_empty_cells <- furrr::future_map(
    indices,
    non_empty_cells_count,
    grids_paths = grids_paths,
    .options = furrr::furrr_options(seed = TRUE)
  )
  
  future::plan(future::sequential)
  
  non_empty_cells <- unlist(non_empty_cells)
  names(non_empty_cells) <- as.character(1:length(grids_paths))
  non_empty_cells <- non_empty_cells[order(non_empty_cells)]
  
  ordered_ids <- as.integer(names(non_empty_cells))
  
  batches_indices <- lapply(
    1:n_batches,
    function(b) {
      assigned_batch <- 1:length(non_empty_cells) %% n_batches
      assigned_current_group <- ordered_ids[assigned_batch == (b - 1)]
      assigned_current_group
    }
  )
  
  return(batches_indices)
}

non_empty_cells_count <- function(is, grids_paths) {
  requireNamespace("sf", quietly = TRUE)
  
  vapply(
    is,
    FUN.VALUE = integer(1),
    FUN = function(i) {
      grid <- readRDS(grids_paths[i])
      routing_grid <- dplyr::filter(grid, pop_total > 0)
      nrow(routing_grid)
    }
  )
}

# pop_units <- tar_read(pop_units)
# h3_resolutions <- tar_read(h3_resolutions)
get_grids_paths <- function(pop_units, h3_resolutions) {
  grids_dir <- "../../data/acesso_oport_v2/hex_grids_with_data"
  resolutions_dir <- file.path(grids_dir, paste0("res_", h3_resolutions))
  census_dir <- file.path(resolutions_dir, "2010")
  
  basenames <- paste0(
    pop_units$code_pop_unit,
    "_",
    pop_units$treated_name,
    ".rds"
  )
  
  combinations <- expand.grid(
    basename = basenames,
    dir = census_dir,
    stringsAsFactors = FALSE
  )
  
  filepaths <- file.path(combinations$dir, combinations$basename)
  
  return(filepaths)
}

# paths <- tar_read(paths_list)
# batch_indices <- tar_read(batches)[[1]]
get_points <- function(paths, batch_indices) {
  grids_paths <- paths[batch_indices]
  
  routing_points <- lapply(grids_paths, routing_points_from_path)
  
  list_names <- stringr::str_extract(grids_paths, "res_\\d.*")
  list_names <- sub("\\.rds$", "", list_names)
  names(routing_points) <- list_names
  
  return(routing_points)
}

# path <- grids_paths[1]
routing_points_from_path <- function(path) {
  grid <- readRDS(path)
  
  routing_grid <- dplyr::filter(grid, pop_total > 0)
  
  # suppressed warning:
  # - st_centroid assumes attributes are constant over geometries
  
  routing_points <- suppressWarnings(sf::st_centroid(routing_grid))
  routing_points <- dplyr::select(routing_points, id = h3_address, geometry)
  
  return(routing_points)
}
