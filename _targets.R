options(
  TARGETS_SHOW_PROGRESS = TRUE,
  TARGETS_N_CORES = 35
)

suppressPackageStartupMessages({
  library(targets)
  library(ggplot2)
  library(sf)
})

source("R/1_prep.R", encoding = "UTF-8")
source("R/2_r5r_file_structure.R", encoding = "UTF-8")

if (!interactive()) future::plan(future.callr::callr)

tar_option_set(workspace_on_error = TRUE)

list(
  tar_target(h3_resolutions, 7:9),
  tar_target(n_batches, 35),
  tar_target(
    pop_units_dataset,
    "../../data/acesso_oport_v2/pop_units.rds",
    format = "file"
  ),
  
  # 1_prep
  tar_target(pop_units, readRDS(pop_units_dataset), iteration = "group"),
  tar_target(paths_list, get_grids_paths(pop_units, h3_resolutions)),
  tar_target(
    batches,
    get_batches_indices(paths_list, n_batches),
    iteration = "list"
  ),
  tar_target(
    routing_points,
    get_points(paths_list, batches),
    pattern = map(batches),
    retrieval = "worker",
    storage = "worker",
    iteration = "list"
  ),
  
  # 2_r5r_file_structure
  tar_target(r5_dirs, create_r5_dirs(pop_units)),
  tar_target(elevation_data, download_elevation_data(pop_units, r5_dirs))
)