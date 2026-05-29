
# filter_grid_by_pbf --------------------------------------------------------------------------

filter_grid_by_pbf <- function(grid_path, pbf_paths, buffer = NULL) {
  
 requireNamespace("sf", quietly = T)
 requireNamespace("duckspatial", quietly = T)
 pop_unit <- regmatches(grid_path, regexpr("\\d{7}", grid_path))
 pbf_path <- grep(pop_unit, pbf_paths, value = T)
 
 pbf <- osmextract::oe_read(pbf_path) |>
   select(osm_id, highway, geometry) |>
   filter(highway != "path") |>
   st_transform(crs = 31983)
 grid <- arrow::open_dataset(grid_path) |>
   st_as_sf() |>
   select(h3_address, geometry) |>
   st_transform(crs = 31983)

 conn <- ddbs_create_conn()
 ddbs_write_table(conn, pbf, "pbf")
 ddbs_write_table(conn, grid, "grid")

 if(is.null(buffer)) {
   ddbs_filter("grid", "pbf", conn = conn, name = "grid_snap", overwrite = T)
 } else {
   ddbs_buffer("pbf", buffer, conn = conn, name = "pbf_buffer", overwrite = T)
   ddbs_union("pbf_buffer", conn = conn, name = "pbf_u", overwrite = T)
   ddbs_filter("grid", "pbf_u", conn = conn, name = "grid_snap", overwrite = T)
 }

 grid_snap <- ddbs_read_table("grid_snap", conn = conn)

 return(grid_snap)
 
}

# targets::tar_load(pbf_data)
# targets::tar_load(paths_list)
# 
# pbf_paths <- unlist(pbf_data)
# grid_paths <- unlist(paths_list)
# 
# # pbf_path <- pbf_paths[1]
# grid_path <- grid_paths[1]
# buffer <- 250
# 
# grid <- filter_grid_by_pbf(grid_path = grid_path, pbf_paths = pbf_paths, buffer = 250)
# 
# grids_filtered <- grid_paths[1:10] |> 
#   purrr::map(~filter_grid_by_pbf(grid_path = .x, pbf_paths = pbf_paths, buffer = 250))



# count_cells ---------------------------------------------------------------------------------

count_cells <- function(grid) {
  grid |> st_drop_geometry() |> pull(h3_address) |> n_distinct()
}

# cc <- purrr::map(grids_filtered, count_cells)



# get_batches_indices -------------------------------------------------------------------------

get_batches_indices <- function(cell_counts, n_batches) {
     # we have too many grids with which we want to route. as a result, we have to
     # run the targets in batches, otherwise the workflow gets too heavy and slow,
     # which makes it hard to run/add new targets and to visualize changes with
     # tar_visnetwork().
     # to decide which grid goes in which batches, we order them by the number of
     # non-empty cells and "balance" the batches with small and large grids

     indices_cuts <- cut(1:length(cell_counts), breaks = n_batches)
     indices <- split(1:length(cell_counts), indices_cuts)

     names(cell_counts) <- as.character(1:length(cell_counts))
     cell_counts <- cell_counts[order(cell_counts)]

     ordered_ids <- as.integer(names(cell_counts))

     batches_indices <- lapply(
       1:n_batches,
       function(b) {
         assigned_batch <- 1:length(cell_counts) %% n_batches
         assigned_current_group <- ordered_ids[assigned_batch == (b - 1)]
         assigned_current_group
       }
     )

     return(batches_indices)
   }

# cell_counts <- tar_read(n_cells)
# tar_load(n_batches)