
##' important: this function branches over pop_units because the same feed can have different calendar
##' dates in different pop_units
import_feeds <- function(feeds_meta, pop_unit, r5_dir = "../../data/acesso_oport_v3/r5",
                         overwrite = TRUE, sync_dates = TRUE, 
                         spatial_filter = TRUE, areas_sf = NULL) {
  
  feeds_meta <- feeds_meta |> 
    filter(code_pop_unit == pop_unit)
  
  feeds_meta <- feeds_meta |> 
    group_by(file_name) |> 
    group_split()
  
  # x <- feeds_meta[[6]] # debug
  
  paths <- purrr::map(
    feeds_meta, function(x) {
      
      dest_path <- file.path(r5_dir, names(pop_unit), x$file_name)
      file_exists <- file.exists(dest_path)
      dates_match <- (x$end_old == x$end_new) & (x$start_old == x$start_new)
      
      if(!overwrite & file_exists) {
        return(dest_path)
      }
      
      if((!sync_dates | dates_match) & !spatial_filter) {
        file.copy(x$feed_path, dest_path, overwrite = overwrite)
      } else {
        gtfs <- gtfsio::import_gtfs(x$feed_path)
        
        if(sync_dates & !dates_match) {
          gtfs$calendar <- gtfs$calendar |> 
            mutate(start_date = x$start_new, end_date = x$end_new)
        }
        
        areas_sf <- areas_sf |> 
          filter(code_pop_unit == pop_unit) |> 
          st_transform(crs = 4326)
        
        # conn <- ddbs_create_conn()
        # ddbs_write_table(conn, areas_sf, "areas")
        # gtfs$shapes gtfs$stops
        # ddbs_write_table(conn, gtfs$shapes, "areas")
        gtfs <- gtfstools::filter_by_spatial_extent(gtfs, areas_sf)
        
        gtfstools::write_gtfs(gtfs, dest_path)
      }
      
      return(dest_path)
    }
  ) |> 
    unlist()
  
  return(paths)
}

# tar_load(feeds_meta)
# tar_load(transit_areas)
# # tar_workspace(feed_paths_b869494c4ddf9a40)
# pop_unit <- transit_areas[13]
# areas_sf <- tar_read(pop_units)
# r5_dir <- "../../data/acesso_oport_v3/r5"
# overwrite = TRUE; sync_dates = TRUE; spatial_filter = TRUE



# specific changes ----------------------------------------------------------------------------

transplant_route_type <- function(from_path, to_path, route_type) {
  from_feed <- gtfsio::import_gtfs(from_path) |>
      filter_by_route_type(route_type)
  
  # to <- to[1] # debug
  purrr::map(
    to_path, function(x) {
      new_feed <- gtfsio::import_gtfs(x)
      new_feed <- merge_gtfs(from_feed, new_feed)
      write_gtfs(new_feed, x)
    }
  )
  
  return(to_path)
} 

## ssz
adjust_santos <- function(code_unit, feeds_meta, feed_paths) {
  stopifnot(code_unit == 3548500)
  
  feeds_meta <- feeds_meta |>
    filter(code_pop_unit == code_unit)
  
  feed_paths <- feed_paths[grep(code_unit, feed_paths)]
  
  from_name <- feeds_meta |>
    filter(feed_alias == "281") |>
    pull(file_name)
  
  from_path <- feed_paths[basename(feed_paths) == from_name]
  
  to_name <- feeds_meta |>
    filter(action == "pegar vlt do 281") |>
    pull(file_name)
  to_path <- feed_paths[basename(feed_paths) %in% to_name]
  
  transplant_route_type(from_path, to_path, 0)
  
  return(to_path)
}

# code_unit <- 3548500
# tar_load(feeds_meta)
# tar_load(feed_paths)


## nat
adjust_natal <- function(code_unit, feeds_meta, feed_paths) {
  stopifnot(code_unit == 2408102)
  
  feeds_meta <- feeds_meta |>
    filter(code_pop_unit == code_unit)
  
  feed_paths <- feed_paths[grep(code_unit, feed_paths)]
  
  from_name <- feeds_meta |>
    filter(feed_alias == "502") |>
    pull(file_name)
  
  from_path <- feed_paths[basename(feed_paths) == from_name]
  
  to_name <- feeds_meta |>
    filter(n_rail == 0) |>
    pull(file_name)
  to_path <- feed_paths[basename(feed_paths) %in% to_name]
  
  transplant_route_type(from_path, to_path, 0)
  
  return(to_path)
}

# code_unit <- 2408102
# tar_load(feeds_meta)
# tar_load(feed_paths)



# general adjust_feeds ------------------------------------------------------------------------

adjust_feeds <- function(code_unit, feeds_meta, feed_paths) {
  if(code_unit == 3548500) adjust_santos(code_unit, feeds_meta, feed_paths)
  if(code_unit == 2408102) adjust_natal(code_unit, feeds_meta, feed_paths)
  return(feed_paths)
}
