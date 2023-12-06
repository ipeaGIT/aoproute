# pop_units <- tar_read(pop_units)
create_r5_dirs <- function(pop_units) {
  dir_names <- paste0(pop_units$code_pop_unit, "_", pop_units$treated_name)
  
  r5_dir <- "../../data/acesso_oport_v2/r5"
  
  dirs <- file.path(r5_dir, dir_names)
  
  for (dir in dirs) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  }
  
  return(dirs)
}

# pop_units <- tar_read(pop_units)
# r5_dirs <- tar_read(r5_dirs)
download_elevation_data <- function(pop_units, r5_dirs) {
  # was originally using {elevatr} to downoad the data, but kept stumbling upon
  # some C stack usage errors. decided to do it "manually".
  
  pop_units <- sf::st_transform(pop_units, 4326)
  
  # using the bounding box of each feature to get the tiles that cover them
  
  bboxes <- lapply(1:nrow(pop_units), function(i) sf::st_bbox(pop_units[i, ]))
  
  pop_units_tiles <- lapply(bboxes, tiles_from_bbox)
  
  unique_tiles <- unique(unlist(pop_units_tiles))
  tiles_url <- paste0(
    "https://e4ftl01.cr.usgs.gov/MEASURES/SRTMGL1.003/2000.02.11/",
    unique_tiles,
    ".SRTMGL1.hgt.zip"
  )
  
  tiles_dir <- "../../data/acesso_oport_v2/zipped_elevation_tiles"
  if (!dir.exists(tiles_dir)) dir.create(tiles_dir)
  output_files <- file.path(tiles_dir, paste0(unique_tiles, ".hgt.zip"))
  
  responses <- mapply(
    url = tiles_url,
    out = output_files,
    i = 1:length(tiles_url),
    function(url, out, i) {
      message(
        "Download tile ", unique_tiles[i],
        " (", i, "/", length(tiles_url), ")"
      )
      
      if (file.exists(out)) {
        message("  - Tile has been previously downloaded")
      } else {
        httr::GET(
          url,
          httr::authenticate(
            Sys.getenv("EARTHDATA_LOGIN"),
            Sys.getenv("EARTHDATA_PASS")
          ),
          httr::write_disk(out),
          httr::progress()
        )
      }
    }
  )
  
  future::plan(future.callr::callr, workers = getOption("TARGETS_N_CORES"))
  
  tmpdir <- tempdir()
  
  unzipped_tiles <- furrr::future_map_chr(
    output_files,
    function(f) utils::unzip(f, exdir = tmpdir),
    .progress = TRUE
  )
  
  # terra::rast() only creates a point to the file, but doesn't load the values
  # to RAM. this prevents us from using parallelization to process our data. so
  # we first wrap it and then unwrap it when actually applying the function
  
  tiles_rasters <- lapply(
    unzipped_tiles,
    function(t) terra::wrap(terra::rast(t))
  )
  names(tiles_rasters) <- unique_tiles
  
  # disabling the future globals' max size check because we have to pass
  # 'tiles_rasters', which is a very large object, as a global
  
  old_options <- options(future.globals.maxSize = Inf)
  on.exit(options(old_options), add = TRUE)
  
  pop_units_rasters <- furrr::future_map(
    pop_units_tiles,
    function(selected_tiles) {
      selected_rasters <- tiles_rasters[selected_tiles]
      selected_rasters <- lapply(selected_rasters, terra::unwrap)
      
      names(selected_rasters) <- NULL
      
      combined_raster <- do.call(
        terra::mosaic,
        args = append(selected_rasters, list(fun = "mean"))
      )
      
      terra::wrap(combined_raster)
    },
    .progress = TRUE,
    .options = furrr::furrr_options(seed = TRUE)
  )
  
  elevation_files <- furrr::future_pmap_chr(
    list(pop_units_rasters, r5_dirs),
    function(elev, r5_dir) {
      elev <- terra::unwrap(elev)
      elev_path <- file.path(r5_dir, "elevation.tif")
      
      terra::writeRaster(elev, elev_path, overwrite = TRUE)
      
      elev_path
    },
    .progress = TRUE,
    .options = furrr::furrr_options(seed = TRUE)
  )
  
  future::plan(future::sequential)
  
  
  
  
  
  rasters <- gsub("\\.zip", "", output)
  
  lista_rasters <- lapply(rasters, terra::rast)
  raster_combinado <- do.call(
    terra::mosaic,
    args = c(lista_rasters, fun = "mean")
  )
  raster_combinado <- terra::crop(raster_combinado, bbox)
  
  terra::writeRaster(raster_combinado, "data/topografia.tif", overwrite = TRUE)
  
  
  
  elevation_files <- furrr::future_map_chr(
    1:nrow(pop_units),
    function(i) {
      requireNamespace("sf", quietly = TRUE)
      
      pop_unit <- pop_units[i, ]
      
      elevation <- elevatr::get_elev_raster(
        pop_unit,
        z = 12,
        verbose = FALSE,
        serial = TRUE
      )
      elevation <- terra::rast(elevation)
      
      r5_dir <- r5_dirs[i]
      
      elevation_file <- file.path(r5_dir, "elevation.tif")
      terra::writeRaster(elevation, elevation_file, overwrite = TRUE)
      
      elevation_file
    },
    .options = furrr::furrr_options(seed = TRUE),
    .progress = getOption("TARGETS_SHOW_PROGRESS")
  )
  
  future::plan(future::sequential)
}

tiles_from_bbox <- function(bbox) {
  lons <- seq(floor(bbox$xmin), ceiling(bbox$xmax - 1), by = 1)
  lats <- seq(floor(bbox$ymin), ceiling(bbox$ymax - 1), by = 1)
  
  coords <- expand.grid(lat = lats, lon = lons)
  
  lat_part <- ifelse(
    lats < 0,
    paste0("S", formatC(coords$lat * -1, width = 2, flag = "0")),
    paste0("N", formatC(coords$lat, width = 2, flag = "0"))
  )
  
  lon_part <- paste0("W", formatC(coords$lon * -1, width = 3, flag = "0"))
  
  tiles <- paste0(lat_part, lon_part)
  
  return(tiles)
}
