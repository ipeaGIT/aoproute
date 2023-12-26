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
  
  # some of the tiles fall entirely on the ocean, so they are not included in
  # the usgs database. in these cases, following the constructed url returns a
  # 404 error, which ultimately results in 1kb invalid .zip files being
  # downloaded. in this case, utils::unzip() returns NULL and throws a warning,
  # which we ignore.
  
  tmpdir <- tempdir()
  
  unzipped_tiles <- furrr::future_map(
    output_files,
    function(f) suppressWarnings(utils::unzip(f, exdir = tmpdir)),
    .progress = TRUE
  )
  names(unzipped_tiles) <- unique_tiles
  
  elevation_files <- furrr::future_pmap_chr(
    list(
      selected_tiles = pop_units_tiles,
      r5_dir = r5_dirs,
      bbox = bboxes,
      i = 1:length(pop_units_tiles)
    ),
    function(selected_tiles, r5_dir, bbox, i) {
      selected_files <- unzipped_tiles[selected_tiles]
      
      selected_rasters <- lapply(
        selected_files,
        function(f) tryCatch(terra::rast(f), error = function(cnd) NULL)
      )
      names(selected_rasters) <- NULL
      
      # removing NULL elements from selected_rasters because some of the
      # elements may have been created from the missing tiles, which leads to an
      # error in terra::mosaic()
      
      selected_rasters <- Filter(Negate(is.null), selected_rasters)
      
      if (length(selected_rasters) > 1) {
        combined_raster <- do.call(
          terra::mosaic,
          args = append(selected_rasters, list(fun = "mean"))
        )
      } else {
        combined_raster <- selected_rasters[[1]]
      }
      
      combined_raster <- terra::crop(combined_raster, bbox)
      
      elev_path <- file.path(r5_dir, "elevation.tif")
      terra::writeRaster(combined_raster, elev_path, overwrite = TRUE)
      
      elev_path
    },
    .progress = TRUE,
    .options = furrr::furrr_options(seed = TRUE)
  )
  
  future::plan(future::sequential)
  
  return(elevation_files)
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

# filtered_brazil_pbf <- tar_read(filtered_brazil_pbf)
# pop_units <- tar_read(pop_units)
# indices <- tar_read(batches_by_pop_unit_area)[[1]]
crop_pbf_data <- function(filtered_brazil_pbf, pop_units, indices) {
  cropped_files <- vapply(
    indices,
    FUN.VALUE = character(1),
    FUN = function(i) {
      pop_unit <- pop_units[i, ]
      
      pop_unit_dir <- paste0(
        "../../data/acesso_oport_v2/r5/",
        pop_unit$code_pop_unit, "_", pop_unit$treated_name
      )
      cropped_pbf_path <- file.path(pop_unit_dir, "street_network.osm.pbf")
      
      rosmium::extract(
        filtered_brazil_pbf,
        extent = pop_unit,
        output_path = cropped_pbf_path,
        overwrite = TRUE
      )
      
      cropped_pbf_path
    }
  )
  
  return(cropped_files)
}