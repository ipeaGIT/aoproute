# targets::tar_visnetwork()
# targets::tar_visnetwork(targets_only = T)


# targets::tar_make(h3_resolutions)
# targets::tar_make(n_batches)
# targets::tar_make(pop_units)
# targets::tar_make(brazil_pbf)
# targets::tar_make(filtered_brazil_pbf)
# targets::tar_make_future(grid_filtered, workers = 4)
# targets::tar_make(n_cells)
# targets::tar_make(batches)
# targets::tar_invalidate(feeds_meta)
# targets::tar_make(transit_areas)
# targets::tar_make_future(feed_paths, workers = 32)
targets::tar_make(feeds_adjusted)



# # just to more easily remember which targets should be run in parallel and which
# # should not. doesn't necessarily run all targets and doesn't necessarily
# # reflects the actual order in which they were run
# targets::tar_make(batches)
# targets::tar_make_future(c(routing_points, elevation_data), workers = 35)
# Sys.sleep(5)
# targets::tar_make_future(pbf_data, workers = 20)
# Sys.sleep(5)
# targets::tar_make_future(r5_network, workers = 8)
# Sys.sleep(5)
# targets::tar_make(walk_matrix, bike_matrix)