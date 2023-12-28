# just to more easily remember which targets should be run in parallel and which
# should not. doesn't necessarily run all targets and doesn't necessarily
# reflects the actual order in which they were run
tar_make(batches)
tar_make_future(c(routing_points, elevation_data), workers = 35)
Sys.sleep(5)
tar_make_future(pbf_data, workers = 20)
Sys.sleep(5)
tar_make_future(r5_network, workers = 8)
Sys.sleep(5)
tar_make(walk_matrix, bike_matrix)