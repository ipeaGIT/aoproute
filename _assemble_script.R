# just to more easily remember which targets should be run in parallel and which
# should not. doesn't necessarily run all targets and doesn't necessarily
# reflects the actual order in which they were run
tar_make(batches)
tar_make_future(routing_points, workers = 35)
tar_make(elevation_data)