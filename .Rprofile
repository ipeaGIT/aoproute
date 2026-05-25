source("renv/activate.R")

if(Sys.getenv("RUSER") == "DHERSZ") {
  source("~/.Rprofile")
} else {
  r_profile <- file.path("C://Users/", Sys.getenv("RUSER"), ".Rprofile")
  if(file.exists(r_profile)) {
    source(r_profile)
  }
}
