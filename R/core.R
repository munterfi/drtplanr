## drtplanr core functions

read_config <- function(file = "config.json", path = getwd()) {
  cfg_file <- file.path(path, file)
  tmessage(sprintf("Read config '%s'", cfg_file))
  cfg <- jsonlite::fromJSON(cfg_file)
  if (cfg$here != "") {
    hereR::set_key(cfg$here$key) 
  }
  if (cfg$proxy$url != "") {
    hereR::set_proxy(
      proxy = cfg$proxy$url,
      proxyuserpwd = sprintf("%s:%s", cfg$proxy$usr, cfg$proxy$pw)
    )
  }
}
  
tmessage <- function(text) {
  message(Sys.time(), " ", text)
}
