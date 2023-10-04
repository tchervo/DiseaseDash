#' Write to the DiseaseDash cache
#'
#' @param data The data to write to the cache
#' @param data_type The type of data you are saving. Either "cases" or "vax. Used to update the log
#'
#' @return No return
#' @export
#'
#' @examples
#' cases_data <- get_infection_data()
#'
#' write_cache(cases_data, "cases")
write_cache <- function(data, data_type) {
  stopifnot(data_type %in% c("cases", "vax"))

  cache_name = ".dd_cache"
  cache_dir = paste0(getwd(), "/", cache_name)

  # We'll need to setup the directory, tracking file, and write the data
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir)

    # Save the data
    saveRDS(data, paste0(cache_dir, "/", data_type, "_cache.rds"))

    # Create a cache info DF
    cache_info = data.frame(type = data_type, update_dt = Sys.Date())

    # Write cache info
    saveRDS(cache_info, paste0(cache_dir, "/", "cache_info.rds"))
  } else {
    saveRDS(data, paste0(cache_dir, "/", data_type, "_cache.rds"))

    # Get cache info
    cache_info = readRDS(paste0(cache_dir, "/", "cache_info.rds"))

    # Check to see if this data type has been cached before
    # If yes, update the cache entry. If not, create a new entry
    if (data_type %in% cache_info$type) {
      cache_info[which(cache_info$type == data_type), "update_dt"] = Sys.Date()
    } else {
      cache_info = rbind(cache_info,
                         data.frame(type = data_type, update_dt = Sys.Date()))
    }

    # Write cache info
    saveRDS(cache_info, paste0(cache_dir, "/", "cache_info.rds"))
  }
}
