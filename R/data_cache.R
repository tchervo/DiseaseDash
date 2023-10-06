# Internal function to get the cache directory. Users can use other functions
# to access the cache so this is kept private
.get_cache_dir <- function() {
  cache_name <- ".dd_cache"

  paste0(getwd(), "/", cache_name)
}

#' Write to the DiseaseDash cache
#'
#' @param data The data to write to the cache
#' @param data_type The type of data you are saving. Either "cases" or "vax". Used to update the log
#'
#' @return No return
#' @export
#'
#' @examples
#' \dontrun{
#'   cases_data <- get_infection_data()
#'
#'   write_cache(cases_data, "cases")
#' }
write_cache <- function(data, data_type) {
  stopifnot(data_type %in% c("cases", "vax"))

  cache_dir <- .get_cache_dir()

  # We'll need to setup the directory, tracking file, and write the data
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir)

    # Save the data
    saveRDS(data, paste0(cache_dir, "/", data_type, "_cache.rds"))

    # Create a cache info DF
    cache_info <- data.frame(type = data_type, update_dt = Sys.Date())

    # Write cache info
    saveRDS(cache_info, paste0(cache_dir, "/", "cache_info.rds"))
  } else {
    saveRDS(data, paste0(cache_dir, "/", data_type, "_cache.rds"))

    # Get cache info
    cache_info <- readRDS(paste0(cache_dir, "/", "cache_info.rds"))

    # Check to see if this data type has been cached before
    # If yes, update the cache entry. If not, create a new entry
    if (data_type %in% cache_info$type) {
      cache_info[which(cache_info$type == data_type), "update_dt"] <- Sys.Date()
    } else {
      cache_info <- rbind(cache_info,
                         data.frame(type = data_type, update_dt = Sys.Date()))
    }

    # Write cache info
    saveRDS(cache_info, paste0(cache_dir, "/", "cache_info.rds"))
  }
}

#' Read a data type from the DiseaseDash cache
#'
#' @param data_type The type of data you are retrieving. Either "cases" or "vax".
#'
#' @return A data.frame containing data on either COVID-19 cases or vaccinations
#' @export
#'
#' @examples
#' \dontrun{cases_data <- read_cache("cases")}
read_cache <- function(data_type) {
  stopifnot(data_type %in% c("cases", "vax"))
  if (!check_cache(data_type)) stop(paste(data_type, "is not saved in the cache!"))

  cache_info <- get_cache_info()
  update_dt <- cache_info[[which(cache_info$type == data_type), "update_dt"]]

  cache_dir <- .get_cache_dir()

  print(paste("Retrieving data for:", data_type, "last saved on:", update_dt))

  readRDS(paste0(cache_dir, "/", data_type, "_cache.rds"))
}

#' Get DiseaseDash cache info file
#'
#' @description
#' Reads the DiseaseDash cache info file if it exists. Returns NULL otherwise
#'
#'
#' @return A data.frame containing the data types stored in the cache and the
#'     last time they were updated. NULL if there is no cache
#' @export
#'
#' @examples
#' \dontrun{cache_info <- get_cache_info()}
get_cache_info <- function() {
  cache_dir <- .get_cache_dir()

  if (!dir.exists(cache_dir)) return(NULL)

  readRDS(paste0(cache_dir, "/cache_info.rds"))
}

#' Check if a file exists in the DiseaseDash cache
#'
#' @param data_type The type of data to check for. Should be either "cases" or "vax"
#'
#' @return TRUE if the data type is stored in the cache, FALSE otherwise
#' @export
#'
#' @examples
#' \dontrun{
#'   if (check_cache("vax")) {
#'      # Do something here
#'    }
#' }
check_cache <- function(data_type) {
  stopifnot(data_type %in% c("cases", "vax"))

  cache_info <- get_cache_info()

  if (is.null(cache_info)) return(FALSE)

  data_type %in% cache_info$type
}
