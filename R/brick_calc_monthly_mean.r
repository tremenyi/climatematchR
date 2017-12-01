#' Calculate the monthly means of a raster brick
#'
#' This function assumes the input is a raster brick object that has many layers that represent an ordered series of different timesteps. It selects the layers for each month and calulates the mean for that month. Then returns a brick of 12 layers, representing the January-December means.
#' @param input_brick raster brick object that has many layers that represent an series of timesteps.
#' @param input_brick_time_as_posixlt associated POSIXlt class vector describing the time value for each layer.
#'
#' @return Returns a raster brick object with 12 layers, one for each month that has been calulated.  (NB: The function may fail if there are not 12 months in the input brick.)
#' @export
#'
#' @examples
#' #I need to add an example...
brick_calc_monthly_mean <- function(input_brick, input_brick_time_as_posixlt){
  calendar_months <- input_brick_time_as_posixlt$mon + 1
  list_of_monthly_rasters <- lapply(1:12, function(target_month_ind){
    tmp_brick <- raster::subset(input_brick, which(calendar_months == target_month_ind))
    tmp_raster <- raster::calc(tmp_brick, mean, na.rm=TRUE)
    return(tmp_raster)
  }) #close lapply
  names(list_of_monthly_rasters) <- as.character(1:12)
  out_brick <- raster::brick(list_of_monthly_rasters)
  return(out_brick)
}#close function brick_calc_monthly_mean
