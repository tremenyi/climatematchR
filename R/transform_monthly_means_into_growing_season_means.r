#' Calculate growing season monthly means
#'
#' Gowing seasons are approximately offset by 12 months between the Northern and Southern Hemispheres.  Although this is a gross simplification, when trying to compare monthly values globally, it is necessary to align the summers and the winters, reffered to in this context as the growing seasons.  This function aims to achive this by shifting the Southern hemishere values of a given grid by 6 months.
#'
#' @param brick_of_monthly_means output from `brick_calc_monthly_mean`. A raster brick of 12 layers representing the 12 months of the year.
#'
#' @return a raster brick object with 12 layers, indicating the 12 months of the year, starting in the middle of winter.
#' @export
#'
#' @examples
#' #I need to add an exmple...
#' tmp_file <- "/place/where/i/keep/my/data"
#' tmp_brick <- brick(tmp_file)
#' tmp_out <- transform_monthly_means_into_growing_season_means(tmp_brick)
transform_monthly_means_into_growing_season_means <- function(brick_of_monthly_means){
  extent_northern_hemisphere <- raster::extent(0, 360, 0, 90)
  extent_southern_hemisphere <- raster::extent(0, 360, -90, 0)
  northern_hemisphere <- raster::crop(brick_of_monthly_means, extent_northern_hemisphere)
  southern_hemisphere <- raster::subset(raster::crop(brick_of_monthly_means, extent_southern_hemisphere), c(7:12, 1:6))
  growing_season_monthly_mean <- raster::merge(northern_hemisphere, southern_hemisphere, overlap=FALSE)
  return(growing_season_monthly_mean)
}#close function
