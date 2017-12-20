#' Climate matching
#'
#' This function is designed to compare climatic values of one variable in two different time periods from a specific region.
#'
#' The 'target' is grid that contains the location and time period you wish to compare to everywhere, at the comparison time, whereas, the 'comparison' is the grid that represents the rest of the world at the desired time period (e.g. if you want to find out where in the world will be similar to what Hobart is now, in 2080, then the 'target' grid needs to be "the present" and the comparison grid need to be "the future").
#'
#' This function requires the outputs from for both the 'target' and the 'comparison' grids:
#'   `brick_calc_percentiles`: a raster brick of 99 layers, each representating the percentiles from the a specific set of ' source, variable, time period, region'.
#'   `transform_monthly_means_into_growing_season_means`: a raster brick of 12 layers, each representing the monthly mean value for each month of the 'growing season'.  NB: 'growing season' monthly means is when the southern hemisphere has been offset by 6 months, so seasons 'align' globally (as much as they can be, i.e. in the Northern Hemisphere, 1=January, 12=December; in the Southern Hemisphere, 1=July, 12=June).
#'
#' @param output_file_name path to output file
#' @param target_data_percentiles output of the function brick_calc_percentiles for the 'target' region and time period.
#' @param target_data_monthly_mean_growing_season output of the function of `transform_monthly_means_into_growing_season_means` for the 'target' region and time period.
#' @param comparison_data_percentiles output of the function `brick_calc_percentiles` for the 'comparison' region and time period.
#' @param comparison_data_monthly_mean_growing_season output of the function of transform_monthly_means_into_growing_season_means for the 'comparison' region and time period.
#' @param target_coordinates must be a named list in the form of: list(x=x_vals,y=y_vals) in coordinates that match the input data; or, the full file PATH to a shapefile
#'
#' @return Returns a file-backed raster object.  i.e. It creates a .grd file, that can also bee accessible in the environment if assigned to a variable.
#' @export
#' @importFrom raster values<-
#' @examples
#' #I need to put in an example
create_climate_difference_matrix <- function(
  output_file_name,
  target_data_percentiles, target_data_monthly_mean_growing_season,
  comparison_data_percentiles, comparison_data_monthly_mean_growing_season,
  target_coordinates = list(x=149.1300, y=35.2809) #default coordinates are for Canberra, Australia in WGS84 projection, can be a path to a shapefile instead
  ){
  #define the target cells to use to calculate each difference matrix (can be 1 or many)
  if(length(names(target_coordinates)) > 0 ){
    if(names(target_coordinates)[1] == "x" & names(target_coordinates)[2] == "y"){
      cat("\nInput variable target_coordinates is defined with direct x and y values\nI hope these coordinates match the data, I'm not checking.\n")
      target_cells <- raster::cellFromXY(target_data_percentiles, cbind(target_coordinates$x, target_coordinates$y))
    } else { stop("target_coordinates is not defined as expected.  \n  Must be either:\n  - a named list in the form list(x=x_vals,y=y_vals) in coordinates that match the input data\n  OR\n  - the full file PATH to a shapefile") }
  } else {
    if(is.character(target_coordinates)){
    cat("\nInput variable target_coordinates is defined with a shapefile.\n")
    target_shape_raw <- raster::shapefile(target_coordinates)
    target_shape_matched_projection <- sp::spTransform(target_shape_raw, raster::projection(target_data_percentiles))
    target_cells_tmp <- unlist(lapply(raster::cellFromPolygon(target_data_percentiles, target_shape_matched_projection, weights=TRUE),function(x){as.integer(x[which.max(x[,2]),1])})) #selects the cell with the greatest overlap from the group
    #names(target_cells_tmp) <- target_shape_raw$NAME
    target_cells <- sort(unique(target_cells_tmp))
    } else {
      stop("The 'target_coordinates' entered do not match what is expected, check they are the correct type or format." )
    }#close if is.character(target_coordinates)
  } #close if length(names(target_coordinates)) > 0
  print(target_cells)
  #create difference matrix for each target_cell and save this into the output file
  list_of_diff_matrices <- vector("list", length(target_cells))
  for( target_ind in 1:length(target_cells) ){
    target_cell <- target_cells[target_ind]
    #calculate difference for percentiles
    target_cell_percentiles_brick <- target_data_percentiles
    values(target_cell_percentiles_brick) <- rep(as.vector(raster::extract(target_data_percentiles, target_cell)), each=raster::ncell(target_data_percentiles))
    #difference_function <- function(x, y){ ((x-y)^2) / sqrt(sd(x)*sd(y))  } #Experimental idea - needs to happen, but unclear exactly how.
    diff_percentile <- raster::calc(comparison_data_percentiles - target_cell_percentiles_brick, function(x){sum(abs(x))})
    #calculate difference for growing season monthly means
    target_cell_monthly_mean_growing_season_brick <- target_data_monthly_mean_growing_season
    raster::values(target_cell_monthly_mean_growing_season_brick) <- rep(as.vector(raster::extract(target_data_monthly_mean_growing_season, target_cell)), each=raster::ncell(target_data_monthly_mean_growing_season))
    diff_monthly_mean_growing_season <- raster::calc(comparison_data_monthly_mean_growing_season - target_cell_monthly_mean_growing_season_brick, function(x){sum(abs(x))})
    diff_climate <- (diff_percentile/max(raster::values(diff_percentile)) + diff_monthly_mean_growing_season/max(raster::values(diff_monthly_mean_growing_season)))/2
    list_of_diff_matrices[[target_ind]] <- diff_climate
    if(length(names(target_cells)) > 0){
      names(list_of_diff_matrices[[target_ind]]) <- sprintf("%s-%s", names(target_cells), as.character(target_cell))
    } else {
      names(list_of_diff_matrices[[target_ind]]) <- as.character(target_cell)
    } #close if length(names(target_cells)) > 0
  }#close for target_row_ind
  diff_climate_brick <- raster::brick(list_of_diff_matrices)
  raster::writeRaster(x=diff_climate_brick, filename = output_file_name, overwrite=TRUE)
} #close function create_climate_difference_matrix
