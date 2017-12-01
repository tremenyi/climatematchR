#' Extract and combine time values from multiple netcdf files.
#'
#' Time handling in any file format is constanly a pain. It can be described in a range of different ways, relative to a range of different starting points.  For example, "Minutes since 1800-01-01 00:00:00", or "Seconds since 1960-01-01 00:00:00". This tool is a wrapper around RNetCDF tools that extract and transform the time representation in netcdf files into a useable, standard reference, human readable format.
#'
#' @param nc_files_list A list of files you wish to combine together, but have independently defined "Time" variables.
#'
#' @return Returns a vector of character strings in the form of "YYYY-MM-DD hh:mm:ss". This can then be used by POSIXct or POSIXlt to get date or time class objects.
#' @export
#'
#' @examples
#' #I need to put in an example...
extract_and_combine_time_from_netcdf_files <- function(nc_files_list){
  if(!loadNamespace("RNetCDF")){stop("RNetCDF package required.")}
  out_time_POSIXct <- unlist(lapply(nc_files_list, function(target_file){
    tmp_nc <- RNetCDF::open.nc(target_file)
    tmp_time_unitstring <- RNetCDF::att.get.nc(ncfile = tmp_nc, variable = "time", attribute = "units")
    tmp_time_raw <- RNetCDF::var.get.nc(ncfile = tmp_nc, variable = "time")
    RNetCDF::close.nc(tmp_nc)
    tmp_time_string <- RNetCDF::utcal.nc(unitstring = tmp_time_unitstring, value = tmp_time_raw, type = "s")
  })) #close unlist
  return(out_time_POSIXct)
}#close function extract_and_combine_time_from_netcdf_files
