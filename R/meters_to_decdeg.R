#' @export meters_to_decdeg
#'
#' @title Convert from meters to decimal degrees correcting for global position
#'
#' @description
#' \code{meters_to_decdeg} converts from meters to decimal degrees at a specified
#' position on the globe. The use case this function was developed for was to
#' calculate occurence point uncertainty values, which are usually reported in
#' meters, as decimal degrees.
#'
#' @param occs_df A \code{data.frame} of occurrence locations that incudes
#'   \emph{at least these three columns} - latitude, longitude, and a distance
#'   in meters to be converted to decimal degrees.
#' @param lat_col Name of column of latitude values. Caps sensitive.
#' @param lon_col Name of column of longitude values. Caps sensitive.
#' @param distance Name of column of distance values, in meters. Caps sensitive.
#'
#' @return dist_dd A \code{data.frame} of latitude and longitude distances in
#'   units of degree decimal.
#'
meters_to_decdeg <- function(occs_df, lat_col = "latitude", lon_col = "longitude", distance){
  lat <- occs_df[lat_col]
  lon <- occs_df[lon_col]
  dist <- occs_df[distance]
  lon_uncertainty = dist / (111.32 * 1000)
  lat_uncertainty = dist / (111.32 * 1000) + (cos(lon) / 360)
  dist_dd <- data.frame(lon_uncertainty = lon_uncertainty,
                        lat_uncertainty = lat_uncertainty)
  return(dist_dd)
}
