#' Calculate geographic distance estimates
#'
#' Calculates geographic distances between a latitude and longitude point location
#' against vectors of similar latitude and longitude points representing multiple locations
#' Note: this travel distance estimate is only useful for relatively close distances, i.e. a couple hundred miles
#'
#' @param pnt_lat a point latitude
#' @param pnt_lon a point longitude
#' @param vec_lat a vector of latitude points
#' @param vec_lon a vector of longitude points
#'
#' @return a vector of distances from the point coordinate the length of vec_lat
#' @export
#'
#' @examples bdr_calc_geo_dist(pnt_lat = 44.98425, pnt_lon = -93.26644, vec_lat = c(44.96280,44.97361,44.97566), vec_lon = c(-93.34795,-93.22987,-93.27227))
bdr_calc_geo_dist <- function(pnt_lat = NULL,
                               pnt_lon = NULL,
                               vec_lat = NULL,
                               vec_lon = NULL){

  # length of a longitude degree depends on latitude
  deglen <- 68.50615 # 110.25 * 0.621371 # kilometers * miles / kilometer

  (deglen * sqrt((pnt_lat - vec_lat)^2 + (cos(pnt_lat)*(pnt_lon - vec_lon))^2))

}
