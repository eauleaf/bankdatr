#' Subset vector of distances
#'
#' Identifies the subset of distances within x miles of 'within_dist' accounting
#' for the maximum number of observations to return from "nearest_n".
#' Function is useful for filter a dataframe by a vector of input distances
#' Note: nearest_n and within_dist act as 'and' statements, returning the intersection of the pair if both are set

#'
#' @param eucl_dists a vector of numeric distances from function calc_eucl_dist()
#' @param nearest_n a number indicating how many of the nearest distances to return, default is Inf
#' @param within_dist a number specifying the maximum distance in miles used to return observations, default is Inf
#'
#' @return a vector of TRUE or FALSE values denoting which observations are within the nearest_n and within_dist specs
#' @export
#'
#' @examples bdr_subset_by_dists(eucl_dists = c(1, 10.3, 0.5109930, 0.5023643, 0), nearest_n = 4, within_dist = .9 )
bdr_subset_by_dists <- function(eucl_dists = NULL,
                                nearest_n = Inf,
                                within_dist = Inf){

  eucl_dists %>%
    tibble::enframe() %>%
    dplyr::mutate(rank = dplyr::row_number(x = value)) %>%
    dplyr::rename(dist = value, orig_locn = name) %>%
    dplyr::mutate(nearest = rank <= nearest_n,
                  within_dist = dist <= within_dist,
                  selection = as.logical(nearest * within_dist)) %>%
    dplyr::pull(selection)

}




