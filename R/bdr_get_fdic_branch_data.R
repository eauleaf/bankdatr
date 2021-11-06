#' API to FDIC Branch Deposit Data for all US branches for any available period
#'
#' returns the complete yearly data from the fdic website
#' Note: data is only available from 1994 or sooner
#' human URL: https://www7.fdic.gov/sod/dynaDownload.asp?barItem=6
#' human URL info: https://www7.fdic.gov/sod/sodDownload3.asp?sState=all&sInfoAsOf=2020&submit1=Continue&barItem=6
#'
#' @param .year integer, the year you want to get fdic branch data for
#'
#' @return tibble of the entire fdic branch data file for the specified year or NULL if no success
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples fdic_2019 <- bdr_get_fdic_branch_data(2019)
#' @examples fdic <- 2020:2021 %>% map(bdr_get_fdic_branch_data)
#' @examples fdic_future <- bdr_get_fdic_branch_data(2025)

bdr_get_fdic_branch_data <- function(.year = 2020){


  fdic_url <- glue::glue('https://www7.fdic.gov/sod/ShowFileWithStats1.asp?strFileName=ALL_{.year}.zip')

  message('\nAttempting to download FDIC zip file data from ', fdic_url, '\n\t')

  safe_curl_download <- purrr::possibly(curl::curl_download, otherwise = NULL)
  zip_file <- safe_curl_download(url = fdic_url, destfile = tempfile(fileext = ".zip"), quiet = F)


  if(purrr::is_null(zip_file)) {
    message('\nUnable to download FDIC data for ', .year, '\n')
    return(NULL)
  } else {
    message('\nDownloaded FDIC data for ', .year, ' Extracting and returning it as a tibble.\n')
  }

  utils::unzip(zip_file, exdir = dirname(zip_file), overwrite=TRUE)

  c_types <- readr::cols(YEAR = 'd', CERT = 'd', BRNUM = 'd', UNINUMBR = 'd', NAMEFULL = 'c',
                         ADDRESBR = 'c', CITYBR = 'c', CNTYNAMB = 'c', STALPBR = 'c',
                         ZIPBR = 'c', BRCENM = 'c', CONSOLD = 'd', BRSERTYP = 'd',
                         DEPSUMBR = 'n', BKMO = 'd', CBSA_DIV_NAMB = 'c', CITY2BR = 'c',
                         CNTRYNAB = 'c', CNTYNUMB = 'd', CSABR = 'd', CSANAMBR = 'c',
                         DIVISIONB = 'd', MSABR = 'd', MSANAMB = 'c', METROBR = 'd',
                         MICROBR = 'd', NAMEBR = 'c', NECTABR = 'd', NECNAMB = 'c',
                         PLACENUM = 'd', SIMS_ACQUIRED_DATE = 'c', SIMS_ESTABLISHED_DATE = 'c',
                         SIMS_LATITUDE = 'd', SIMS_LONGITUDE = 'd', SIMS_DESCRIPTION = 'c',
                         SIMS_PROJECTION = 'c', STCNTYBR = 'd', STNAMEBR = 'c', STNUMBR = 'd',
                         HCTMULT = 'c', RSSDHCR = 'd', NAMEHCR = 'c', CITYHCR = 'c', STALPHCR = 'c',
                         RSSDID = 'd', UNIT = 'd', ADDRESS = 'c', CITY = 'c', STALP = 'c', ZIP = 'c',
                         ASSET = 'n', BKCLASS = 'c', CALL = 'c', CHARTER = 'c', CHRTAGNN = 'c',
                         CHRTAGNT = 'c', CLCODE = 'd', CNTRYNA = 'c', DENOVO = 'd', DEPDOM = 'n',
                         DEPSUM = 'n', DOCKET = 'd', ESCROW = 'n', FDICDBS = 'd', FDICNAME = 'c',
                         FED = 'd', FEDNAME = 'c', INSAGNT1 = 'c', INSURED = 'c', INSBRDD = 'd',
                         INSBRTS = 'd', OCCDIST = 'd', OCCNAME = 'c', REGAGNT = 'c', SPECGRP = 'd',
                         SPECDESC = 'c', STCNTY = 'c', STNAME = 'c', USA = 'd')


  branch_datr <-
    file.path(dirname(zip_file),glue::glue('ALL_{.year}.csv')) %>%
    readr::read_csv(file = ., col_types = c_types) %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(where(is.character), .fns = ~iconv(., 'Latin1', 'UTF8', '')))

  unlink(dirname(zip_file), recursive = T)

  return(branch_datr)

}

