#' Download and return FDIC Branch Deposit Data for all US branches for any available period
#'
#' Gets the complete yearly data from the fdic website and saves the data to a
#' local specified folder, or if the data has already been downloaded, the
#' function just picks up the saved data
#' Note: data is only available from 1994 or sooner
#' human URL: https://www7.fdic.gov/sod/dynaDownload.asp?barItem=6
#' human URL info: https://www7.fdic.gov/sod/sodDownload3.asp?sState=all&sInfoAsOf=2020&submit1=Continue&barItem=6
#'
#' @param .year integer, the year you want to get fdic branch data for
#' @param save_dir string, the location to save the zip and rds files, default is here::here('data')
#' @param keep_fdic_rds_file boolean to save the rds file in the save_dir, default is T
#' @param keep_fdic_zip_file boolean to save the zip file in the save_dir, default is F
#'
#' @return tibble of the entire fdic branch data file and corresponding saved rds file, as well as saved zip files if elected
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples fdic_2020 <- get_fdic_branch_deposit_data(2020)

get_fdic_branch_deposit_data <- function(.year = 2020
                                         ,save_dir = here::here('data')
                                         ,keep_fdic_rds_file = T
                                         ,keep_fdic_zip_file = F
){

  # require(tidyverse)

  # create a path if the first time
  dir.create(path = save_dir, showWarnings = F, recursive = T)
  zip_file <- file.path(save_dir, glue::glue('all_branches_{.year}.zip'))
  rds_file <- file.path(save_dir, glue::glue('all_branches_{.year}.rds'))

  # if rds file already exists, return it and be done
  if (file.exists(rds_file)) {
    message('the .rds file exists for ', .year, ' -- returning it to you now\n')
    return( readr::read_rds(rds_file) )

    # if zip file already exists, don't download it again
  } else if (!file.exists(zip_file)){

    fdic_url <- glue::glue('https://www7.fdic.gov/sod/ShowFileWithStats1.asp?strFileName=ALL_{.year}.zip')
    message('\nattempting to download FDIC data for ', .year, 'from:\n\t',
            fdic_url)

    safe_curl_download <- purrr::possibly(curl::curl_download, otherwise = NULL)
    zip_file <- safe_curl_download(url = fdic_url, destfile = zip_file)
    if(purrr::is_null(zip_file)) {
      message('\nunable to download FDIC data for', .year, '\n')
      return(NULL)
    }
    else {
      message('\ndownloaded FDIC data for', .year, '\n')
    }

  }


  utils::unzip(zip_file, exdir = save_dir)

  # list csv files
  csv_files <- list.files(save_dir,'csv$',full.names = T)

  # define fdic column types
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

  # read complete csv file and write to rds
  branch_datr <- csv_files[(csv_files %>% stringr::str_detect((glue::glue('ALL_{.year}.csv$') %>% as.character())))] %>%
    readr::read_csv(file = ., col_types = c_types) %>%
    janitor::clean_names()

  if(keep_fdic_rds_file) {
    readr::write_rds(branch_datr,
                     file = file.path(rds_file),
                     compress = 'xz')
    message("Your saved FDIC branch rds file is here: \n",
            file.path(save_dir,paste0(' deposits_',.year,'.rds')),"\n"
    )
  }

  # delete interim files
  file.remove(csv_files)
  if (!keep_fdic_zip_file) {file.remove(zip_file)}


  # return tibble
  message('\nreturning tibble of',.year,'FDIC data\n')
  return(branch_datr)

}


