#' DII_FLAV_INTAKE: Calculate the Flavonoids Component within the DII
#' @description
#' To calculate the intakes of flavan-3-ols, flavones, flavonols, flavonones, anthocyanidins, and isoflavones for participants from NHANES 2007-2010 and 2017-2018.
#'
#' @param FLAV_DR1TOT_PATH The file path for the Flavonoid Intake Data File for Day 1. The file names should be formatted like: flav_dr1tot_0710.sas7bdat, flav_dr1tot_1718.sas7bdat, etc. Alternatively, it can be a dataframe already imported into the Global Environment from files like flav_dr1tot_0710.sas7bdat, flav_dr1tot_1718.sas7bdat, etc.
#' @param FLAV_DR2TOT_PATH The file path for the Flavonoid Intake Data File for Day 2. The file names should be formatted like: flav_dr2tot_0708.sas7bdat, flav_dr2tot_1718.sas7bdat, etc. Alternatively, it can be a dataframe already imported into the Global Environment from files like flav_dr2tot_0708.sas7bdat, flav_dr2tot_1718.sas7bdat, etc.
#'
#' @return A dataframe with the variable name SEQN and variable names for the intakes of isoflavones, anthocyanidins, flavan-3-ols, flavanones, flavones and flavonols for Day 1, Day2, or both.
#' @export
#'
#' @examples
#' # Example 1
#' DII_FLAV_INTAKE(FLAV_DR1TOT_PATH = './flav_dr1tot_0710.sas7bdat',
#'                 FLAV_DR2TOT_PATH = './flav_dr2tot_0710.sas7bdat')
#' # Example 2
#' DII_FLAV_INTAKE(FLAV_DR1TOT_PATH = './flav_dr1tot_0710.sas7bdat')
#' # Example 3
#' DII_FLAV_INTAKE(FLAV_DR2TOT_PATH = flav_dr2tot_1718) # flav_dr2tot_1718 is a dataframe already imported into the Global Environment from file flav_dr2tot_1718.sas7bdat.
DII_FLAV_INTAKE <- function(FLAV_DR1TOT_PATH = NULL, FLAV_DR2TOT_PATH = NULL){

  if(is.null(FLAV_DR1TOT_PATH) & is.null(FLAV_DR2TOT_PATH)){
    stop(" Please provide Total Flavonoid File for either Day 1, Day 2, or both. The file names should be formatted as follows: flav_dr1tot_0708.sas7bdat, flav_dr1tot_0710.sas7bdat, flav_dr2tot_1718.sas7bdat.")
  }

  message <- paste0(' The extraction of the flavonoid components used for calculating the DII has been initiated')
  cat('\n')
  for (i in 1:3) {
    cat("\r", message, paste(rep(".", i), collapse = ""), "  ")
    Sys.sleep(1)
  }
  cat('\n')

  if (!is.null(FLAV_DR1TOT_PATH)) {

    if (is.character(FLAV_DR1TOT_PATH) == TRUE) {
      FLAV_DR1TOT = haven::read_sas(data_file = FLAV_DR1TOT_PATH)
    }else {
      FLAV_DR1TOT = FLAV_DR1TOT_PATH
    }

    dr1tot.flav <- FLAV_DR1TOT %>%
      dplyr::select(., all_of(c("seqn", "dr1t_fl_iso", "dr1t_fl_antho", "dr1t_fl_3_ols", "dr1t_fl_nones", "dr1t_fl_ones", "dr1t_fl_ols"))) %>%
      dplyr::rename(., 'SEQN' = 'seqn') %>%
      dplyr::filter(!if_all(-SEQN, is.na))
  }


  if (!is.null(FLAV_DR2TOT_PATH)) {

    if (is.character(FLAV_DR2TOT_PATH) == TRUE) {
      FLAV_DR2TOT = haven::read_sas(data_file = FLAV_DR2TOT_PATH)
    }else {
      FLAV_DR2TOT = FLAV_DR2TOT_PATH
    }

    dr2tot.flav <- FLAV_DR2TOT %>%
      dplyr::select(., all_of(c("seqn", "dr2t_fl_iso", "dr2t_fl_antho", "dr2t_fl_3_ols", "dr2t_fl_nones", "dr2t_fl_ones", "dr2t_fl_ols"))) %>%
      dplyr::rename(., 'SEQN' = 'seqn') %>%
      dplyr::filter(!if_all(-SEQN, is.na))
  }

  if(!is.null(FLAV_DR1TOT_PATH) & !is.null(FLAV_DR2TOT_PATH)){
    dr12tot.flav <- dplyr::full_join(dr1tot.flav, dr2tot.flav, by = 'SEQN')
    cat('\n  The extraction for the flavonoid components of DII has been done based on the Total Flavonoid Files for Day 1 and Day 2.')
    return(dr12tot.flav)
    }
  if(!is.null(FLAV_DR1TOT_PATH) & is.null(FLAV_DR2TOT_PATH)){
    cat('\n  The extraction for the flavonoid components of DII has been done based on the Total Flavonoid Files for Day 1.')
    return(dr1tot.flav)
    }
  if(is.null(FLAV_DR1TOT_PATH) & !is.null(FLAV_DR2TOT_PATH)){
    cat('\n  The extraction for the flavonoid components of DII has been done based on the Total Flavonoid Files for Day 2.')
    return(dr2tot.flav)
    }
}
