#' DII_NHANES_PLUS: Calculate the DII with Additional Components Included in One Step
#' @description
#' This function calculates the dietary inflammation index (DII) within one step, incorporating garlic, ginger, onion, pepper, and oregano/thyme, along with the components used in the DII_NHANES_FPED() from the R package 'dietaryindex' for NHANES participants. Additionally, for participants from NHANES 2007-2010 and 2017-2018, the intakes of flavan-3-ols, flavones, flavonols, flavonones, anthocyanidins, and isoflavones are included.
#'
#' @param FPED_PATH The file path for the total Food Patterns Equivalents Intakes per Individual for Day 1. The file name should be formatted like: fped_dr1tot_0708.sas7bdat, pyr_tot.sas7bdat, etc. Alternatively, it can be a dataframe already imported into the Global Environment from files like fped_dr1tot_0708.sas7bdat, pyr_tot.sas7bdat, etc.
#' @param NUTRIENT_PATH The file path for the total Nutrient Intakes file for Day 1. The file name should be formatted like: DR1TOT_J.XPT, DRXTOT_B.XPT, etc. Alternatively, it can be a dataframe already imported into the Global Environment from files like DR1TOT_J.XPT, DRXTOT_B.XPT, etc.
#' @param DEMO_PATH The file path for the DEMOGRAPHIC data. The file name should be formatted like: DEMO_J.XPT. Alternatively, it can be a dataframe already imported into the Global Environment from files like DEMO_J.XPT.
#' @param FPED_PATH2 The file path for the total Food Patterns Equivalents Intakes per Individual for Day 2. The file name should be formatted like: fped_dr2tot_1718.sas7bdat, pyr_tot_d2.sas7bdat, etc. Alternatively, it can be a dataframe already imported into the Global Environment from files like fped_dr2tot_1718.sas7bdat, pyr_tot_d2.sas7bdat, etc.
#' @param NUTRIENT_PATH2 The file path for the total Nutrient Intakes file for Day 2. The file name should be formatted like: DR2TOT_D.XPT, DRXTOT_B.XPT, etc. Alternatively, it can be a dataframe already imported into the Global Environment from files like DR2TOT_D.XPT, DRXTOT_B.XPT, etc.
#' @param FNDDS_PATH The file path for the a FNDDS file in Microsoft Access® format. The file names should be formatted like: FNDDS.mdb, FNDDS_2017-2018.mdb, etc. Alternatively, it can be a dataframe that has already been imported into the Global Environment from the table named FNDDSSRLinks or FNDDSIngred within an FNDDS file in Microsoft Access® format, such as FNDDS.mdb, FNDDS_2017-2018.mdb, etc.
#' @param DR1IFF_PATH The file path for the Individual Foods File for Day 1. The file names should be formatted like: DR1IFF_E.XPT, DRXIFF_B.XPT, etc. Alternatively, it can be a dataframe already imported into the Global Environment from files like DR1IFF_F.XPT, DRXIFF_B.XPT, etc.
#' @param DR2IFF_PATH The file path for the Individual Foods File for Day 2. The file names should be formatted like: DR2IFF_C.XPT, DRXIFF_B.XPT, etc. Alternatively, it can be a dataframe already imported into the Global Environment from files like DR2IFF_D.XPT, DRXIFF_B.XPT, etc.
#' @param FLAV_DR1TOT_PATH The file path for the Flavonoid Intake Data File for Day 1. The file names should be formatted like: flav_dr1tot_0710.sas7bdat, flav_dr1tot_1718.sas7bdat, etc. Alternatively, it can be a dataframe already imported into the Global Environment from files like flav_dr1tot_0710.sas7bdat, flav_dr1tot_1718.sas7bdat, etc.
#' @param FLAV_DR2TOT_PATH The file path for the Flavonoid Intake Data File for Day 2. The file names should be formatted like: flav_dr2tot_0708.sas7bdat, flav_dr2tot_1718.sas7bdat, etc. Alternatively, it can be a dataframe already imported into the Global Environment from files like flav_dr2tot_0708.sas7bdat, flav_dr2tot_1718.sas7bdat, etc.
#'
#' @return A dataframe with the variable name SEQN and variable names for the intakes of garlic, ginger, onion, pepper, thyme/oregano, isoflavones, anthocyanidins, flavan-3-ols, flavanones, flavones and flavonols for Day 1, Day 2, or both.
#' @export
#'
#' @examples
#' #' # Example 1
#' DII_NHANES_PLUS(FPED_PATH = './fped_dr1tot_0910.sas7bdat',
#'                 NUTRIENT_PATH = './DR1TOT_F.XPT',
#'                 DEMO_PATH = './DEMO_F.XPT',
#'                 FPED_PATH2 = './fped_dr2tot_0910.sas7bdat',
#'                 NUTRIENT_PATH2 = './DR2TOT_F.XPT', FNDDS_PATH = './FNDDS5.mdb',
#'                 DR1IFF_PATH = './DR1IFF_E.XPT', DR2IFF_PATH = './DR2IFF_E.XPT',
#'                 FLAV_DR1TOT_PATH = './flav_dr1tot_0710.sas7bdat',
#'                 FLAV_DR2TOT_PATH = './flav_dr2tot_0710.sas7bdat')
#' # Example 2
#' DII_NHANES_PLUS(FPED_PATH = fped_dr1tot_0708,
#'                 NUTRIENT_PATH = './DR1TOT_E.XPT',
#'                 DEMO_PATH = './DEMO_E.XPT',
#'                 FNDDS_PATH = './FNDDS4.1.mdb',
#'                 DR1IFF_PATH = './DR1IFF_E.XPT',
#'                 FLAV_DR1TOT_PATH = './flav_dr1tot_0708.sas7bdat') # fped_dr1tot_0708 is a dataframe already imported into the Global Environment from file fped_dr1tot_0708.sas7bdat.
#' # Example 3
#' DII_NHANES_PLUS(DEMO_PATH = demo.j,
#'                 FPED_PATH2 = './fped_dr2tot_1718.sas7bdat',
#'                 NUTRIENT_PATH2 = './DR2TOT_J.XPT',
#'                 FNDDS_PATH = './FNDDS_2017-2018.mdb',
#'                 DR2IFF_PATH = './DR2IFF_J.XPT',
#'                 FLAV_DR2TOT_PATH = './flav_dr2tot_1718.sas7bdat') # demo.j is a dataframe already imported into the Global Environment from file DEMO_J.XPT.
#' # Example 4
#' DII_NHANES_PLUS(FPED_PATH = './pyr_tot_d1.sas7bdat',
#'                 NUTRIENT_PATH = dr1tot.c,
#'                 DEMO_PATH = './DEMO_C.XPT',
#'                 FNDDS_PATH = './FNDDS2.mdb',
#'                 DR1IFF_PATH = './DR1IFF_C.XPT') # dr1tot.c is a dataframe already imported into the Global Environment from file DR1TOT_C.XPT.
#' # Example 5
#' DII_NHANES_PLUS(FPED_PATH2 = './fped_dr2tot_1314.sas7bdat',
#'                 NUTRIENT_PATH2 = './DR2TOT_H.XPT',
#'                 DEMO_PATH = './DEMO_H.XPT',
#'                 FNDDS_PATH = './FNDDS2013-2014.mdb',
#'                 DR2IFF_PATH = dr2iff.h) # dr2iff.h is a dataframe already imported into the Global Environment from file DR2IFF_H.XPT.
#' # Example 6
#' DII_NHANES_PLUS(FPED_PATH = './pyr_tot.sas7bdat',
#'                 NUTRIENT_PATH = './DRXTOT_B.XPT',
#'                 DEMO_PATH = './DEMO_B.XPT',
#'                 FNDDS_PATH = './FNDDS.mdb',
#'                 DR1IFF_PATH = './DRXIFF_B.XPT')
DII_NHANES_PLUS <- function(FPED_PATH = NULL, NUTRIENT_PATH = NULL, DEMO_PATH = NULL, FPED_PATH2 = NULL, NUTRIENT_PATH2 = NULL,
                            FNDDS_PATH = NULL, DR1IFF_PATH = NULL, DR2IFF_PATH = NULL, FLAV_DR1TOT_PATH = NULL, FLAV_DR2TOT_PATH = NULL){

  cat('\n  Thank you for using dietaryindexNDP! This R package is designed to calculate the Dietary Inflammation Index (DII) based on the DII_NHANES_FPED function from the ‘dietaryindex’ package. It incorporates additional components, including garlic, ginger, onion, pepper, and thyme/oregano for NHANES participants, as well as flavan-3-ols, flavones, flavonols, flavanones, anthocyanidins, and isoflavones for certain survey periods. Due to the lengthy calculation process, the DII has been pre-calculated to expedite the process. Researchers can access the results directly using the DII_NHANES_PLUS_RESULT() function provided in this R package.\n')
  cat('\n  Please cite the following two publications when using this R package in your research:\n')
  cat('  1. J.J. Zhan, R.A. Hodge, A.L. Dunlop, M.M. Lee, L. Bui, D. Liang, E.P. Ferranti, Dietaryindex: A User-Friendly and Versatile R Package for Standardizing Dietary Pattern Analysis in Epidemiological and Clinical Studies, The American Journal of Clinical Nutrition, https://doi.org/10.1016/j.ajcnut.2024.08.021.\n')
  cat('  2. Improving the dietaryindex R Package: A Proposal to Include Additional Components for More Accurate DII Computation in NHANES, The American Journal of Clinical Nutrition. (Under review)\n')

  cat('\n  The calculation of the DII consists of three steps:\n')
  cat('  1. The first step is flattening the FNDDSSRLinks or FNDDSIngred file.\n')
  cat('  2. The second step involves calculating the intakes of garlic, ginger, onion, pepper, thyme and oregano, as well as isoflavones, anthocyanidins, flavan-3-ols, flavanones, flavones and flavonols for specific NHANES survey periods.\n')
  cat('  3. In the third step, the DII is calculated based on components such as alcohol, vitamins (B12, B6, A, C, D, E), β-carotene, caffeine, carbohydrates, cholesterol, energy, fiber, folic acid, iron, magnesium, monounsaturated fatty acids (MUFA), niacin, n-6 fatty acids, protein, polyunsaturated fatty acids (PUFA), riboflavin, saturated fat, selenium, thiamin, zinc, garlic, ginger, onion, pepper, thyme, and oregano. It is also advisable to include isoflavones, anthocyanidins, flavan-3-ols, flavanones, flavones, and flavonols for participants of NHANES 2007-2008, 2009-2010 or 2017-2018.\n')

  if (is.null(FPED_PATH) & is.null(NUTRIENT_PATH) & is.null(FPED_PATH2) & is.null(NUTRIENT_PATH2)
      & is.null(DR1IFF_PATH) & is.null(DR2IFF_PATH)) {
    stop("  Please provide the file path for the FPED data, Total Nutrient Intake and Individual Foods Files for either Day 1, Day 2, or both.")
  }
  if (!is.null(FPED_PATH) & !is.null(NUTRIENT_PATH)) {
    if(!is.null(FPED_PATH2) & is.null(NUTRIENT_PATH2)){
      cat("\n  Caution: Since there is no Total Nutrient Intakes data for Day 2, the DII for that day cannot be calculated.")
    }
    if(is.null(FPED_PATH2) & !is.null(NUTRIENT_PATH2)){
      cat("\n  Caution: Since there is no FPED data for Day 2, the DII for that day cannot be calculated.")
    }
  }

  if (!is.null(FPED_PATH2) & !is.null(NUTRIENT_PATH2)) {
    if(!is.null(FPED_PATH) & is.null(NUTRIENT_PATH)){
      cat("\n  Caution: Since there is no Total Nutrient Intakes data for Day 1, the DII for that day cannot be calculated.")
    }
    if(is.null(FPED_PATH) & !is.null(NUTRIENT_PATH)){
      cat("\n  Caution: Since there is no FPED data for Day 1, the DII for that day cannot be calculated.")
    }
  }

  if(is.null(FNDDS_PATH)){
    stop('  Please provide a version of the FNDDS specific to a particular NHANES survey period. The file names should adhere to the following format: FNDDS2.mdb, FNDDS2011-2012.mdb, FNDDS_2017-2018.mdb, and so forth.')
  }

  if(is.null(DR1IFF_PATH) & is.null(DR2IFF_PATH)){
    stop("  Please provide Individual Foods Files for either Day 1, Day 2, or both.")}

  FPED <- FPED_PATH
  NUTRIENT <- NUTRIENT_PATH
  DEMO <- DEMO_PATH
  FPED2 <- FPED_PATH2
  NUTRIENT2 <- NUTRIENT_PATH2
  FNDDS <- FNDDS_PATH
  DR1IFF <- DR1IFF_PATH
  DR2IFF <- DR2IFF_PATH
  FLAV_DR1TOT <- FLAV_DR1TOT_PATH
  FLAV_DR2TOT <- FLAV_DR2TOT_PATH

  FNDDSSRLinks_FNDDSIngred_flattened1 <- FNDDSSRLinks_FNDDSIngred_flattened(FNDDS_PATH = FNDDS)
  DII_ONESTEP_INTAKE2 <- DII_ONESTEP_INTAKE(FLATTENED_FILEPATH = FNDDSSRLinks_FNDDSIngred_flattened1,
                                            DR1IFF_PATH = DR1IFF,
                                            DR2IFF_PATH = DR2IFF,
                                            FLAV_DR1TOT_PATH = FLAV_DR1TOT,
                                            FLAV_DR2TOT_PATH = FLAV_DR2TOT)
  DII_NHANES_CALCULATION3 <- DII_NHANES_CALCULATION(FPED_PATH = FPED,
                                      DEMO_PATH = DEMO,
                                      NUTRIENT_PATH = NUTRIENT,
                                      FPED_PATH2 = FPED2,
                                      NUTRIENT_PATH2 = NUTRIENT2,
                                      OTHER_INGREDIENTS = DII_ONESTEP_INTAKE2)

  return(DII_NHANES_CALCULATION3)
}

