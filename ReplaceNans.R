#' Title
#'
#' @param df
#' @param colName
#'
#' @return
#' @export
#'
#' @examples
replaceNansWithMedian <- function(df, colName) {
  colMedian <- median(df[[colName]], na.rm=TRUE)
  df[[colName]] <- df[[colName]] %>% replace_na(colMedian)
  return(df)
}

#' Title
#'
#' @param data
#' @param predictstatement
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
writeCSV <- function(data, predictstatement, filename) {
  fit2 <- data.frame(data$SK_ID_CURR = data$test$SK_ID_CURR, data$TARGET = predictstatement, check.rows = FALSE)
  write.csv(fit2, filename)
}


prepData <- function(df, test = FALSE) {

  if (test == TRUE) {
    df <- dplyr::select(df, SK_ID_CURR, OWN_CAR_AGE, DAYS_ID_PUBLISH, AMT_ANNUITY, DAYS_BIRTH, EXT_SOURCE_3, EXT_SOURCE_1, EXT_SOURCE_2, EXT_SOURCE_1, DAYS_BIRTH, AMT_INCOME_TOTAL, DAYS_LAST_PHONE_CHANGE, CODE_GENDER, AMT_CREDIT, AMT_GOODS_PRICE)
  }
  else {
    df <- dplyr::select(df, TARGET, OWN_CAR_AGE, DAYS_ID_PUBLISH, AMT_ANNUITY, DAYS_BIRTH, EXT_SOURCE_3, EXT_SOURCE_1, EXT_SOURCE_2, EXT_SOURCE_1, DAYS_BIRTH, AMT_INCOME_TOTAL, DAYS_LAST_PHONE_CHANGE, CODE_GENDER, AMT_CREDIT, AMT_GOODS_PRICE)
  }

  df <- replaceNansWithMedian(df, "OWN_CAR_AGE")
  df <- replaceNansWithMedian(df, "DAYS_ID_PUBLISH")
  df <- replaceNansWithMedian(df, "AMT_ANNUITY")
  df <- replaceNansWithMedian(df, "DAYS_BIRTH")
  df <- replaceNansWithMedian(df, "EXT_SOURCE_3")
  df <- replaceNansWithMedian(df, "EXT_SOURCE_1")
  df <- replaceNansWithMedian(df, "EXT_SOURCE_2")
  df <- replaceNansWithMedian(df, "AMT_INCOME_TOTAL")
  df <- replaceNansWithMedian(df, "DAYS_LAST_PHONE_CHANGE")
  df <- replaceNansWithMedian(df, "AMT_GOODS_PRICE")
  df <- replaceNansWithMedian(df, "AMT_CREDIT")

  return(df)
}
