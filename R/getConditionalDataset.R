#' @export
getConditionalDataset <- function(df, condition=NA, variableCondition=NA) {
  condDf = df
  secondCondition = TRUE
  dfName = deparse(substitute(df))
  finalCondition = ""

  if (!is.na(condition)) {
    firstCondition = gsub("\\$",paste(dfName,"$",sep=""),condition)
    firstCondition = paste("(", firstCondition, ")", sep="")
    finalCondition = firstCondition
  }
  if (!is.na(variableCondition)) {
    secondCondition = gsub("\\$",paste(dfName,"$",sep=""),variableCondition)
    secondCondition = paste("(", secondCondition, ")", sep="")
    if (!is.na(condition)) {
      finalCondition = paste(firstCondition, secondCondition, sep=" & ")
    } else {
      finalCondition = secondCondition


    }
  }
  # print(finalCondition) # for debugging purposes remove later
  if (!is.na(condition) | !is.na(variableCondition)) {
    condDf = with(df,df[which(eval(parse(text = finalCondition))) ,])
    # using `with` and `which` in this way, prevents conditions that produce
    # an NA results from being in the data frame.
  }

  return (condDf)
}
