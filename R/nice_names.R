#' @export
nice_names <- function(data_dictionary) {
  betternames <- c(
    'variablename',
    'formname',
    'sectionheader',
    'fieldtype',
    'fieldlabel',
    'choices_calc_silderlabels',
    'fieldnote',
    'textvalidation_slidernumber',
    'textvalidationmin',
    'textvalidationmax',
    'identifier?',
    'branchinglogic',
    'requiredfield?',
    'customalignment',
    'questionnumber_surveyonly',
    'matrixgroupname',
    'matrixranking?',
    'fieldannotation'
  )
  names(data_dictionary) <- betternames
  return(data_dictionary)
}
