#' @export
branchinglogicconvert <- function(dict) {
  dict <- nice_names(dict)
  dict$branchinglogic[ dict$branchinglogic %in% "0" | dict$branchinglogic %in% "" ] = NA
  dict$branchinglogic = gsub("\\[", "$", dict$branchinglogic)
  dict$branchinglogic = gsub("\\)\\]", "", dict$branchinglogic)
  dict$branchinglogic = gsub("\\]", "", dict$branchinglogic)
  dict$branchinglogic = gsub("\\(([0-9]+)", "___\\1", dict$branchinglogic)
  dict$branchinglogic = gsub("\\'([0-9]+)\\'", '\\1', dict$branchinglogic)
  dict$branchinglogic = gsub('\\"([0-9]+)\\"', '\\1', dict$branchinglogic)
  dict$branchinglogic = lapply(dict$branchinglogic, notConditionMap,pattern="\\$[A-Za-z0-9\\s\\r\\n]*<>")
  dict$branchinglogic = lapply(dict$branchinglogic, notConditionMap,pattern="\\$[A-Za-z0-9\\s\\r\\n]*!=")
  dict$branchinglogic = gsub("<>", " %in% ", dict$branchinglogic)
  dict$branchinglogic = gsub("!=", " %in% ", dict$branchinglogic)
  dict$branchinglogic = gsub("(?<![><])\\=", " %in% ", dict$branchinglogic, perl=TRUE)
  dict$branchinglogic = gsub(" and ", " & ", dict$branchinglogic)
  dict$branchinglogic = gsub(" or | OR ", " | ", dict$branchinglogic)
  return(dict)

}

notConditionMap <- function(text, pattern) {
  returnText = stringr::str_replace(text, pattern, paste("!",stringr::str_extract(text,pattern),sep=""))
  return (returnText)
}
