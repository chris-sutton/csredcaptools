#' @export
rc_metadata_read <- function(url,token) {
  formData <- list("token"=token,
                   content='metadata',
                   format='csv',
                   returnFormat='json'
  )
  response <- httr::POST(url, body = formData, encode = "form")
  result <- httr::content(response, type = "text/csv", as="parsed")
  return(result)
}
