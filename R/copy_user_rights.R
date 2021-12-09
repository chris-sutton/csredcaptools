

copyUserRightsAndUpload <- function(source_vunetid="",target_vunetid="",
                                    uri="",token="") {
  user <- NULL
  source_userrights <- getUserRights(target_vunetid,uri,token)
  if (nrow(source_userrights) != 1) {
    print("More than one user, this is impossible! ERROR!")

  } else {
    user$data <- copyUserRights(source_userrights,target_vunetid)
    user$str_data <- converteUserRightsString(user$data)
    user$response <- uploadUserRights(user$str_data,uri,token)
  }
  return(user)
}

getUserRights <- function(vunetid="",uri="",token="") {
  formData <- list("token"=token,
                   content='user',
                   format='csv',
                   returnFormat='json'
  )
  response <- httr::POST(uri, body = formData, encode = "form")
  result <- httr::content(response)
  result <- data.table::as.data.table(result)
  result[username==vunetid]
}


copyUserRights <- function(template,vunetid="") {
  fields <- c('username','expiration','data_access_group','design','user_rights',
              'data_access_groups','data_export','reports','stats_and_charts',
              'manage_survey_participants','calendar','data_import_tool',
              'data_comparison_tool','logging','file_repository',
              'data_quality_create','data_quality_execute','api_export',
              'api_import','mobile_app','mobile_app_download_data','record_create',
              'record_rename','record_delete','lock_records_customization',
              'lock_records','lock_records_all_forms','forms')

  template[[1,"username"]] <- vunetid
  return(template[,..fields])
}


converteUserRightsString <- function(user) {
  fname <- "user.csv"
  data.table::fwrite(user,fname)
  user_str <- readChar(fname, file.info(fname)$size)
  unlink(fname)
  return(user_str)
}


uploadUserRights <- function(users="", uri="",token="") {
  formData <- list("token"=token,
                   content='user',
                   format='csv',
                   data=users,
                   returnFormat='json'
  )
  response <- httr::POST(uri, body = formData, encode = "form")
  return(response)
}
