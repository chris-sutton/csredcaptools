#' @import data.table
#' @export
copyUserRightsAndUpload <- function(source_vunetid="",target_vunetid="",
                                    uri="",token="") {
  user <- NULL
  user$error <- NULL
  source_userrights <- getUserRights(uri,token)
  source_userrights <- source_userrights[username %like% source_vunetid]

  if (!length(source_userrights$forms) == 0)  {
    tmp <- data.table::data.table(forms=stringr::str_split(source_userrights$forms,",")[[1]])
    tmp <- tmp[!forms %like% ":3"]
    source_userrights$forms <- paste0(tmp$forms,collapse = ",")
  }


  if (nrow(source_userrights) != 1) {
    user$error <- source_userrights
    print("Check that the source_vunetid exists in the project!")

  } else {
    user$data <- copyUserRights(source_userrights,target_vunetid)
    user$str_data <- converteUserRightsString(user$data)
    user$response <- uploadUserRights(user$str_data,uri,token)
  }
  return(user)
}

#' @export
copyUserRightsAndUploadTest <- function(source_vunetid="",target_vunetid="",
                                    uri="",token="") {
  user <- NULL
  source_userrights <- getUserRights(source_vunetid,uri,token)
  user$source_userrights <- source_userrights
    user$data <- copyUserRights(source_userrights,target_vunetid)
    user$str_data <- converteUserRightsString(user$data)
    #user$response <- uploadUserRights(user$str_data,uri,token)
  return(user)
}

#' @export
getUserRights <- function(uri="",token="") {
  formData <- list("token"=token,
                   content='user',
                   format='csv',
                   returnFormat='json'
  )
  response <- httr::POST(uri, body = formData, encode = "form")
  result <- httr::content(response, show_col_types = F)
  result <- data.table::as.data.table(result)
  return(result)
}

#' @export
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

#' @export
converteUserRightsString <- function(user) {
  fname <- "user.csv"
  data.table::fwrite(user,fname)
  user_str <- readChar(fname, file.info(fname)$size)
  unlink(fname)
  return(user_str)
}

#' @export
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
