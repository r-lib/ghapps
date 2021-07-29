#' Authenticate as an App
#'
#' Authenticates with the GitHub API on behalf of a GitHub app using the
#' app-id and private-key file that you get when registering the app. The
#' result is a temporary token that is valid for 1 hour, which you can use
#' just like other tokens to act on behalf of the app using e.g. [gh()].
#'
#' To register a new app go to:
#' [https://github.com/settings/apps/new](https://github.com/settings/apps/new)
#'
#' @export
#' @rdname gh_apps
#' @importFrom gh gh
#' @name GitHub Apps
#' @param installation the target app-installation to authenticate with, This must
#' either be a user / organization name such as `"ropensci"`, or a repository that
#' has the app installed for example `"ropensci/magick"`.
#' @param app_id a string with the github app id
#' @param app_key file or string with your private key, passed to [openssl::read_key]
#' @return a temporary token that will be valid for 1 hour
#' @examples \donttest{
#' Sys.setenv('GH_APP_ID' = 87942)
#' Sys.setenv('GH_APP_KEY' = "~/Downloads/r-universe.2020-11-25.private-key.pem")
#' gh_app_installation_count()
#' gh_app_installation_list()
#' gh_app_installation_info('jeroen')
#' gh_app_token('jeroen')
#' }
gh_app_token <- function(installation, app_id = Sys.getenv('GH_APP_ID'),
                         app_key = Sys.getenv('GH_APP_KEY')){
  if(!nchar(app_id) || !nchar(app_key))
    stop("No app_id or app_key found")
  jwt <- gh_app_jwt(app_id = app_id, app_key = app_key)
  endpoint <- if(grepl("/", installation)){
    sprintf('/repos/%s/installation', installation)
  } else {
    sprintf('/users/%s/installation', installation)
  }
  installation_id <- gh_as_app(endpoint, jwt = jwt)$id
  endpoint <- sprintf('/app/installations/%d/access_tokens', installation_id)
  gh_as_app(endpoint, jwt = jwt, .method = 'POST')$token
}

#' @export
#' @rdname gh_apps
gh_app_installation_count <- function(app_id = Sys.getenv('GH_APP_ID'), app_key = Sys.getenv('GH_APP_KEY')){
  if(!nchar(app_id) || !nchar(app_key))
    stop("No app_id or app_key found")
  jwt <- gh_app_jwt(app_id = app_id, app_key = app_key)
  gh_as_app('/app', jwt = jwt)$installations_count
}

#' @export
#' @rdname gh_apps
gh_app_installation_list <- function(app_id = Sys.getenv('GH_APP_ID'),
                                     app_key = Sys.getenv('GH_APP_KEY')){
  if(!nchar(app_id) || !nchar(app_key))
    stop("No app_id or app_key found")
  jwt <- gh_app_jwt(app_id = app_id, app_key = app_key)
  gh_as_app('/app/installations', jwt = jwt, per_page = 100, .limit = 1e6)
}

#' @export
#' @rdname gh_apps
gh_app_installation_info <- function(installation, app_id = Sys.getenv('GH_APP_ID'),
                                     app_key = Sys.getenv('GH_APP_KEY')){
  if(!nchar(app_id) || !nchar(app_key))
    stop("No app_id or app_key found")
  jwt <- gh_app_jwt(app_id = app_id, app_key = app_key)
  endpoint <- if(grepl("/", installation)){
    sprintf('/repos/%s/installation', installation)
  } else {
    sprintf('/users/%s/installation', installation)
  }
  gh_as_app(endpoint, jwt = jwt)
}

#' @export
#' @rdname gh_apps
gh_app_installation_delete <- function(installation, app_id = Sys.getenv('GH_APP_ID'),
                                     app_key = Sys.getenv('GH_APP_KEY')){
  installation_info <- gh_app_installation_info(installation = installation,
                                              app_id = app_id, app_key = app_key)
  endpoint <- paste0('/app/installations/', installation_info$id)
  jwt <- gh_app_jwt(app_id = app_id, app_key = app_key)
  gh_as_app(endpoint, jwt = jwt, .method = 'DELETE')
}

#' @export
#' @rdname gh_apps
#' @param name slug of the github app from `https://github.com/apps/{name}`
gh_app_info <- function(name = 'r-universe'){
  gh::gh(paste0('/apps/', name))
}

gh_as_app <- function(endpoint, jwt, ...){
  jwt_auth <- c(Authorization = paste('Bearer', jwt))
  gh(endpoint, ..., .token = "", .send_headers = jwt_auth)
}

gh_app_jwt <- function(app_id, app_key){
  payload <- jose::jwt_claim(exp = unclass(Sys.time()) + 300, iss = app_id)
  jose::jwt_encode_sig(payload, app_key)
}
