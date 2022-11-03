#' Authenticate as a GitHub App
#'
#' GitHub *apps* provide a powerful way to manage fine grained programmatic access
#' to specific git repositories, without having to create dummy users, and which
#' are safer than PATs for automated tasks. This package extends [gh] to let you
#' authenticate and interact with the GitHub API in R on behalf of an app.
#'
#' Instead of authenticating as a user, you can also authenticate with the GitHub
#' API as a "GitHub app". An app is a first class actor within GitHub. This means
#' it has its own permissions to specific repositories, without being tied to any
#' particular user account. In fact, you could think of an app as a special type
#' of dummy user with a specific purpose. Don't be too intimidated by the word
#' 'app', it is mainly an authentication concept. Any program that authenticates
#' with GitHub, such as an R script, can be considered an app.
#'
#' ## Creating a GitHub app
#'
#' To register a new app go to:
#' [https://github.com/settings/apps/new](https://github.com/settings/apps/new).
#' You may register as many apps as you like, with different permissions, for
#' different purposes.
#'
#' You can choose if the the app is public (to allow others to give the app access
#' to their repo by "installing" the app) or if the app is only for your own
#' account. The latter is basically a safe way to provide limited access to a
#' single repository, without exposing your user account in any way.
#'
#' ## Acting on behalf of an app in R
#'
#' We can interact with [most of the GitHub API](https://docs.github.com/en/rest/overview/endpoints-available-for-github-apps)
#' on behalf of an app in exactly the same way as a regular user. The only
#' difference is that instead of a personal access token (PAT) we generate a
#' temporary app token, which looks very similar. From here we use [gh()] in
#' exactly the same way as usual.
#'
#' Authentication on behalf of an app is a two step process. First you need to
#' generate a so-called JWT with [gh_app_jwt()] using the App-ID and a RSA key file
#' that you can retrieve on GitHub in the app settings. This JWT is only valid for
#' 5 minutes, and is used to generate app tokens for specific target repositories
#' with [gh_app_token()]. This app token works the same as a PAT, but it is valid
#' for 1 hour and has permission only to the repository that you specified as the
#' `installations` parameter in [gh_app_token()]. From here you can use [gh()] to
#' perform all the operations that the git repository has given your app permission
#' for.
#'
#' ## Using this in CI
#'
#' A common use case is to authenticate as a github-app inside a github action script,
#' in order to run tasks that require authentication, without the need to use someones
#' personal credentials. To make this easy, you can specify the app-id and private key
#' via environment variables `GH_APP_ID` and `GH_APP_KEY` which you can expose as a
#' 'secret' in the CI. The `GH_APP_KEY` can either be a file path, or simply the
#' verbatim content of the private key (pem) file.
#'
#' @export
#' @name ghapps
#' @rdname ghapps
#' @param jwt a jwt credential object generated with [gh_app_jwt()]
gh_app_info <- function(jwt = gh_app_jwt()){
  gh_as_app('/app', jwt = jwt)
}

#' @export
#' @rdname ghapps
#' @importFrom gh gh
#' @param installation the target repositorie(s) which we want to access.
#' Either a user / organization name such as `"ropensci"`, or a specific repository
#' that has the app installed for example `"ropensci/magick"`.
#' @return `gh_app_token` returns a temporary token that will be valid for 1 hour
#' that you use instead of a PAT.
#' @examples
#' # Authenitcate and show some metadata about our demo-app
#' gh_app_info(jwt = gh_demo_jwt())
#' gh_app_installation_list(jwt = gh_demo_jwt())
#' \donttest{
#' # This requires that user 'testingjerry' has our app installed:
#' gh_app_installation_info('testingjerry', jwt = gh_demo_jwt())
#' token <- gh_app_token('testingjerry', jwt = gh_demo_jwt())
#'
#' # Use the token in gh() to do things on behalf of the app
#' res <- gh::gh('/users/testingjerry', .token = token)
#' }
gh_app_token <- function(installation, jwt = gh_app_jwt()){
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
#' @rdname ghapps
gh_app_installation_count <- function(jwt = gh_app_jwt()){
  gh_app_info(jwt = jwt)$installations_count
}

#' @export
#' @rdname ghapps
gh_app_installation_list <- function(jwt = gh_app_jwt()){
  gh_as_app('/app/installations', jwt = jwt, per_page = 100, .limit = 1e6)
}

#' @export
#' @rdname ghapps
gh_app_installation_info <- function(installation, jwt = gh_app_jwt()){
  endpoint <- if(grepl("/", installation)){
    sprintf('/repos/%s/installation', installation)
  } else {
    sprintf('/users/%s/installation', installation)
  }
  gh_as_app(endpoint, jwt = jwt)
}

#' @export
#' @rdname ghapps
gh_app_installation_delete <- function(installation, jwt = gh_app_jwt()){
  installation_info <- gh_app_installation_info(installation = installation, jwt = jwt)
  endpoint <- paste0('/app/installations/', installation_info$id)
  gh_as_app(endpoint, jwt = jwt, .method = 'DELETE')
}

gh_as_app <- function(endpoint, jwt, ...){
  jwt_auth <- c(Authorization = paste('Bearer', jwt))
  gh(endpoint, ..., .token = "", .send_headers = jwt_auth)
}

#' @export
#' @rdname ghapps
#' @param name slug of the github app from `https://github.com/apps/{name}`
gh_app_public_info <- function(name = 'r-universe'){
  gh::gh(paste0('/apps/', name))
}

#' @export
#' @rdname ghapps
#' @param app_id a string with the github app id
#' @param app_key file path or string with your private key, passed to [openssl::read_key].
#' or a key object returned by [openssl::read_key]
gh_app_jwt <- function(app_id = Sys.getenv('GH_APP_ID'), app_key = Sys.getenv('GH_APP_KEY')){
  if(is_empty(app_id))
    stop("No app_id is set")
  if(is_empty(app_key))
    stop("No app_key is set")
  expires <- Sys.time() + 300
  payload <- jose::jwt_claim(exp = unclass(expires), iss = app_id)
  structure(jose::jwt_encode_sig(payload, app_key), app_id = app_id, expires = expires)
}

#' @export
#' @rdname ghapps
gh_demo_jwt <- function(){
  keyfile <- system.file('keys/demo-app-for-r.2022-10-31.private-key.pem', package = 'ghapps')
  app_key <- openssl::read_key(keyfile, password = 'test')
  gh_app_jwt(app_id = '256536', app_key = app_key)
}

is_empty <- function(x){
  !length(x) || (is.character(x) && !nchar(x))
}
