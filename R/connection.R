

setClass("smartConnect",
         representation(
           api_key = "character",
           refresh_token = "character",
           feed_token = "character",
           user_id ="character",
           access_token = "character",
           debug = "logical",
           micro_cache = "logical",
           session_hook = "function",
           timeout = "numeric",
           proxies = "character",
           root = "character",
           login = "character",
           routes = "list",
           details = "list",
           publicip="character",
           localip="character",
           macaddress="character",
           accept="character",
           usertype="character",
           sourceid="character",
           privatekey="character"

         )
         )
is_connection_obj <-function(object){
  return(class(object) == "smartConnect")
}

is_valid_api_key <- function(object){
  if(!is_connection_obj(object))return(FALSE)
  return(length(object@api_key) > 0)
}
is_valid_refresh_token <- function(object){
  if(!is_connection_obj(object))return(FALSE)
  return(length(object@refresh_token) > 0)
}
is_valid_access_token <- function(object){
  if(!is_connection_obj(object))return(FALSE)
  return(length(object@access_token) > 0)
}
is_valid_session_hook <- function(object){
  if(!is_connection_obj(object))return(FALSE)
  return(deparse(object@session_hook)[2]=="NULL")
}
is_valid_root <- function(object){
  if(!is_connection_obj(object))return(FALSE)

  return(length(object@root) > 0)
}
is_valid_login <- function(object){
  if(!is_connection_obj(object))return(FALSE)
  return(length(object@login) > 0)
}
is_valid_routes <- function(object){
  if(!is_connection_obj(object))return(FALSE)

  return(length(object@routes) > 0)
}
is_valid_details <- function(object){
  if(!is_connection_obj(object))return(FALSE)
  return(length(object@details) > 0)
}
is_valid_timeout <- function(object){
  if(!is_connection_obj(object))return(FALSE)
  return(length(object@timeout) > 0)
}
is_valid_connection <- function(object){
  if(!is_connection_obj(object))return(FALSE)

  return(
    is_valid_api_key(object) &
      is_valid_root(object) &
      is_valid_routes(object)
  )
}
is_logged_connection <- function(object){

  if(!is_connection_obj(object))return(FALSE)
  return(
    is_valid_connection(object) &
      is_valid_refresh_token(object)
  )
}

is_api_connected <- function(object){

  if(!is_connection_obj(object))return(FALSE)
  return(
    is_logged_connection(object) &
      is_valid_access_token(object)
  )
}

#'Function to set the refresh token in the smart connect object.
#'@description Function to set refresh token (obtained from a successful login flow) in the smart connect object.
#'@param object an S4 object of type smart connect.
#'@param refresh_token a string containing the refresh token from login flow.
#'@details  This function allows to set the refresh token of the smart
#'connect object. The refresh token is obtained from a successful login flow.
#'After the login flow, the user is redirected to a pre-specified URL. The
#'refresh token is posted alongwith as a query parameter.
#'@return Returns the object with refresh token field set.
#'@export

set_refresh_token <- function(object,refresh_token){
  if(!is_connection_obj(object)){
    message("Invalid smart connect object")
  }
  if(any(class(refresh_token)=="character")){
    object@refresh_token <- refresh_token
    message(object@refresh_token)
  }else{
    message("Invalid refresh token")
  }
  return(object)
}

#'Function to set the access token in the smart connect object.
#'@description This function sets the access token inside the smart connect object.
#'@param object a smart connect object.
#'@param access_token Access token.
#'@return Smart connect object with access token set.
#'@export

set_access_token <- function(object,access_token){
  if(any(class(access_token)=="character")){
    object@access_token <- access_token
  }else{
    message("Invalid access token")
  }
  return(object)
}

#'Function to set the user_id in the smart connect object.
#'@description This function sets the user_id inside the smart connect object.
#'@param object a smart connect object.
#'@param user_id Access token.
#'@return Smart connect object with user_id set.
#'@export

set_user_id<-function(object,user_id){
  if(!is_connection_obj(object)){
    message("Invalid smart connect object")
  }
  if(any(class(user_id)=="character")){
    object@user_id <- user_id
  }else{
    message("Invalid user_id")
  }
  return(object)
}

#'Function to set the feed token in the smart connect object.
#'@description This function sets the feed token inside the smart connect object.
#'@param object a smart connect object.
#'@param feed_token Feed token.
#'@return Smart connect object with feed token set.
#'@export

set_feed_token<-function(object,feed_token){
  if(!is_connection_obj(object)){
    message("Invalid smart connect object")
  }
  if(any(class(feed_token)=="character")){
    object@feed_token <- feed_token
  }else{
    message("Invalid feed_token")
  }
  return(object)
}

set_session_hook <- function(object,f){
  if(!is_connection_obj(object)){
    stop(InvalidConnectionObjectException)
  }
  if(any(class(f)=="function")){
    object@session_hook <- f
  }else{
    stop(InvalidInputException)
  }
  return(object)
}
set_proxies <- function(object, proxies){
  if(!is_connection_obj(object)){
    stop(InvalidConnectionObjectException)
  }
  if(any(class(proxies)=="character")){
    object@proxies <- proxies
  }else{
    stop(InvalidInputException)
  }
  return(object)
}

set_timeout <- function(object, timeout){
  if(!is_connection_obj(object)){
    stop(InvalidConnectionObjectException)
  }
  if(any(class(timeout)=="numeric")){
    object@timeout <- round(timeout)
  }else{
    stop(InvalidInputException)
  }
  return(object)
}
set_debug <- function(object,debug){
  if(!is_connection_obj(object)){
    stop(InvalidConnectionObjectException)
  }
  if(any(class(debug)=="logical")){
    object@debug <- debug
  }else{
    stop(InvalidInputException)
  }
  return(object)
}
set_micro_cache <- function(object, micro_cache){
  if(!is_connection_obj(object)){
    stop(InvalidConnectionObjectException)
  }
  if(any(class(micro_cache)=="logical")){
    object@micro_cache <- micro_cache
  }else{
    stop(InvalidInputException)
  }
  return(object)
}
set_details <- function(object, data){
  if(!is_connection_obj(object)){
    stop(InvalidConnectionObjectException)
  }
  if(class(data)=="list"){
    object@details <- data
  }else{
    stop(InvalidInputException)
  }
  return(object)
}



get_api_endpoint <- function(object,endpoint){
  if(!is_valid_connection(object)){
    message("Invalid Smart connect object")
    return(NULL)
  }
  url <- paste0(object@root,glue::glue(object@routes[[endpoint]] ))
  return(url)
}

#'Function to get the feed_token.
#'@description Function to get the feed_token from
#'the smart object.
#'@param object An object of smart connect type.
#'@return Returns the feed_token.
#'@export
#'
get_feed_token<-function(object){
  if(!is_valid_connection(object)){
    message("Invalid smart connect object")
    return(NULL)
  }
  return(object@feed_token)
}

#'Function to get the login URL.
#'@description Function to get the login URL (the login entry + api_key) from
#'the smart object.
#'@param object An object of smart connect type.
#'@return Returns the login URL.
#'@export

get_login_url <- function(object){
  if(!is_valid_connection(object)){
    message("Invalid smart connect object")
    return(NULL)
  }
  return(paste0(object@login,"?&api_key=",object@api_key))
}

#'Create a smart connect connection object.
#'@description This function returns an object of smart connect type, given the
#'input parametrs.
#'@param params a list mapping object property name to values. See details
#'for more.
#'@details This function creates an S4 object from the input parameters. The
#'required parameters are api_key, api_secret. If root (the api root), routes
#'(the list of api endpoints) and login (the login url) is supplied, they are
#'used. Otherwise the internally defined values are used. Only a single
#'instance of this class per `api_key` should be initialized.
#'Parameters should be a named list of all inputs to create a connection.
#'The important attributes are: api_key( character) - your api key;
#'api_secret(character) - your api secret (both of these are available on
#'the developer page in your smart app, and must be supplied during object
#'creation); refresh_token(character) - should be set after a successful
#'login flow; access_token(character) - obtained using fetch_access_token
#'function (which also set the attribute `details`). Some attributes like
#'root (api root URL), login (smart api specific login URL) and routes (list
#'of api end points) are optional and recommended to use the default values.
#'Other attributes which are not implemented are: debug(logical); micro_cache
#'(logical); session_hook(function); timeout(numeric) and proxies(character).
#'@return S4 object for smart connect which can be used in further api calls.
#'@export

create_connection_object <- function(params){
  object = methods::new("smartConnect")

  tryCatch({
    object@api_key = params[["api_key"]]
    object@root = ifelse(is.null(params[["root"]]),api_root,
                         params[["root"]])
    if(is.null(params[["routes"]])){
      object@routes = api_routes
    } else{
      object@routes = params[["routes"]]
    }
    object@login = ifelse(is.null(params[["login"]]),api_login_url,
                          params[["login"]])
  }, error=function(e){
    message("in error function",e$message)
    return(NULL)
  })

  if(!is_valid_connection(object)){
    message("Failed to create smart connect object")
    return(NULL)
  }

  object@debug <- ifelse(is.null(params[["debug"]]),FALSE,params[["debug"]])
  object@micro_cache <- ifelse(is.null(params[["micro_cache"]]),FALSE,params[["micro_cache"]])
  object@timeout <- ifelse(is.null(params[["timeout"]]),7,params[["timeout"]])
  if(!is.null(params[["proxies"]])){
    object@proxies <- params[["proxies"]]
  }
  object@publicip<-gsub(".*? ([[:digit:]])", "\\1", system("ipconfig", intern=T)[grep("IPv4", system("ipconfig", intern = T))])
  object@localip<-tryCatch({
    gsub(".*? ([[:digit:]])", "\\1", system("ipconfig", intern=T)[grep("IPv4", system("ipconfig", intern = T))])
  },
  warning=function(war){
    print(paste("MY WARNING: ", war))
  },
  error = function(err)
  {
    print(paste("MY ERROR: ", err))
  },
  finally = function(f)
  {
    print(paste("127.0.0.0"))
  })
  ipconfig <- system("ipconfig", intern=TRUE)
  ipv6 <- ipconfig[grep("IPv6", ipconfig)]
  object@macaddress <- gsub(".*:? ([[:alnum:]])", "\\1", ipv6)
  object@accept <- "application/json"
  object@usertype <- "USER"
  object@sourceid <-"WEB"

  return(object)
}

#'Function to start an api session.
#'@description This function is used to login the session.
#'@param object of type smart connect, must have a valid client code and password set.
#'@param clientCode valid client id of the user is must.
#'@param password valid password against the user/client id is must.
#'@details This function sends a request to smart api to login the session.
#'@return the object data with access token and refresh token.
#'@export

generate_session<-function(object,clientCode,password){
  method_params = list("clientcode"=clientCode,"password"=password)
  r <- NULL
  tryCatch({
    message("inside try catch")
    r<-rest_api_call(object,"POST","api.login",method_params)
    message(r)
    r<-httr::content(r)

  },error=function(e){
    message("Error ",e$message)
    stop(InvalidAccessTokenException)
  })

  access_token=r$data$jwtToken
  refresh_token=r$data$refreshToken
  feed_token=r$data$feedToken


  object=set_access_token(object,access_token)
  object=set_refresh_token(object,refresh_token)
  object=set_feed_token(object,feed_token)
  tryCatch({
    user<- get_profile(object,refresh_token)
    user_id=user$data[["clientcode"]]
    object=set_user_id(object,user_id)
  })
  user$data$jwtToken=paste0(object@access_token)
  user$data$refreshToken=object@refresh_token
  object=set_details(object,user)
  return(object)
}

#'Fetch access token given the refresh token.
#'@description This function returns an access token given the inputs, that
#'can be used for subsequent api calls.
#'@param object An object of type smart connect, must have refresh token set.
#'@param refresh_token refresh token is passed.
#'@usage generate_token(object,refresh_token)
#'@details This function generate the `access_token` by exchanging
#'`refresh_token`.The `refresh_token` obtained after the login flow. the
#'`access_token` required for all subsequent requests. The object passed in
#'this call must already have `refresh_token` set. A successful call also set the user data within the object.
#'@return A string containing the access token.
#'@export
generate_token<-function(object,refresh_token){
  message(refresh_token)
  method_params=list("refreshToken"=refresh_token)
  r <- NULL
  tryCatch({
    r<-rest_api_call(object,"POST","api.token",method_params)
    message(r)
    r<-httr::content(r)

  },error=function(e){
    message(e$message)
    stop(InvalidRefreshTokenException)
  })
  access_token <- r$data$jwtToken
  refresh_token<-r$data$refreshToken
  feed_token<-r$data$feedToken
  object = set_access_token(object,access_token)
  object=set_feed_token(object,feed_token)
  object=set_refresh_token(object,refresh_token)
  object = set_details(object,r$data)
  return(object)
}

renew_access_token <-function(object){
  if(!is_logged_connection(object)){
    stop(InvalidConnectionObjectorTokenException)
  }
  method_params=list(
    "jwtToken"= object@access_token,
    "refreshToken"= object@refresh_token
  )
  r <- NULL
  tryCatch({
    r <- rest_api_call(object,"POST","api.refresh",method_params)
    r<-httr::content(r)
    access_token <- r$data$jwtToken
    refresh_token<-r$data$refreshToken
    feed_token<-r$data$feedToken
  }, error=function(e){
    message(e$message)
    stop(InvalidAccessTokenException)
  })
  if(class(access_token)!="character"){
    stop(InvalidAccessTokenException)
  }
  object = set_access_token(object,access_token)
  object=set_feed_token(object,feed_token)
  object=set_refresh_token(object,refresh_token)
  object = set_details(object,r$data)
  return(object)
}

#'Function to end an api session.
#'@description This function is used to logout the session.
#'@param object of type smart connect, must have a valid client code.
#'@param user_id it is the client id of the user
#'@details This function sends a request to smart api to logout the session.
#'@return An object of type smart connect.
#'@export

terminate_session=function(object,user_id){

  if(!is_logged_connection(object)){
    stop(InvalidConnectionObjectorTokenException)
  }
  method_params=list("clientcode"=user_id)
  r <- NULL
  tryCatch({
    r<-rest_api_call(object,"POST","api.logout",method_params)

    r<-httr::content(r)

  })
  object=set_details(object,r)
  return(object)
}




