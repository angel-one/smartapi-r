
#library(ipify)
options(scipen=999)

rest_api_call <- function(object,method,endpoint,method_params){
  message(endpoint)
  if(!method=="POST"){
    if(!is_api_connected(object)){
      stop(NotConnectedToAPIException)
    }
  }
  url <- get_api_endpoint(object,endpoint)

  privateKey=object@api_key

  if(endpoint=="api.login"){
    authorization=''
  }else{
    authorization=paste0("Bearer ",object@access_token)

  }


  r <- NULL

  method <- tolower(method)

  tryCatch({
    if(method=="get"){
      r <- httr::GET(url,body = method_params,
                     httr::add_headers("Content-type"=object@accept,
                                       "X-ClientLocalIP"=object@localip,
                                       "X-ClientPublicIP"=object@publicip,
                                       "X-MACAddress"=object@macaddress,
                                       "Accept"=object@accept,
                                       "X-PrivateKey"=privateKey,
                                       "X-UserType"=object@usertype,
                                       "X-SourceID"=object@sourceid,
                                       "Authorization"=authorization))
    } else if(method=="post"){
      r <- httr::POST(url,body = method_params, encode = "json",
                      httr::add_headers("Content-type"=object@accept,
                                        "X-ClientLocalIP"=object@localip,
                                        "X-ClientPublicIP"=object@publicip,
                                        "X-MACAddress"=object@macaddress,
                                        "Accept"=object@accept,
                                        "X-PrivateKey"=privateKey,
                                        "X-UserType"=object@usertype,
                                        "X-SourceID"=object@sourceid,
                                        "Authorization"=authorization))

    } else if(method=="put"){
      r <- httr::PUT(url,body = method_params, encode = "json",
                     httr::add_headers("Content-type"=object@accept,
                                       "X-ClientLocalIP"=object@localip,
                                       "X-ClientPublicIP"=object@publicip,
                                       "X-MACAddress"=object@macaddress,
                                       "Accept"=object@accept,
                                       "X-PrivateKey"=privateKey,
                                       "X-UserType"=object@usertype,
                                       "X-SourceID"=object@sourceid,
                                       "Authorization"=authorization))
    } else if(method=="delete"){
      r <- httr::DELETE(url,body = method_params, encode = "json",
                        httr::add_headers("Content-type"=object@accept,
                                          "X-ClientLocalIP"=object@localip,
                                          "X-ClientPublicIP"=object@publicip,
                                          "X-MACAddress"=object@macaddress,
                                          "Accept"=object@accept,
                                          "X-PrivateKey"=privateKey,
                                          "X-UserType"=object@usertype,
                                          "X-SourceID"=object@sourceid,
                                           "Authorization"=authorization))
    } else{
      stop(UnknwonHttpException)
    }
  }, error=function(e){
    message("ERROR=",e$message)
    stop(HttpException)

    if(r$status_code != 200){
      r <- httr::content(r)
      message(paste0(r$error_type,": ",r$message))
      return(NULL)
    }

    if(!(r$headers$`content-type` %in% VALID_CONTENT_TYPE)){
      stop(DataException)
    }

    r <- suppressMessages(httr::content(r))

    if(any(class(r)=="data.frame") && NROW(r)>0){
      return(r)
    }

    if(any(class(r)=="data.frame") && NROW(r)<1){
      stop(NoDataException)
    }

    if(r$status != STATUS_OK){
      stop(GeneralException)
    }

    if(length(r$data)>0){

      if(smart_error(toString(r$data[[1]]))){
        stop(SmartException)
      }
    }

    if(length(r$data)==0){
      stop(NoDataException)
    }

    return(r)
  })

}

#'Function to get the details of logged on user.
#'@description Gets the details of the user.
#'@usage get_profile(object,refresh_token)
#'@param object An object of type smart connect with valid refresh_token.
#'@param refresh_token refresh_token needs to be passed
#'@return Returns a list with user details, if successful.
#'@export

get_profile <- function(object,refresh_token){
  method_params=list("refreshToken"=refresh_token)
  r <- NULL
  tryCatch({
    r<-rest_api_call(object,"GET","api.user.profile",method_params)
    r<-httr::content(r)

  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r))r <- r
  return(r)

}
#'Function to place a trade.
#'@description Function to place a trade with specified details.
#'@param object An object of type smart connect.
#'@param variety Order variety, e.g. regular, bo, co or amo.
#'@param tradingsymbol Trading Symbol of the instrument.
#'@param symboltoken Symbol token
#'@param transactiontype BUY or SELL.
#'@param exchange Exchange for the order, e.g. NSE, NFO, CDS etc.
#'@param ordertype Order type, e.g. MARKET, LIMIT, SL or SL-M.
#'@param producttype Product type, e.g. MIS, NRML, CNC, BO or CO.
#'@param duration Duration
#'@param price Price if required (e.g. for limit orders).
#'@param squareoff Price difference for profit booking (bracker orders).
#'@param stoploss Price difference for loss booking (bracket orders).
#'@param quantity Quantity of the order.
#'@details This function sends an order with input details for execution.
#'Please note not all parameters are relevant for all types of orders. This
#'function (as with rest of the package) does no error checks and post the
#'trade request as is. It is left to the function calling this method to
#'carry out necessary error checks. If the order is successfully posted, an
#'order ID will be returned. A successfully posted order does not mean it is
#'a valid order, and orders with erroneous user input can be immediately
#'cancelled. Therefore it is good practice to check the order details once
#'an order ID is received from this call.
#'@return Returns an order ID (string), if successful.
#'@export

place_order<-function(object,param){
  params <- as.list(environment(), all=TRUE)

  params[["object"]] <- NULL

  keys <- names(params[["param"]])

  for(key in keys){
    if(is.null(params[[key]])){
      params[[key]] <- NULL
    }
  }

  method_params = params

  r <- NULL

  tryCatch({r <- rest_api_call(object,"POST","api.order.place",method_params)
  r<-httr::content(r)
  message(r)
  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r)) r <- r$data[["orderid"]]
  return(r)

}
#'Function to modify an exiting order.
#'@description Function to modify an exiting order with specified details.
#'@param object An object of type smart connect.
#'@param variety Order variety, e.g. regular, bo, co or amo.
#'@param orderid Order ID for the order to modify.
#'@param ordertype Order type, e.g. MARKET, LIMIT, SL or SL-M.
#'@param duration Duration
#'@param producttype Product type, e.g. MIS, NRML, CNC, BO or CO
#'@param price New Price (e.g. for limit orders).
#'@param quantity New quantity.
#'@param tradingsymbol Trading Symbol of the instrument.
#'@param symboltoken Symbol token
#'@param exchange Exchange for the order, e.g. NSE, NFO, CDS etc.
#'@details This function sends a modification request for an existing order,
#'assuming it is not already executed. Please note: not all parameters are
#'relevant for all types of orders. No error checks is carried out, It is
#'left to the function calling this method to carry out necessary error
#'checks.If successful, this will return the order ID (should be same as the
#'one passed). A successfully placed request does not mean it is
#'a valid order, and orders with erroneous user input can be immediately
#'canceled. Therefore it is good practice to check the order details once
#'an order ID is received from this call.
#'@return Returns an order ID (string), if successful.
#'@export

modify_order<-function(object,param){
  params <- as.list(environment(), all=TRUE)
  params[["object"]] <- NULL
  keys <- names(params[["param"]])

  for(key in keys){
    if(is.null(params[[key]])){
      params[[key]] <- NULL
    }
  }

  method_params = params
  r <- NULL

  tryCatch({
    r<-rest_api_call(object,"POST","api.order.modify",method_params)
    r<-httr::content(r)
  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r)) r <- r$data[["orderid"]]
  return(r)

}

#' Function to cancel an exiting order.
#'@description Function to cancel an exiting order with specified ID.
#'@param object An object of type Smart connect.
#'@param variety Order variety, e.g. regular, bo, co or amo.
#'@param orderid Order ID for the order to modify.
#'@details This places a request to cancel an existing order, if it is not
#'already executed. Please note: no error checks are done for the input
#'parameters. Sanity check is left to the user of this function. If
#'successful, this will return the order ID (should be same as the
#'one passed). A successfully placed request does not mean it is
#'a valid order, and orders with erroneous user input can be immediately
#'canceled. Therefore it is good practice to check the order details once
#'an order ID is received from this call.
#'@return Returns an order ID (string), if successful.
#'@export
cancel_order<-function(object,orderid,variety){
  params <- as.list(environment(), all=TRUE)
  params[["object"]] <- NULL
  keys <- names(params)

  for(key in keys){
    if(is.null(params[[key]])){
      params[[key]] <- NULL
    }
  }

  method_params = params
  r <- NULL

  tryCatch({
    r<-rest_api_call(object,"POST","api.order.cancel",method_params)
    r<-httr::content(r)

  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r)) r <- r$data[["orderid"]]
  return(r)

}

#'Function to obtain last traded prices.
#'@description Function to obtain last traded prices.
#'@param object An object of type smart connect.
#'@param exchange Exchange for the order, e.g. NSE, NFO, CDS etc.
#'@param tradingsymbol Trading Symbol of the instrument.
#'@param symboltoken Symbol token
#'@details This function accepts a list of instruments (as exchange:symbol
#'pair, e.g. NFO:NIFTY18JANFUT) and returns last traded prices. Please note:
#' this function is meant to be called as a one-off way to get the quotes.
#' One should not call this function too many times. The api possibly has a
#' rate limit and too many requests will block the user
#' @return a data frame with last price data, with instruments as row names.
#' @export

get_ltp_data<-function(object,exchange,tradingsymbol,symboltoken){
  params <- as.list(environment(), all=TRUE)
  params[["object"]] <- NULL
  keys <- names(params)

  for(key in keys){
    if(is.null(params[[key]])){
      params[[key]] <- NULL
    }
  }

  method_params = params
  r <- NULL

  tryCatch({
    r<-rest_api_call(object,"POST","api.ltp.data",method_params)
    r<-httr::content(r)

  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r)& is.null(r$error)) r <- list_to_df(list(r$data))
  return(r)
}

#'Function to get the order details.
#'@description Gets the order details of the user.
#'@param object An object of type smart connect.
#'@return Returns a data frame with order details, if successful.
#'@export

order_book<-function(object){
  method_params=list()
  r<-NULL
  tryCatch({
    r<-rest_api_call(object,"GET","api.order.book",method_params)
    r<-httr::content(r)

  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r)& is.null(r$error)) r <-list_to_df(r$data)
  return(r)
}

#'Function to get the trade details.
#'@description Gets the order details of the user.
#'@usage trade_book(object)
#'@param object An object of type smart connect.
#'@return Returns a data frame with trade details, if successful.
#'@export

trade_book<-function(object){
  method_params=list()
  r<-NULL
  tryCatch({
    r<-rest_api_call(object,"GET","api.trade.book",method_params)
    r<-httr::content(r)

  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r$data)& is.null(r$error)) r <- list_to_df(r$data)
  return(r)
}

#'Function to get the rms limit details.
#'@description Gets the rms limit details of the user.
#'@usage rms_limit(object)
#'@param object An object of type smart connect.
#'@return Returns with rms_limit details, if successful.
#'@export

rms_limit<-function(object){
  method_params=list()
  r<-NULL
  tryCatch({
    r<-rest_api_call(object,"GET","api.rms.limit",method_params)
    r<-httr::content(r)

  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r)) r <- r
  return(r)
}

#'Function to get the position details.
#'@description Gets the position details of the user.
#'@usage position(object)
#'@param object An object of type smart connect.
#'@return Returns a dataframe with trade details, if successful.
#'@export

position<-function(object){
  method_params=list()
  r<-NULL
  tryCatch({
    r<-rest_api_call(object,"GET","api.position",method_params)
    r<-httr::content(r)

  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r)& is.null(r$error)) r <- list_to_df(list(r$data))
  return(r)
}

#'Function to get the holding details.
#'@description Gets the holdings details of the user.
#'@usage holding(object)
#'@param object An object of type smart connect.
#'@return Returns a list with holdings details, if successful.
#'@export

holding<-function(object){
  method_params=list()
  r<-NULL
  tryCatch({
    r<-rest_api_call(object,"GET","api.holding",method_params)
    r<-httr::content(r)
  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r)) r
  return(r)
}

convert_position<-function(object,exchange,oldproducttype,newproducttype,tradingsymbol,transactiontype,quantity,type){
  params <- as.list(environment(), all=TRUE)
  params[["object"]] <- NULL
  keys <- names(params)

  for(key in keys){
    if(is.null(params[[key]])){
      params[[key]] <- NULL
    }
  }

  method_params = params
  r <- NULL

  tryCatch({
    r<-rest_api_call(object,"POST","api.convert.position",method_params)
    r<-httr::content(r)
  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r)) r
  return(r)
}

#'Function to create Gtt rules
#'@description Function to create Gtt rules
#'@param object An object of type smart connect
#'@param tradingsymbol Trading Symbol of the instrument.
#'@param symboltoken Symbol token
#'@param exchange Exchange for the order, e.g. NSE, NFO, CDS etc.
#'@param transactiontype BUY or SELL.
#'@param producttype Product type, e.g. MIS, NRML, CNC, BO or CO.
#'@param price New Price (e.g. for limit orders).
#'@param quantity New quantity.
#'@param triggerprice Trigger Price
#'@param disclosedqty Disclosed Quantity
#'@param timeperiod Time Period.
#'@return Returns a rule id, if successful.
#'@export

gtt_create<-function(object,tradingsymbol,symboltoken,exchange,producttype,transactiontype,price,quantity,disclosedqty,triggerprice,timeperiod){
  params <- as.list(environment(), all=TRUE)
  params[["object"]] <- NULL
  keys <- names(params)
  message(keys)
  for(key in keys){
    if(is.null(params[[key]])){
      message(key)
      params[[key]] <- NULL
    }


    }

  method_params = params
  r <- NULL
  message(is.list(method_params))
  tryCatch({
    r<-rest_api_call(object,"POST","api.gtt.create",method_params)
    message(r)
    r<-httr::content(r)
  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r)) r
  return(r)
}

#'Function to modify gtt rule
#'@description Function to modify gtt rule
#'@param object An object of type smart connect
#'@param id Gtt rule id
#'@param symboltoken Symbol token
#'@param exchange Exchange for the order, e.g. NSE, NFO, CDS etc.
#'@param price New Price (e.g. for limit orders).
#'@param quantity New quantity.
#'@param triggerprice Trigger Price
#'@param disclosedqty Disclosed Quantity
#'@param timeperiod Time Period.
#'@return Returns a rule id, if successful.
#'@export
gtt_modify<-function(object,id,tradingsymbol,symboltoken,exchange,producttype,transactiontype,price,quantity,triggerprice,disclosedqty,timeperiod){
  params <- as.list(environment(), all=TRUE)
  params[["object"]] <- NULL
  keys <- names(params)

  for(key in keys){
    if(is.null(params[[key]])){
      params[[key]] <- NULL
    }
    else{
      message("in else")
      if(key == "id" ||key == "price" || key == "quantity" || key== "disclosedqty" || key == "triggerprice" || key == "timeperiod"){
        message("in if",key)
        params[[key]]<-as.integer(params[[key]])

        message(params[[key]])

      }
    }
  }

  method_params = params
  r <- NULL
  tryCatch({
    r<-rest_api_call(object,"POST","api.gtt.modify",method_params)
    r<-httr::content(r)
  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r)) r
  return(r)
}

#'Function to cancel gtt rule
#'@param object An object of type smart connect.
#'@param id Gtt rule id.
#'@param symboltoken Symbol token
#'@param exchange Exchange for the order, e.g. NSE, NFO, CDS etc
#'@return Returns a rule id, if successful.
#'@export
gtt_cancel<-function(object,id,symboltoken,exchange){
  params <- as.list(environment(), all=TRUE)
  params[["object"]] <- NULL
  keys <- names(params)

  for(key in keys){
    if(is.null(params[[key]])){
      params[[key]] <- NULL
    }
    else{
      message("in else")
      if(key == "id" ||key == "price" || key == "quantity" || key== "disclosedqty" || key == "triggerprice" || key == "timeperiod"){
        message("in if",key)
        params[[key]]<-as.integer(params[[key]])

        message(params[[key]])

      }
    }
  }

  method_params = params
  r <- NULL
  tryCatch({
    r<-rest_api_call(object,"POST","api.gtt.cancel",method_params)
    r<-httr::content(r)
  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r)) r
  return(r)
}

#'Function to get the details of gtt rule
#'@param object An object of type smart connect.
#'@param id Gtt rule id.
#'@return Returns a data frame with gtt details, if successful.
#'@export
gtt_details<-function(object,id){
  method_params=list("id"=id)
  r <- NULL
  tryCatch({
    r<-rest_api_call(object,"POST","api.gtt.details",method_params)
    r<-httr::content(r)

  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r))r <- r
  return(r)
}
#'Function to get the detailed list of gtt rule
#'@param object An object of type smart connect.
#'@param status a list of status to find the rulelist accordingly
#'@param page No of page
#'@param count no of counts of rule
#'@return Returns a data frame with gtt detailed list, if successful.
#'@export
gtt_lists<-function(object,status,page,count){
  params <- as.list(environment(), all=TRUE)
  params[["object"]] <- NULL
  keys <- names(params)

  for(key in keys){
    if(is.null(params[[key]])){
      params[[key]] <- NULL
    }
  }

  method_params = params
  message(is.list(method_params))
  r <- NULL
  tryCatch({
    r<-rest_api_call(object,"POST","api.gtt.list",method_params)
    r<-httr::content(r)

  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r))r <- r
  return(r)
}
#'Function to get the historic data
#'@param object An object of type smart connect.
#'@param exchange Exchange for the order, e.g. NSE, NFO, CDS etc
#'@param symboltoken symboltoken.
#'@param interval time interval
#'@param fromdate fromdate
#'@param todate todate
#'@return Returns historic data from the date range given with the time interval provided, if successful.
#'@export
get_candle_data<-function(object,exchange,symboltoken,interval,fromdate,todate){
  params <- as.list(environment(), all=TRUE)
  params[["object"]] <- NULL
  keys <- names(params)

  for(key in keys){
    if(is.null(params[[key]])){
      params[[key]] <- NULL
    }
  }

  method_params = params
  r <- NULL

  tryCatch({
    r<-rest_api_call(object,"POST","api.candle.data",method_params)
    r<-httr::content(r)

  }, error=function(e){
    message(e$message)
  })
  if(!is.null(r)) r <- r$data
  return(r)
}

