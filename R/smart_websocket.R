library(rjson)
library(base64)
setClass("smartwebSocket",
         representation(

           jwtToken = "character",
           clientCode ="character",
           apiKey="character",
           url = "character",
           feedType="character",
           actionType="character"

         )
)
r_url<- "wss://smartapisocket.angelbroking.com/websocket"
smart_connect_object<-function(params){
  object = methods::new("smartwebSocket")
  tryCatch({

    object@jwtToken=ifelse(is.null(params[["jwtToken"]]),message("jwtToken cannot be blank"),
                           params[["jwtToken"]])
    object@clientCode=ifelse(is.null(params[["clientCode"]]),message("clientCode cannot be blank"),
                             params[["clientCode"]])
    object@apiKey=ifelse(is.null(params[['apiKey']]),'',params[["apiKey"]])

    object@url=ifelse(is.null(params[['url']]),r_url,params[['url']])

    object@feedType=ifelse(is.null(params[["feedType"]]),message("feedType cannot be blank"),
                           params[["feedType"]])
    object@actionType=ifelse(is.null(params[["actionType"]]),message("actionType cannot be blank"),
                             params[["actionType"]])

  }, error=function(e){
    message("in error function",e$message)
  })
  return(object)
}

smartwebSocket.connect<-(function(object){
  url<-paste(object@url,"?jwttoken=",object@jwtToken,"&&clientcode=",object@clientCode,"&&apikey=",object@apiKey,sep = "")
  message(url)
  ws <- websocket::WebSocket$new(url,autoConnect = FALSE)
  #message(ws)
  is_open=NULL
  ws$onOpen(function(event){
    #message("----------------------",event)
    is_open<-TRUE
    message("connection is opened")
    fetch_data()
    send_ticks()

  })
  fetch_data<-function(){
    message("Fetching data")
    #message(toJSON(list("actiontype":object@actionType,"feedtype":object@feedType,"jwttoken":object@jwtToken,"clientcode":object@clientCode,"apikey":object@apiKey)))
    ws$send(toJSON(list("actiontype":"subscribe","feedtype":"order_feed","jwttoken":"eyJhbGciOiJIUzUxMiJ9.eyJ1c2VybmFtZSI6IlMyMTI3NDEiLCJyb2xlcyI6MCwidXNlcnR5cGUiOiJVU0VSIiwiaWF0IjoxNjE5NTEyMTUxLCJleHAiOjE3MDU5MTIxNTF9.XVJQTyVJXSuG4j8QLH647RDcA8bu2HilkwhDIv_1Nu0fSZX3a5qYx9noOEMcvnl4iyHhfddEY2Q9OnfxOF3AlQ","clientcode":"S212741","apikey":"smartapi_key")))
  }
  send_ticks<-function(){
    message("heartbeat")
    later::later(send_ticks,10)
    ws$send(toJSON(list("actiontype":"heartbeat","feedtype":object@feedType,"jwttoken":object@jwtToken,"clientcode":object@clientCode,"apikey":object@apiKey)))
  }

  ws$onMessage(function(event) {
    message(base64enc::base64decode(event$data))
    msg_data<-memDecompress(base64enc::base64decode(event$data), "gzip", asChar=TRUE)

    cat("Client received message:",msg_data, "\n")

  })
  ws$onError(function(){
    cat("Error in the connection")
  })
  ws$onClose(function(){
    is_open<-FALSE
    cat("Closing the connection")

  })
  ws$connect()
})







