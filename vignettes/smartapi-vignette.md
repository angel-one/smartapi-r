---
title: "R Package for Smart Connect API"
date: "2021-04-21"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{smartapi-vignette}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```r
library(smartapi)
```

SmartApi is a set of REST-like HTTP APIs that expose many capabilities required to build a complete stock market investment and trading platform. It lets you execute orders in real time (equities, commodities, mutual funds), manage user portfolios, stream live market data over WebSockets, and more. This R package wraps a select set of functionalities of the version 3 (latest) of this API. This package allows
one to do the following:

- Place, modify and cancel orders
- Query specific order details, as well query holdings, positions, available margins and other related portfolio management operations.
- Obtain market quotes as well as historical data
- Websocket streaming (may be in future!).

## Requirements

The use of the API is based on authentication, using `api_key`, `client_code` and `password`.

## Workflow

The workflow starts with the creation of a smart connect object and completing a successful login flow. To create a smart connect object the following may be used. The object must be created using the `create_connection_object` function passing the `api_key` as an argument to the function and then generation the session using `generate_session` function by passing the `client_code` and `password` as argument to it.

```r
require(smartapi)
object = create_connection_object(list(api_key="you api key"))
object = generate_session(object,"your Client code","password")
print(object)
```

Completing a successful login will generate  `accessToken`,`refreshToken` and `feedToken` and also return the updated object.

## Example of Use
The following displays the call signatures for some useful functions. 

```r

profile <- get_profile(object,object@refresh_token)
object<- generate_token(object,object@refresh_token)
object<-renew_access_token(object)
order_id<-place_order(object,"NORMAL","SBIN-EQ","3045","BUY","NSE","LIMIT","INTRADAY","DAY","19500","0","0","1")
order_id<-modify_order(object,"NORMAL",order_id,"LIMIT","INTRADAY","DAY","19500","1","SBIN-EQ","3045","NSE")
order_id<-cancel_order(object,order_id, "NORMAL")
order<-order_book(object)
trade<-trade_book(object)
rms_limit<-rms_limit(object)
position<-position(object)
holding<-holding(object)
ltp_data<-get_ltp_data(object,"NSE", "SBIN-EQ", "3045")
convert_position<-convert_position(object,"NSE","DELIVERY","MARGIN","SBIN-EQ","BUY",1,"DAY")
gttcreateRule<-gtt_create(object,"SBIN-EQ","3045","NSE","MARGIN","BUY",100000.0,10,10,200000.0,365)
gttmodifyRule<-gtt_modify(object,10,"SBIN-EQ","3045","NSE","MARGIN","BUY",100000.0,10,10,200000.0,365)
gttCancelRule<-gtt_cancel(object,10,"3045","NSE")
gttDetails<-gtt_details(object,10)
gttLists<-gtt_lists(object,c("FORALL","CANCELLED"),1,10)
historicdata<-get_candle_data(object,"NSE","3045","ONE_MINUTE","2021-03-08 09:00","2021-03-09 09:20")


##Websocket

clientcode<-object@user_id
feedToken<-object@feed_token
script<-"nse_cm|2885&nse_cm|1594"   //exchange|token for multi stocks use & seperator

connection_object<-connect_object(list(feedToken=feedToken,clientCode=clientcode,script=script))

webSocket.connect(connection_object)

#logout
terminate_session(object,object@user_id)

```

