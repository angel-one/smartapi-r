

api_root = "https://apiconnect.angelbroking.com"
api_login = "https://apiconnect.angelbroking.com/rest/auth/angelbroking/user/v1/loginByPassword"
api_login_url="https://smartapi.angelbroking.com/publisher-loginc"
api_routes <- list()

api_routes[["api.login"]] = "/rest/auth/angelbroking/user/v1/loginByPassword"
api_routes[["api.logout"]]="/rest/secure/angelbroking/user/v1/logout"
api_routes[["api.token"]] = "/rest/auth/angelbroking/jwt/v1/generateTokens"
api_routes[["api.refresh"]] = "/rest/auth/angelbroking/jwt/v1/generateTokens"
api_routes[["api.user.profile"]] = "/rest/secure/angelbroking/user/v1/getProfile"

#ORDER Params

api_routes[["api.order.place"]] = "/rest/secure/angelbroking/order/v1/placeOrder"
api_routes[["api.order.modify"]]="/rest/secure/angelbroking/order/v1/modifyOrder"
api_routes[["api.order.cancel"]]="/rest/secure/angelbroking/order/v1/cancelOrder"
api_routes[["api.order.book"]]="/rest/secure/angelbroking/order/v1/getOrderBook"

api_routes[["api.ltp.data"]]= "/rest/secure/angelbroking/order/v1/getLtpData"
api_routes[["api.trade.book"]]="/rest/secure/angelbroking/order/v1/getTradeBook"
api_routes[["api.rms.limit"]]="/rest/secure/angelbroking/user/v1/getRMS"
api_routes[["api.holding"]]="/rest/secure/angelbroking/portfolio/v1/getHolding"
api_routes[["api.position"]]="/rest/secure/angelbroking/order/v1/getPosition"
api_routes[["api.convert.position"]]= "/rest/secure/angelbroking/order/v1/convertPosition"

api_routes[["api.gtt.create"]]="/gtt-service/rest/secure/angelbroking/gtt/v1/createRule"
api_routes[["api.gtt.modify"]]="/gtt-service/rest/secure/angelbroking/gtt/v1/modifyRule"
api_routes[["api.gtt.cancel"]]="/gtt-service/rest/secure/angelbroking/gtt/v1/cancelRule"
api_routes[["api.gtt.details"]]="/rest/secure/angelbroking/gtt/v1/ruleDetails"
api_routes[["api.gtt.list"]]="/gtt-service/rest/secure/angelbroking/gtt/v1/ruleList"

api_routes[["api.candle.data"]]="/rest/secure/angelbroking/historical/v1/getCandleData"
