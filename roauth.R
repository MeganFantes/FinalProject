
library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "LIMv0T8kxo6nJxTL1JrJ7WzM2"
consumerSecret <- "VuSHpLZd120w8iGbqSqm9MOFtyNV6Zao4FF25gbW8GqboYHahf"
my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                             requestURL = requestURL, accessURL = accessURL, authURL = authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(my_oauth, file = "my_oauth_final.Rdata")
