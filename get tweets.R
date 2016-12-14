library(streamR)

# load the authorization for thw Twitter API (the "handshake")
load("my_oauth.Rdata")

# search for tweets in the Boston area (the given coordinates are a bouding box around the city of Boston)
filterStream("tweetsBoston.json",
             locations=c(-71.192,42.2275,-70.9859,42.399),timeout=3600,
             oauth=my_oauth)

# search for tweets in the Sydney area (the given coordinates are a bouding box around the city of Sydney)
filterStream("tweetsSydney.json",
             locations=c(150.5199,-34.11899,151.34665,-33.592110),timeout=5400,
             oauth=my_oauth)