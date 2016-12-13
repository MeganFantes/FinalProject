require(ggmap)
require(ggplot2)
require(streamR)

tweets.Boston <- parseTweets("tweetsBoston.json")
tweets.Sydney <- parseTweets("tweetsSydney.json")

boston.map <- get_map(location = c(-71.192,42.2275,-70.9859,42.399), maptype = "roadmap")
ggmap(boston.map)

sydney.map <- get_map(location = c(150.5199,-34.11899,151.34665,-33.592110), maptype = "roadmap")
ggmap(sydney.map)

## interpretation of stuff from class
boston.points <- data.frame(x=as.numeric(tweets.Boston$lon),
                     y=as.numeric(tweets.Boston$lat))

boston.tweets.mapped <- ggmap(boston.map)+ 
  geom_point(data=boston.points, 
             aes(x=x,y=y),size=3,
             alpha = 1/5, color="darkblue")

boston.tweets.mapped

sydney.points <- data.frame(x=as.numeric(tweets.Sydney$lon),
                     y=as.numeric(tweets.Sydney$lat))

sydney.tweets.mapped <- ggmap(sydney.map)+ 
  geom_point(data=sydney.points, 
             aes(x=x,y=y),size=3,
             alpha = 1/5, color="darkblue")

sydney.tweets.mapped

