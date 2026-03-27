library(jsonlite)
library(dplyr)

# Lees JSON in (streaming = veiliger voor grote files)
watch_raw_p01 <- stream_in(file("data/raw_json/participant_01.json"))
watch_raw_p02 <- stream_in(file("data/raw_json/participant_02.json"))
watch_raw_p04 <- stream_in(file("data/raw_json/participant_04.json"))
watch_raw_p05 <- stream_in(file("data/raw_json/participant_05.json"))
watch_raw_p06 <- stream_in(file("data/raw_json/participant_06.json"))

# Basis inspectie
dim(watch_raw_p01)
names(watch_raw_p01)
head(watch_raw_p01)
str(watch_raw_p01)

# de echte tabellen zitten als data.frame in de list-columns
#P01
video_browse_p01 <- watch_raw_p01$tiktok_video_browsing_history[[1]]
likes_p01       <- watch_raw_p01$tiktok_like_list[[3]]
favorites_p01   <- watch_raw_p01$tiktok_favorite_videos[[2]]
searches_p01    <- watch_raw_p01$tiktok_searches[[4]]
shares_p01      <- watch_raw_p01$tiktok_share_history[[5]]
logins_p01      <- watch_raw_p01$tiktok_login_history[[10]]

#P02
video_browse_p02 <- watch_raw_p02$tiktok_video_browsing_history[[1]]
likes_p02       <- watch_raw_p02$tiktok_like_list[[4]]
favorites_p02   <- watch_raw_p02$tiktok_favorite_videos[[2]]
searches_p02    <- watch_raw_p02$tiktok_searches[[5]]
shares_p02      <- watch_raw_p02$tiktok_share_history[[6]]
logins_p02      <- watch_raw_p02$tiktok_login_history[[11]]

#P04
video_browse_p04 <- watch_raw_p04$tiktok_video_browsing_history[[1]]
likes_p04       <- watch_raw_p04$tiktok_like_list[[3]]
favorites_p04   <- watch_raw_p04$tiktok_favorite_videos[[2]]
searches_p04    <- watch_raw_p04$tiktok_searches[[4]]
shares_p04      <- watch_raw_p04$tiktok_share_history[[5]]
logins_p04      <- watch_raw_p04$tiktok_login_history[[10]]

#P05
video_browse_p05 <- watch_raw_p05$tiktok_video_browsing_history[[1]]
likes_p05       <- watch_raw_p05$tiktok_like_list[[3]]
favorites_p05   <- watch_raw_p05$tiktok_favorite_videos[[2]]
searches_p05    <- watch_raw_p05$tiktok_searches[[4]]
shares_p05      <- watch_raw_p05$tiktok_share_history[[5]]
logins_p05      <- watch_raw_p05$tiktok_login_history[[11]]

#P06
video_browse_p06 <- watch_raw_p06$tiktok_video_browsing_history[[1]]
likes_p06       <- watch_raw_p06$tiktok_like_list[[3]]
favorites_p06   <- watch_raw_p06$tiktok_favorite_videos[[2]]
searches_p06    <- watch_raw_p06$tiktok_searches[[4]]
shares_p06      <- watch_raw_p06$tiktok_share_history[[5]]
logins_p06      <- watch_raw_p06$tiktok_login_history[[11]]

# quick checks
dim(video_browse_p01); names(video_browse_p01)
dim(likes_p02); names(likes_p02)
