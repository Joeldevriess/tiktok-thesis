library(jsonlite)
library(dplyr)

# Lees JSON in (streaming = veiliger voor grote files)
watch_raw_p01 <- stream_in(file("data/raw_json/participant_01.json"))
watch_raw_p02 <- stream_in(file("data/raw_json/participant_02.json"))
watch_raw_p03 <- stream_in(file("data/raw_json/participant_03.json"))
watch_raw_p04 <- stream_in(file("data/raw_json/participant_04.json"))
watch_raw_p05 <- stream_in(file("data/raw_json/participant_05.json"))
watch_raw_p06 <- stream_in(file("data/raw_json/participant_06.json"))
watch_raw_p07 <- stream_in(file("data/raw_json/participant_07.json"))
watch_raw_p08 <- stream_in(file("data/raw_json/participant_08.json"))
watch_raw_p09 <- stream_in(file("data/raw_json/participant_09.json"))
watch_raw_p10 <- stream_in(file("data/raw_json/participant_10.json"))
watch_raw_p11 <- stream_in(file("data/raw_json/participant_11.json"))
watch_raw_p12 <- stream_in(file("data/raw_json/participant_12.json"))
watch_raw_p13 <- stream_in(file("data/raw_json/participant_13.json"))
watch_raw_p14 <- stream_in(file("data/raw_json/participant_14.json"))
watch_raw_p15 <- stream_in(file("data/raw_json/participant_15.json"))
watch_raw_p16 <- stream_in(file("data/raw_json/participant_16.json"))
watch_raw_p17 <- stream_in(file("data/raw_json/participant_17.json"))
watch_raw_p18 <- stream_in(file("data/raw_json/participant_18.json"))
watch_raw_p19 <- stream_in(file("data/raw_json/participant_19.json"))
watch_raw_p20 <- stream_in(file("data/raw_json/participant_20.json"))
watch_raw_p22 <- stream_in(file("data/raw_json/participant_22.json"))
watch_raw_p23 <- stream_in(file("data/raw_json/participant_23.json"))

# Basis inspectie
dim(watch_raw_p01)
names(watch_raw_p01)
head(watch_raw_p01)
str(watch_raw_p01)

# de echte tabellen zitten als data.frame in de list-columns

#P01
video_browse_p01 <- watch_raw_p01$tiktok_video_browsing_history[[1]]
likes_p01        <- watch_raw_p01$tiktok_like_list[[3]]
favorites_p01    <- watch_raw_p01$tiktok_favorite_videos[[2]]
searches_p01     <- watch_raw_p01$tiktok_searches[[4]]
shares_p01       <- watch_raw_p01$tiktok_share_history[[5]]
logins_p01       <- watch_raw_p01$tiktok_login_history[[10]]

#P02
video_browse_p02 <- watch_raw_p02$tiktok_video_browsing_history[[1]]
likes_p02        <- watch_raw_p02$tiktok_like_list[[4]]
favorites_p02    <- watch_raw_p02$tiktok_favorite_videos[[2]]
searches_p02     <- watch_raw_p02$tiktok_searches[[5]]
shares_p02       <- watch_raw_p02$tiktok_share_history[[6]]
logins_p02       <- watch_raw_p02$tiktok_login_history[[11]]

#P03
video_browse_p03 <- watch_raw_p03$tiktok_video_browsing_history[[1]]
likes_p03        <- watch_raw_p03$tiktok_like_list[[4]]
favorites_p03    <- watch_raw_p03$tiktok_favorite_videos[[2]]
searches_p03     <- watch_raw_p03$tiktok_searches[[5]]
shares_p03       <- watch_raw_p03$tiktok_share_history[[6]]
logins_p03       <- watch_raw_p03$tiktok_login_history[[11]]

#P04
video_browse_p04 <- watch_raw_p04$tiktok_video_browsing_history[[1]]
likes_p04        <- watch_raw_p04$tiktok_like_list[[3]]
favorites_p04    <- watch_raw_p04$tiktok_favorite_videos[[2]]
searches_p04     <- watch_raw_p04$tiktok_searches[[4]]
shares_p04       <- watch_raw_p04$tiktok_share_history[[5]]
logins_p04       <- watch_raw_p04$tiktok_login_history[[10]]

#P05
video_browse_p05 <- watch_raw_p05$tiktok_video_browsing_history[[1]]
likes_p05        <- watch_raw_p05$tiktok_like_list[[3]]
favorites_p05    <- watch_raw_p05$tiktok_favorite_videos[[2]]
searches_p05     <- watch_raw_p05$tiktok_searches[[4]]
shares_p05       <- watch_raw_p05$tiktok_share_history[[5]]
logins_p05       <- watch_raw_p05$tiktok_login_history[[11]]

#P06
video_browse_p06 <- watch_raw_p06$tiktok_video_browsing_history[[1]]
likes_p06        <- watch_raw_p06$tiktok_like_list[[4]]
favorites_p06    <- watch_raw_p06$tiktok_favorite_videos[[2]]
searches_p06     <- watch_raw_p06$tiktok_searches[[5]]
shares_p06       <- watch_raw_p06$tiktok_share_history[[6]]
logins_p06       <- watch_raw_p06$tiktok_login_history[[11]]

#P07
video_browse_p07 <- watch_raw_p07$tiktok_video_browsing_history[[1]]
likes_p07        <- watch_raw_p07$tiktok_like_list[[4]]
favorites_p07    <- watch_raw_p07$tiktok_favorite_videos[[2]]
searches_p07     <- watch_raw_p07$tiktok_searches[[5]]
shares_p07       <- watch_raw_p07$tiktok_share_history[[6]]
logins_p07       <- watch_raw_p07$tiktok_login_history[[9]]

#P08
video_browse_p08 <- watch_raw_p08$tiktok_video_browsing_history[[1]]
likes_p08        <- watch_raw_p08$tiktok_like_list[[3]]
favorites_p08    <- watch_raw_p08$tiktok_favorite_videos[[2]]
searches_p08     <- watch_raw_p08$tiktok_searches[[4]]
shares_p08       <- watch_raw_p08$tiktok_share_history[[5]]
logins_p08       <- watch_raw_p08$tiktok_login_history[[6]]

#P09
video_browse_p09 <- watch_raw_p09$tiktok_video_browsing_history[[1]]
likes_p09        <- watch_raw_p09$tiktok_like_list[[4]]
favorites_p09    <- watch_raw_p09$tiktok_favorite_videos[[2]]
searches_p09     <- watch_raw_p09$tiktok_searches[[5]]
shares_p09       <- watch_raw_p09$tiktok_share_history[[6]]
logins_p09       <- watch_raw_p09$tiktok_login_history[[9]]

#P10
video_browse_p10 <- watch_raw_p10$tiktok_video_browsing_history[[1]]
likes_p10        <- watch_raw_p10$tiktok_like_list[[4]]     
favorites_p10    <- watch_raw_p10$tiktok_favorite_videos[[2]]
searches_p10     <- watch_raw_p10$tiktok_searches[[5]]
logins_p10       <- watch_raw_p10$tiktok_login_history[[7]]

#P11
video_browse_p11 <- watch_raw_p11$tiktok_video_browsing_history[[1]]
likes_p11        <- watch_raw_p11$tiktok_like_list[[3]]
favorites_p11    <- watch_raw_p11$tiktok_favorite_videos[[2]]
searches_p11     <- watch_raw_p11$tiktok_searches[[4]]         
shares_p11       <- watch_raw_p11$tiktok_share_history[[5]]    
logins_p11       <- watch_raw_p11$tiktok_login_history[[7]]

#P12
likes_p12        <- watch_raw_p12$tiktok_like_list[[2]]       
favorites_p12    <- watch_raw_p12$tiktok_favorite_videos[[1]]


#P13
video_browse_p13 <- watch_raw_p13$tiktok_video_browsing_history[[1]]
likes_p13        <- watch_raw_p13$tiktok_like_list[[3]]        
favorites_p13    <- watch_raw_p13$tiktok_favorite_videos[[2]]
searches_p13     <- watch_raw_p13$tiktok_searches[[4]]         
shares_p13       <- watch_raw_p13$tiktok_share_history[[5]]    
logins_p13       <- watch_raw_p13$tiktok_login_history[[7]]   

#P14
video_browse_p14 <- watch_raw_p14$tiktok_video_browsing_history[[1]]
likes_p14        <- watch_raw_p14$tiktok_like_list[[3]]        
favorites_p14    <- watch_raw_p14$tiktok_favorite_videos[[2]]
searches_p14     <- watch_raw_p14$tiktok_searches[[4]]         
shares_p14       <- watch_raw_p14$tiktok_share_history[[5]]    
logins_p14       <- watch_raw_p14$tiktok_login_history[[8]]   

#P15  
video_browse_p15 <- watch_raw_p15$tiktok_video_browsing_history[[1]]
likes_p15        <- watch_raw_p15$tiktok_like_list[[5]]        
favorites_p15    <- watch_raw_p15$tiktok_favorite_videos[[2]]
searches_p15     <- watch_raw_p15$tiktok_searches[[6]]         
shares_p15       <- watch_raw_p15$tiktok_share_history[[7]]    
logins_p15       <- watch_raw_p15$tiktok_login_history[[13]]

#P16  
video_browse_p16 <- watch_raw_p16$tiktok_video_browsing_history[[1]]
likes_p16        <- watch_raw_p16$tiktok_like_list[[2]]        
searches_p16     <- watch_raw_p16$tiktok_searches[[3]]         
shares_p16       <- watch_raw_p16$tiktok_share_history[[4]]    
logins_p16       <- watch_raw_p16$tiktok_login_history[[7]]   

#P17  
video_browse_p17 <- watch_raw_p17$tiktok_video_browsing_history[[1]]
likes_p17        <- watch_raw_p17$tiktok_like_list[[4]]
favorites_p17    <- watch_raw_p17$tiktok_favorite_videos[[2]]
searches_p17     <- watch_raw_p17$tiktok_searches[[5]]         
shares_p17       <- watch_raw_p17$tiktok_share_history[[6]]    
logins_p17       <- watch_raw_p17$tiktok_login_history[[12]]

#P18  
video_browse_p18 <- watch_raw_p18$tiktok_video_browsing_history[[1]]
likes_p18        <- watch_raw_p18$tiktok_like_list[[5]]
favorites_p18    <- watch_raw_p18$tiktok_favorite_videos[[2]]
searches_p18     <- watch_raw_p18$tiktok_searches[[6]]         
shares_p18       <- watch_raw_p18$tiktok_share_history[[7]]    
logins_p18       <- watch_raw_p18$tiktok_login_history[[13]]

#P19  
video_browse_p19 <- watch_raw_p19$tiktok_video_browsing_history[[1]]
likes_p19        <- watch_raw_p19$tiktok_like_list[[3]]
favorites_p19    <- watch_raw_p19$tiktok_favorite_videos[[2]]
searches_p19     <- watch_raw_p19$tiktok_searches[[4]]         
shares_p19       <- watch_raw_p19$tiktok_share_history[[5]]    
logins_p19       <- watch_raw_p19$tiktok_login_history[[11]]

#P20  
likes_p20        <- watch_raw_p20$tiktok_like_list[[3]]        
favorites_p20    <- watch_raw_p20$tiktok_favorite_videos[[1]]
searches_p20     <- watch_raw_p20$tiktok_searches[[4]]         
shares_p20       <- watch_raw_p20$tiktok_share_history[[5]]    
logins_p20       <- watch_raw_p20$tiktok_login_history[[7]]   

#P22  
video_browse_p22 <- watch_raw_p22$tiktok_video_browsing_history[[1]]
likes_p22        <- watch_raw_p22$tiktok_like_list[[3]]
favorites_p22    <- watch_raw_p22$tiktok_favorite_videos[[2]]
searches_p22     <- watch_raw_p22$tiktok_searches[[4]]         
shares_p22       <- watch_raw_p22$tiktok_share_history[[5]]    
logins_p22       <- watch_raw_p22$tiktok_login_history[[7]]

#P23  
video_browse_p23 <- watch_raw_p23$tiktok_video_browsing_history[[1]]
likes_p23        <- watch_raw_p23$tiktok_like_list[[3]]
favorites_p23    <- watch_raw_p23$tiktok_favorite_videos[[2]]
searches_p23     <- watch_raw_p23$tiktok_searches[[4]]         
shares_p23       <- watch_raw_p23$tiktok_share_history[[5]]    
logins_p23       <- watch_raw_p23$tiktok_login_history[[7]]

# quick checks
dim(video_browse_p01); names(video_browse_p01)
dim(likes_p02); names(likes_p02)