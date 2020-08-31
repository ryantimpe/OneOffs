
.First <- function(){
  if(interactive()){
    random_url_at_startup <- function(user = "ryantimpe",
                                      open_link = "n",
                                      link_block_list = c("lego.com", "ryantimpe.com"),
                                      update_increment = 14){

      #Decide whether to perform an API call or not
      file_name = paste0(dirname(tempdir()), "/rtweetlikes_", user, ".rds")

      if(!file.exists(file_name)){
        days_since_update = 99
      } else {
        days_since_update = as.numeric(Sys.Date() - as.Date(file.info(file_name)$ctime))
      }

      if(days_since_update > update_increment){
        rtweet::get_favorites(user) -> likes
        saveRDS(likes, file = file_name)
      } else {
        likes <- readRDS(file_name)
      }

      #Process twitter data for likes
      unlist(likes$urls_expanded_url) -> likes_urls

      #Remove likes without URLs...
      likes_urls <- likes_urls[!is.na(likes_urls)]

      #... And remove likes with links that you don't want to see...
      # (like tweets, my job, my own site, etc.)
      # Might want to consider github.com if you only want posts and not repos
      link_block_list = c(link_block_list, "twitter.com", "instagram.com",
                          "bit.ly", "tinyurl.com")
      likes_urls <- likes_urls[!grepl(paste0(link_block_list, collapse = "|"), likes_urls)]

      #... And remove only root URLs (like https://ryantimpe.com/)
      likes_urls <- likes_urls[grepl("//.+/.+/", likes_urls)]

      #Pick one
      this_link <- sample(likes_urls, 1)
      cat("Check out this link you wanted to read!\n", this_link)

      #Should the link open in your browser?
      if(open_link == "ask"){
        user_open <- readline(prompt="Open the link? (y/n): ")
      } else {
        user_open <- open_link
      }
      if(tolower(substr(user_open, 1, 1)) == "y") {
        browseURL(this_link)
      }

    } #End function
    random_url_at_startup()
  }
}

