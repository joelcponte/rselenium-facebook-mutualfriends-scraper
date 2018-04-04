library(RSelenium)
library(rvest)

fb_profile <- function(id) {
      return(paste0("http://www.facebook.com/profile.php?id=", id))
}
      
fb_mutual_friends_page <- function(string, type = "username") {

      if (type == "username") {
              return(paste0("https://www.facebook.com/", string, "/friends_mutual"))
      }
      if (type == "id") {
            return(paste0("https://www.facebook.com/profile.php?id=", string, "&sk=friends_mutual&pnref=lhc"))
      } else {
            print("Choose a valid type")
            return(NULL)
      }
}


remDr <- remoteDriver(remoteServerAddr = "192.168.99.100", port = 4445L, browserName = "firefox")
remDr$open()

remDr$setTimeout(type = "page load", milliseconds = 99999999999)
remDr$navigate("http://www.facebook.com")

## log in
login = "xxxx" # PUT LOGIN HERE
password = "xxxx" # PUT PASSWORD HERE
  
## go to password field and fill
txtfield <- remDr$findElement(using = 'css selector', "#email")
txtfield$sendKeysToElement(list(login))
## go to password field and fill
txtfield <- remDr$findElement(using = 'css selector', "#pass")
txtfield$sendKeysToElement(list(password))
## click to log in
wxbutton <- remDr$findElement(using = 'css selector', "#u_0_2")
wxbutton$clickElement()

## go to your profile page
wxbutton <- remDr$findElement(using = 'css selector', "#userNav .noCount")
wxbutton$clickElement()

## go to your friends page
wxbutton <- remDr$findElement(using = 'css selector', '#fbTimelineHeadline [data-tab-key="friends"]')
wxbutton$clickElement()

#get number of friends
page = read_html(remDr$getPageSource()[[1]])
n_friends = html_nodes(page, "._3d0") %>% html_text
n_friends = as.numeric(n_friends[1])

friends = 1
# scroll down page down to the bottom
while(length(friends) < n_friends) {
  
  webElem <- remDr$findElement("css", "body")
  webElem$sendKeysToElement(list(key = "end"))
  
  #get number of friends shown now
  page = read_html(remDr$getPageSource()[[1]])
  friends = html_nodes(page, ".fcb a")
  
  #collected friends should be increasing
  print(paste0("collected friends: ", length(friends), " < total friends: ", n_friends))
}
# select only friends (the css selector gets other elements once all friends are found)
friends = friends[1:n_friends]



## retrieve ids and usernames
friends_names = html_text(friends)
friends_ids = sapply(friends, function(x) return(sub(".*id=([0-9]*).*", "\\1", x)))
friends_usernames = sapply(friends, function(x) return(sub(".*facebook\\.com/([A-Za-z0-9\\.]*)\\?fref.*", "\\1", x)))
friends_usernames[nchar(friends_usernames)>50] = NA

#some facebook accounts have usernames and others don't. The links can look different for those two groups.
#set ids for friends who have usernames to NA
## 
friends_df = data.frame(name = friends_names,
                        friends_usernames = friends_usernames,
                        friends_ids = friends_ids,
                        link_id = fb_mutual_friends_page(friends_ids, "id"),
                        link_username = fb_mutual_friends_page(friends_usernames),
                        friends_ids = friends_ids,
                        friends_usernames = friends_usernames,
                        stringsAsFactors = F)

#
friends_df$link = friends_df$link_id
friends_df$link[!is.na(friends_usernames)] = friends_df$link_username[!is.na(friends_usernames)]
friends_df$link_username[is.na(friends_df$friends_usernames)] = NA
#View(friends_df)
mutual_friends_ids_all = list()

#loops though all friends and collect your mutual friends with them
for (i in 1:n_friends) {
    #check if list is filled. This can be helpful if you would like to run the loop twice in case someone was missed in the first loop. The second loop will be a lot faster.
    if (!is.null(mutual_friends_ids_all[i][[1]])) next
    cat("Scrapping friend ", i, "out of ", n_friends, "...\n")
    remDr$navigate(friends_df$link[i])
    current_page = read_html(remDr$getPageSource()[[1]])
    n_friends_now = html_nodes(current_page, ".fsl.fwb.fcb") %>% html_text %>% length()
    mutual_friends = html_nodes(current_page, "[name='Mutual Friends'] ._3d0") %>% html_text %>% as.numeric
    mutual_friends = mutual_friends[1]
    
    #check if friend has deleted page
    deleted_text = tryCatch({html_nodes(current_page, ".uiHeaderTitle") %>% tail(1) %>% html_text()}, error = function(e) {'page not deleted!'})
    if ( deleted_text == "Sorry, this content isn't available right now") next
    #check if friend has no mutual friendship
    if ( is.na(mutual_friends) ) next
    
    # scroll down to load all mutual friends
    while (n_friends_now < mutual_friends) {
      webElem <- remDr$findElement("css", "body")
      webElem$sendKeysToElement(list(key = "end"))
      n_friends_now = html_nodes(current_page, ".fsl.fwb.fcb") %>% html_text %>% length()
      current_page = read_html(remDr$getPageSource()[[1]])
    }
    
    mutual_friends = html_nodes(current_page, ".fcb a")
    mutual_friends = mutual_friends[1:n_friends_now]
    mutual_friends_ids = sapply(mutual_friends, function(x) return(sub(".*id=([0-9]*).*", "\\1", x)))
    mutual_friends_ids_all[[i]] <- mutual_friends_ids
}
  