library(RSelenium)
library(rvest)

fb_profile <- function(id) {
      return(paste0("http://www.facebook.com/profile.php?id=", id))
}
      
fb_mutual_friends_page <- function(id) {
      return(paste0("https://www.facebook.com/profile.php?id=", id, "&sk=friends_mutual&pnref=lhc"))
}

checkForServer() 
startServer()


remDr <- remoteDriver(browserName="firefox", port=4444)

remDr$open(silent=T)


remDr$navigate("http://www.facebook.com")

#logar
#wxbox <- remDr$findElement(using = 'css selector', "#email")
#wxbox$sendKeysToElement("joelcerqueiraponte@gmail.com")

wxbutton <- remDr$findElement(using = 'css selector', ".fbxWelcomeBoxBlock")
wxbutton$clickElement()


wxbutton <- remDr$findElement(using = 'css selector', "a._6-6:nth-child(3)")
wxbutton$clickElement()

test = read_html(remDr$getPageSource()[[1]])


n_friends = html_nodes(test, "._3d0") %>%
                  html_text
n_friends = n_friends[1]

friends = html_nodes(test, ".fcb a")

friends = friends[1:n_friends]

friends_links = sapply(friends, function(x) return(sub(".*id=([0-9]*).*", "\\1", x)))

friends_names = html_text(friends)

i=1
      current_friend_link = fb_profile(friends_links[i])
      current_friend_name = friends_names[i]
      remDr$navigate(fb_mutual_friends_page(id))
      for (j in 1:100) {
            webElem <- remDr$findElement("css", "body")
            webElem$sendKeysToElement(list(key = "end"))
            remDr$setTimeout(type = "page load", milliseconds = 10000) 
      }

      ###
      current_page = read_html(remDr$getPageSource()[[1]])
      mutual_friends = html_nodes(current_page, ".fcb a")
      mutual_friends_links =  sapply(mutual_friends, function(x) return(sub(".*id=([0-9]*).*", "\\1", x)))
      