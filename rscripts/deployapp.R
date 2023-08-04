#deploy app 
rsconnect::setAccountInfo(name='shawprojects',
                          token='760D39FCAEBB0CBBA181F3679BE8E6F3',
                          secret='6W88PpFX7Nk+DBymuBpFt62sTpOmYVTs4nBmWelx')

rsconnect::deployApp(appDir="/Users/rachel/Documents/GitHub/VideoGame-Dashboard-Repo/",
                     appName = "Video-Game-Dashboard",
                     forceUpdate = TRUE,
                     account = "shawprojects",
                     upload = TRUE)
