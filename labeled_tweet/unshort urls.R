#install.packages("RCurl")
library(RCurl)
decode_short_url <- function(url, ...) {
  # LOCAL FUNCTIONS #
  decode <- function(u) {
    Sys.sleep(0.5)
    x <- try( getURL(u, header = TRUE, nobody = TRUE, followlocation = FALSE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
    if(inherits(x, 'try-error') | length(grep(".*Location: (\\S+).*", x))<1) {
      return(u)
    } else {
      return(gsub('.*Location: (\\S+).*', '\\1', x))
    }
  }
  
  # MAIN #
  gc()
  # return decoded URLs
  urls <- c(url, ...)
  l <- vector(mode = "list", length = length(urls))
  l <- lapply(urls, decode)
  names(l) <- urls
  return(l)
}


decode_short_url(obamacare_labeled[[1]]$entities$urls[[1]]$expanded_url)#not the final redirection
expd<-decode_short_url(decode_short_url(decode_short_url(obamacare_labeled[[1]]$entities$urls[[1]]$expanded_url)))
identical(expd[[1]],names(expd))
decode_short_url(expd)
