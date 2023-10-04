source(vpn.r)

safe_read_html <- function(url, x = 10) {
  switch_vpn(x)
  tryCatch(
    {
      print(paste("Getting html content of:", url))
      xml2::read_html(url)
    },
    
    error = function(e) {
      print(paste("ERROR Retry getting html of:", url))
      disconnect_vpn()
      safe_read_html(url, 30)
    }
  )
}
