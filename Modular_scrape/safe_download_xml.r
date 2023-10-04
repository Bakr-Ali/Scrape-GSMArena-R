source(vpn.r)

safe_download_xml <- function(url, x = 10) {
  switch_vpn(x)
  tryCatch(
    {
      print(paste("Downloading xml content of:", url))
      xml2::download_xml(url)
    },
    
    error = function(e) {
      print(paste("ERROR Retry downloading xml of:", url))
      disconnect_vpn()
      safe_download_xml(url, 30)
    }
  )
}
