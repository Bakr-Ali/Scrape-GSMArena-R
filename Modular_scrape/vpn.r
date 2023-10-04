# TODO: don't run these vpn functions if not on Linux
switch_vpn <- function(x = 10) {
  print("Switching VPN server..")
  protonvpn_resp <- system("protonvpn-cli c -r", intern = TRUE)
  if (!grepl("Successfully connected", protonvpn_resp[4], fixed = TRUE)) {
    disconnect_vpn(5)
    switch_vpn()
  }
  print("VPN changed!")
  Sys.sleep(x)
}
disconnect_vpn <- function(x = 1) {
  print("Disconnecting VPN..")
  protonvpn_resp <- system("protonvpn-cli d", intern = TRUE)
  print("VPN Disconnected!")
  Sys.sleep(x)
}
