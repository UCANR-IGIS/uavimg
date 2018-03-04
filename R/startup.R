.onAttach <- function(lib, pkg) {
    ver <- read.dcf(file.path(lib,pkg,"DESCRIPTION"), "Version")
    msg <- sprintf("uavimg (version %s)\nBug reports: https://github.com/UCANR-IGIS/uavimg/issues", as.character(ver))
    packageStartupMessage(msg)
}
