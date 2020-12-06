require(shiny)
folder_address = 'C:\MCSDS\CS542 Practical Statistical Learning\Project4\app'

x <- system("ipconfig", intern=TRUE)
z <- x[grep("IPv4", x)]
ip <- gsub(".*? ([[:digit:]])", "\\1", z)
print(paste0("the Shiny Web application runs on: http://", ip, ":80/"))

runApp('app', launch.browser=FALSE, port = 80, host = ip)