# =============================================================
# 
# Setting Utility
# 
# =============================================================

source('functions/helpers.R')

readSetting <- function(filename){
  setting = read("setting.txt", ":")
  colnames(setting) = c('Key', 'Value')
  setting
}

write <- function(dataset, file, separators){
  df = data.frame(dataset)
  write.table(df,file=file, col.names=FALSE,row.names=FALSE,sep=separators,quote=FALSE)
}

setSetting <- function(setting, key, newValue){
  setting[which(setting$Key == key),"Value"] = newValue
  setting
}

getSetting <- function(setting, key){
  setting[which(setting$Key == key),"Value"]
}
