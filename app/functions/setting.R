# =============================================================
# 
# Setting Utility
# 
# =============================================================

source('functions/helpers.R')

readSetting <- function(filename){
  allSetting = read(filename, ":")
  colnames(allSetting) = c('Key', 'Value')
  allSetting
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
