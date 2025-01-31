
#Run this script to install and use the Updated Ranacapa
#IMPORTANT - make sure you opened the RPorject where Ranacapa is (ranacapa-master.rproj)

RanacapaFolder=getwd()
install.packages(RanacapaFolder, type="source",repos=NULL)
ranacapa::runRanacapaApp()



