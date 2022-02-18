InputDir="G:/travail_manip_tournesol/travail_sur_voix_humaine/test_50000"
Parallel=3
Frequency=2
Expansion=1
TadaridaDfolder="C:/Users/Ludovic Crochard/OneDrive/Documents/These/audiomoth/Tadarida-D_forWindows-master/install"


WDi=getwd()
setwd(TadaridaDfolder)

shell(paste0("TadaridaD -t ",Parallel," -x ",Expansion," -f ",Frequency," ",InputDir))

setwd(WDi)
