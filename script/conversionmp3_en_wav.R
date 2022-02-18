library(tuneR)

DirMP3="E:/travail_manip_tournesol/doc_voix_humaine/voix_humaine_database"
DirWAV="E:/travail_manip_tournesol/doc_voix_humaine/voix_humaine_database_wav"

FileMP3=list.files(DirMP3,full.names=T, pattern = ".mp3$",ignore.case=T)

for (i in 1:length(FileMP3))
{
  r <- try(readMP3(FileMP3[i]) ) 
  WavName=gsub(DirMP3,DirWAV,FileMP3[i])
  WavName=gsub(".mp3",".wav",WavName)
  r2=normalize(r,unit="16")
  writeWave(r2,WavName,extensible=FALSE)
  
#  mp3_to_wav(FileMP3[i])
}

