################################################################################################################# 
##### MSc NOTES #####
################################################################################################################# 

# Use the project startup notebook to start loading in packages, load in data, and get some basic good functions

# SQlite Connection notebook specifically for connecting to SQLite and connecting databases

## ALWAYS KEEP A BACKUP OF IMPORTANT DATASETS, ESPECIALLY WHEN DOING BIG EDITING TO IT 
Buzz_Master_BACKUP_20241013 <- Buzz_Master

################################################################################################################# 
## HOW TO CONNECT TO GITHUB 

# Connect to GitHub by going to Tools->Project Setup->Git connection-> Select files and Commit
# Make repository in GitHub if not already existing 
# then copy the information for existing bit not the TERMINAL window of R (Below...not the colsole)
# will ask for username and password but this will not work, must use TOKEN ... will likely need to create a new token in Github
## create token go to github profile..settings..scroll all the way to the bottom left..<> Developer Settings ....Create new Tokem
##copy token clipboard (and best to copy to text file)
## Input token code into the first popup AND INTO THE USERNAME AND PASSWORD POPUPS (even though this is really dumb)

# If you want to push a dataframe up to GitHub, need to save it as a .rds with function saveRDS()
saveRDS(Buzz_Noise_Monitor_Oct2018, "Buzz_Noise_Monitor_Oct2018") ## saves to project folder and can then push 
