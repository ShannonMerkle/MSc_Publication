###################################################################################################################################
##### GITHUB NOTES ####

## all commands for git must be done in the TERMINAL window, not the command window 

git status ## will tell you what it is working on (can ignore the untracked files - look for unmerged paths in red)
git pull origin main # type into terminal window to pull any changes down from the main branch (do this before making a push)
git pull origin Shannon # same but a pull from branch Shannon instead of main 

git commit -m "commit message here" # to commit via terminal window 
git push origin main ## to push to the main branch 
git push origin shannon ## I am assuming this is how you push to other branches 

## FOR BRANCHES 

# First: do work in your branch to avoid overlapping with something someone else is working on 
# when you want to merge your branch with the main branch use MERGE in the TERMINAL WINDOW 

# Make sure your branch is up-to-date with main (no changes have been made to the main branch without knowing)
git checkout Shannon(branch_name)
git fetch origin <- updates local repository with any changes from the remote repository but DOES NOT APPLY THEM YET 
git merge origin/main <- brings any new changes from the main branch into your branch  
## ALL OF THIS ENSURES YOUR BRANCH IS FULLY SYNCHRONIZED WITH TH EMAIN BRACH BEFORE YOU ATTEMPT THE FINAL MERGE INTO MAIN 
# if you do not want to apply all the changes from the main branch to yours - skip down to REBASE 

# Switch to the main branch
git checkout main

# Pull the latest changes from the remote main
git pull origin main

# Merge your branch into main
git merge Shannon

# Push the changes to the remote main branch
git push origin main

##################################################################################################
### WHEN TO REBASE INSTEAD OF MERGE 

## a MERGE will combine the branches changes together, which is great for script changes but as great for any structural changes 
# or additions (new scripts, etc)

## use REBASE instead which will layer changes on top of each other instead of merge 
# ie: can layer structural changes from the main branch on top of script additions and edits from personal branch 

# to layer personal branch ON TOP OF main branch: 

# make sure you have the latest updates from the remote main branch without changes your current branch 
git fetch origin  

# apply your branch's commits on top of the main branch 
git rebase origin/main 

## <- will need to look for conflicts and resolve them 

