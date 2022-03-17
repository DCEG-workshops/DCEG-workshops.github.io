###########################
# LIBRARY DEPENDENCIES
###########################
# Install "boxr",if necessary, and load the boxr package (and other packages 
# used in this script). boxr will allow you to access and write data to the box
# folders to which you you have access.

install.packages("boxr")
install.packages("plyr")
library(boxr)
library(plyr)

###########################
# SETUP
###########################
# setting up variables for ids and client secrets

CLIENT_ID = <YOUR_CLIENT_ID>
CLIENT_SECRET = <YOUR_CLIENT_SECRET>
BOX_INPUT_DIR_ID = 138553471000
BOX_INPUT_FILE_ID = 874644867544

###########################
# AUTHENTICATION
###########################
# Authentication is the process of syncing RStudio with your Box.com account.
# If you are not already logged into Box after running this code, you may be
# asked to log into Box. Identify Box as hard disk in the cloud.

box_auth(client_id = CLIENT_ID,
         client_secret = CLIENT_SECRET) 

# Set the working directory to your Box folder using the folder ID
box_setwd(dir_id = BOX_INPUT_DIR_ID)

###########################
# READING IN FILES
###########################
# box_read reads the file into local memory - see the console after running the 
# code. After closing your RStudio session the file/data is deleted from the 
# temporary local memory.

# NOTE: The file ID is needed to read in the data

bc_data = box_read(file_id = BOX_INPUT_FILE_ID)

###########################
# RUNNING ANALYSES
###########################
# We can now run standard R code. Here we are generating descriptive statistics,
# creating a new variable, and running a logistic model.

   # Generate descriptive statistics
apply(bc_data[c('age', 'BMI')], 2, summary)

apply((bc_data[c('famhist', 'menarche_cat', 'parity', 'agemeno_cat',
                 'ever_smoke')]), 2, table)

   # Create new variable: BMICAT, a 4-level categorical variable  
bc_data$BMIcat <- cut(bc_data$BMI,c(0, 18.5, 25, 30, 35, 100), 
                      labels=c("under weight", "normal","over weight","obese1",
                               "obese2"),right=FALSE)

   # Check BMI categories
ddply(bc_data, "BMIcat", summarise,
      N    = length(BMI),
      Min  = min(BMI),
      Mean = mean(BMI),
      Max  = max(BMI))

   # Run logistic model
logit_parity <- glm(status ~ parity + age, data=bc_data, family = "binomial")
summary(logit_parity)

###########################
# WRITING TO BOX
###########################
# We can use the box_write function to write the updated data set with the
# newly created BMIcat variable back to your personal Box folder.

   #set the BOX_OUTPUT_DIR_ID to the Box folder that you created by setting
   #the BOX_OUTPUT_DIR_ID variable to your folder ID
   BOX_OUTPUT_DIR_ID = <YOUR_BOX_OUTPUT_DIR_ID>

   # Save the file using the 'box_write' function
box_write(object = bc_data, file_name = "bc_data_ANH.csv",
             dir_id = BOX_OUTPUT_DIR_ID, 
             description = "ANH Updated workshop dataset, Oct 28: added BMI_cat variable")

###########################
# READING IN UPDATED FILE
###########################
# The updated data that was saved in the last step can be loaded using the
# file id for the new file (found in console).

BOX_OUTPUT_FILE_ID = <YOUR_BOX_OUTPUT_FILE_ID>

# NOTE: Without specifying a 'version_id' or 'version_no', you will always get
# the most recent version

box_setwd(dir_id = BOX_OUTPUT_DIR_ID)

bc_data_new = box_read(file_id = BOX_OUTPUT_FILE_ID)

###########################
# MANIPULATING UPDATED FILE
###########################   
# The data can then be manipulated or analyzed as needed

   # Subset the data to include only those 50 years of age or older
bc_data_new=subset(bc_data_new, age>=50)

###########################
# SAVE UPDATED FILE
###########################   
# Because we are using the same file name, Box will update the data set using
# version control

box_write(object = bc_data_new, file_name = "bc_data_ANH.csv",
          dir_id = BOX_OUTPUT_DIR_ID, 
          description = "ANH Updated workshop dataset, Oct 28: ages ≥50")
   

# Save updated version to new version--now read in previous version