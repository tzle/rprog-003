## run_analysis.R
## author: Sharon Metzler
## date: 10/24/2014
## purpose: Coursework / Getting & Cleaning Data / JHU&Coursera

# -----------------------------------------------
# STEP 1: LOAD FEATURE DATA = COLUMN VARIABLE NAMES
features <- read.table("C:/_rdev/uci/features.txt",na.strings="NA",skip=0)
dim(features) # RESULT: [1] 561 2 # in english: 561 rows & 2 columns


# -----------------------------------------------
# STEP 2: CONVERT FEATURES TO VECTOR & EXAMINE
col_names <- c() # all values from column 2 of the features dataset
col_names <- features[,2]
col_names # DISPLAY RESULTS to verify list of variable names


# -----------------------------------------------
# STEP 3: LOAD THE TRAINING DATA & TEST DATA
xtrain <- read.table("C:/_rdev/uci/train/X_train.txt",na.strings="NA",skip=0)
xtest <- read.table("C:/_rdev/uci/test/X_test.txt",na.strings=NA,skip=0)


# -----------------------------------------------
# STEP 4: ATTACH COLUMN VARIABLE NAMES & EXAMINE RESULTS
colnames(xtrain) <- col_names
dim(xtrain) # RESULT: [1] 7352 561

colnames(xtest) <- col_names
dim(xtest)  # RESULT: [1] 2947 561


# -----------------------------------------------
# STEP 5: LOAD EXERCISE IDENTIFIERS & EXAMINE
ytrain <- read.table("C:/_rdev/uci/train/y_train.txt",na.strings="NA",skip=0)
dim(ytrain) # RESULT: [1] 7352 1

ytest <- read.table("C:/_rdev/uci/test/y_test.txt",na.strings="NA",skip=0)
dim(ytest) # RESULT: [1] 2947 1

# -----------------------------------------------
# STEP 6: ADD COLUMN OF EXERCISE IDS FROM THE YDATA
xtrain["Exercise"] <- ytrain
xtest["Exercise"] <- ytest

# EXAMINE VALUE DISTRIBUTION OF EXERCISE VALUES
table(xtrain$Exercise)
table(xtest$Exercise)

# -----------------------------------------------
# STEP 7: REPLACE EXERCISE IDS WITH DESCRIPTIVE TEXT
xtrain$Exercise[xtrain$Exercise==1] <- "Walking"
xtrain$Exercise[xtrain$Exercise==2] <- "Walking_Upstairs"
xtrain$Exercise[xtrain$Exercise==3] <- "Walking_Downstairs"
xtrain$Exercise[xtrain$Exercise==4] <- "Sitting"
xtrain$Exercise[xtrain$Exercise==5] <- "Standing"
xtrain$Exercise[xtrain$Exercise==6] <- "Laying"

# EXAMINE RESULTS
table(xtrain$Exercise)

xtest$Exercise[xtest$Exercise==1] <- "Walking"
xtest$Exercise[xtest$Exercise==2] <- "Walking_Upstairs"
xtest$Exercise[xtest$Exercise==3] <- "Walking_Downstairs"
xtest$Exercise[xtest$Exercise==4] <- "Sitting"
xtest$Exercise[xtest$Exercise==5] <- "Standing"
xtest$Exercise[xtest$Exercise==6] <- "Laying"


# EXAMINE RESULTS
table(xtest$Exercise)

# -----------------------------------------------
# STEP 8: LOAD SUBJECT ID DATA
# THESE ARE THE NUMERIC IDENTIFIERS OF THE PARTICIPANTS, 1-30

subj_train <- read.table("C:/_rdev/uci/train/subject_train.txt",na.strings="NA",skip=0) 
subj_test <- read.table("C:/_rdev/uci/test/subject_test.txt",na.strings="NA",skip=0) 

# ADD COLUMN OF SUBJECT ID TO XTRAIN
xtrain["SubjectID"] <- subj_train
xtest["SubjectID"] <- subj_test


# -----------------------------------------------
# STEP 9: CREATE VECTORS OF FIELDS TO KEEP
all_flds <- c()
all_flds <- read.table("C:/_rdev/uci/features.txt")
all_flds <- all_flds[,2]

keep_means <- grep("mean()",all_flds,value=TRUE)
keep_std <- grep("std()",all_flds,value=TRUE)
keep_flds <- c(keep_means,keep_std)
keep_flds <- c(keep_flds,"SubjectID","Exercise")

# MANAGE OBJECTS IN MEMORY
rm(all_flds,keep_means,keep_std)


# -----------------------------------------------
# STEP 10: REDUCE COLUMNS TO MEAN, STD, EXERCISE & SUBJECTID
xtrain <- xtrain[keep_flds]
xtest <- xtest[keep_flds]

# EXAMINE
dim(xtrain)
dim(xtest)

# -----------------------------------------------
# STEP 11: CLEAN THE COLUMN NAMES
names(xtrain) <- gsub("[^0-9A-Za-z///]","",names(xtrain))
names(xtest) <- gsub("[^0-9A-Za-z///]","",names(xtest))


# -----------------------------------------------
# STEP 12: COMBINE XTRAIN & XTEST
uci_data <- rbind(xtrain,xtest)

# EXAMINE
dim(uci_data)

# RENAME SUBJECTID VALUES WITH PREFIX TEXT TO MINIMIZE DATATYPE CONFUSION
uci_data <- transform(uci_data, SubjectID = sprintf('ID_%d',SubjectID))

# -----------------------------------------------
# STEP 13: RESHAPE UCI DATA
uci_melt <- melt(uci_data, id.vars=c("SubjectID","Exercise"))
uci_dply <- ddply(uci_melt, .(SubjectID,Exercise), summarize, mean=mean(value))

# MANAGE OBJECTS IN MEMORY
rm(uci_data, uci_melt)

STEP: 14 OUTPUT AS TEXT FILE
write.table(uci_dply, file="C:/_rdev/uci/output_mean_by_ID_Exercise.txt", row.names=FALSE, col.names=FALSE,sep=",")
