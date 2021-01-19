library(tree)
library(tidyverse)


# Import Data -------------------------------------------------------------


#get the label
TrainLabs <- scan("C:/Users/Joyce/Desktop/Emory MSBA/Machine Learning 1/Final Project/Project Data/Project Data/ProjectTrainingData.csv",nlines = 1,sep = ",",what = "xx")

TrainData <- matrix(scan("C:/Users/Joyce/Desktop/Emory MSBA/Machine Learning 1/Final Project/Project Data/Project Data/split1m-000.csv", sep = ",",what = "xx"),ncol = length(TrainLabs),byrow = T)

colnames(TrainData) <- TrainLabs

#convert to data frame if necessary
TrainDataDF <- as.data.frame(TrainData)

TestLabs <- scan("C:/Users/Joyce/Desktop/Emory MSBA/Machine Learning 1/Final Project/Project Data/Project Data/ProjectTestData.csv",nlines = 1,sep = ",",what = "xx")

#read in the data for only 5m record
TestData <- matrix(scan("C:/Users/Joyce/Desktop/Emory MSBA/Machine Learning 1/Final Project/Project Data/Project Data/ProjectTestData.csv",skip = 1, sep = ",",what = "xx"),ncol = length(TestLabs),byrow = T)

colnames(TestData) <- TestLabs

set.seed(1)
TestDataDF <- as.data.frame(TestData)

test <- TestDataDF

# Check Level Difference --------------------------------------------------

# recode the unseen levels in test data into level name "Other"

#C14
condition_C14 <- !(test$C14 %in% TrainDataDF$C14)

test$C14[condition_C14] <- 'Other'
sum(test$C14=='Other')

#C16
condition_C16 <- !(test$C16 %in% TrainDataDF$C16)

test$C16[condition_C16] <- 'Other'
sum(test$C16=='Other')

#C18
condition_C18 <- !(test$C18 %in% TrainDataDF$C18)

test$C18[condition_C18] <- 'Other'
sum(test$C18=='Other')

#C19
condition_C19 <- !(test$C19 %in% TrainDataDF$C19)

test$C19[condition_C19] <- 'Other'
sum(test$C19=='Other')

#C20
condition_C20 <- !(test$C20 %in% TrainDataDF$C20)

test$C20[condition_C20] <- 'Other'
sum(test$C20=='Other')

#C21
condition_C21 <- !(test$C21 %in% TrainDataDF$C21)

test$C21[condition_C21] <- 'Other'
sum(test$C21=='Other')

#Hour
condition_hour <- !(test$hour %in% TrainDataDF$hour)

test$hour[condition_hour] <- 'Other'
sum(test$hour=='Other')


#site_id
condition_site_id <- !(test$site_id %in% TrainDataDF$site_id)

test$site_id[condition_site_id] <- 'Other'
sum(test$site_id=='Other')

#site_domain
condition_site_domain <- !(test$site_domain %in% TrainDataDF$site_domain)

test$site_domain[condition_site_domain] <- 'Other'
sum(test$site_domain=='Other')

#app_id
condition_app_id <- !(test$app_id %in% TrainDataDF$app_id)

test$app_id[condition_app_id] <- 'Other'
sum(test$app_id=='Other')

#app_domain
condition_app_domain <- !(test$app_domain %in% TrainDataDF$app_domain)

test$app_domain[condition_app_domain] <- 'Other'
sum(test$app_domain=='Other')

#app_category
condition_app_category <- !(test$app_category %in% TrainDataDF$app_category)

test$app_category[condition_app_category] <- 'Other'
sum(test$app_category=='Other')

#device_model
condition_device_model <- !(test$device_model %in% TrainDataDF$device_model)

test$device_model[condition_device_model] <- 'Other'
sum(test$device_model=='Other')

#device_conn_type
condition_device_conn_type <- !(test$device_conn_type %in% TrainDataDF$device_conn_type)

test$device_conn_type[condition_device_conn_type] <- 'Other'
sum(test$device_conn_type=='Other')


# Calculate Click Through Rate ------------------------------------------------------

# Click through rate for each level in each variable
# for unseen levels, use the average click through probability of the corresponding variable
# add the levels in the data frame (dataframe for each variable)

# click through rate of C14
C14P <- tapply(as.integer(TrainDataDF[,'click']),TrainDataDF[,'C14'],FUN = mean)
clickt14 <- data.frame(C14P)
clickt14 <- cbind(C14=rownames(clickt14),clickt14)
clickt14 <- clickt14 %>% add_row(C14="Other",C14P=0.1457714)
mean(clickt14$C14P) # 0.1457714


# click through rate of hour
hourP <- tapply(as.integer(TrainDataDF[,'click']),TrainDataDF[,'hour'],FUN = mean)
clickt_hour <- data.frame(hourP)
clickt_hour <- cbind(hour=rownames(clickt_hour),clickt_hour)
clickt_hour <- clickt_hour %>% add_row(hour="Other",hourP=0.1718464)
mean(clickt_hour$hourP) #0.1718464


## click through rate of site_id
site_idP <- tapply(as.integer(TrainDataDF[,'click']),TrainDataDF[,'site_id'],FUN = mean)
clickt_site_id <- data.frame(site_idP)
clickt_site_id <- cbind(site_id=rownames(clickt_site_id),clickt_site_id)
clickt_site_id <- clickt_site_id %>% add_row(site_id='Other',site_idP=0.1762789)
mean(clickt_site_id$site_idP) #0.1762789


## click through rate of site_domain
site_domainP <- tapply(as.integer(TrainDataDF[,'click']),TrainDataDF[,'site_domain'],FUN = mean)
clickt_site_domain <- data.frame(site_domainP)
clickt_site_domain <- cbind(site_domain=rownames(clickt_site_domain),clickt_site_domain)
clickt_site_domain <- clickt_site_domain %>% add_row(site_domain="Other",site_domainP=0.1872113)
mean(clickt_site_domain$site_domainP) #0.1872113


## click through rate of app_id
app_idP <- tapply(as.integer(TrainDataDF[,'click']),TrainDataDF[,'app_id'],FUN = mean)
clickt_app_id <- data.frame(app_idP)
clickt_app_id <- cbind(app_id=rownames(clickt_app_id),clickt_app_id)
clickt_app_id <- clickt_app_id %>% add_row(app_id="Other",app_idP=0.1712762)
mean(clickt_app_id$app_idP) #0.1712762


## click through rate of app_domain
app_domainP <- tapply(as.integer(TrainDataDF[,'click']),TrainDataDF[,'app_domain'],FUN = mean)
clickt_app_domain <- data.frame(app_domainP)
clickt_app_domain <- cbind(app_domain=rownames(clickt_app_domain),clickt_app_domain)
clickt_app_domain <- clickt_app_domain %>% add_row(app_domain="Other",app_domainP=0.1313504)
mean(clickt_app_domain$app_domainP) #0.1313504

## click through rate of app_category
app_categoryP <- tapply(as.integer(TrainDataDF[,'click']),TrainDataDF[,'app_category'],FUN = mean)
clickt_app_category <- data.frame(app_categoryP)
clickt_app_category <- cbind(app_category=rownames(clickt_app_category),clickt_app_category)
clickt_app_category <- clickt_app_category %>% add_row(app_category="Other",app_categoryP=0.08660536)
mean(clickt_app_category$app_categoryP) #0.08660536


## click through rate of device_model
device_modelP <- tapply(as.integer(TrainDataDF[,'click']),TrainDataDF[,'device_model'],FUN = mean)
clickt_device_model <- data.frame(device_modelP)
clickt_device_model<- cbind(device_model=rownames(clickt_device_model),clickt_device_model)
clickt_device_model <- clickt_device_model %>% add_row(device_model="Other",device_modelP=0.182276)
mean(clickt_device_model$device_modelP) #0.182276

## click through rate of device_conn_type
device_conn_typeP <- tapply(as.integer(TrainDataDF[,'click']),TrainDataDF[,'device_conn_type'],FUN = mean)
clickt_device_conn_type <- data.frame(device_conn_typeP)
clickt_device_conn_type<- cbind(device_conn_type=rownames(clickt_device_conn_type),clickt_device_conn_type)
mean(clickt_device_conn_type$device_conn_typeP) #0.09777013


## click through rate of C16
C16P <- tapply(as.integer(TrainDataDF[,'click']),TrainDataDF[,'C16'],FUN = mean)
clickt_C16 <- data.frame(C16P)
clickt_C16 <- cbind(C16=rownames(clickt_C16),clickt_C16)
mean(clickt_C16$C16P) #0.2121632


## click through rate of C18
C18P <- tapply(as.integer(TrainDataDF[,'click']),TrainDataDF[,'C18'],FUN = mean)
clickt_C18 <- data.frame(C18P)
clickt_C18<- cbind(C18=rownames(clickt_C18),clickt_C18)
mean(clickt_C18$C18P) #0.1589967


## click through rate of C19
C19P <- tapply(as.integer(TrainDataDF[,'click']),TrainDataDF[,'C19'],FUN = mean)
clickt_C19 <- data.frame(C19P)
clickt_C19 <- cbind(C19=rownames(clickt_C19),clickt_C19)
clickt_C19 <- clickt_C19 %>% add_row(C19='Other',C19P=0.1334054)

mean(clickt_C19$C19P) #0.1334054


## click through rate of C20
C20P <- tapply(as.integer(TrainDataDF[,'click']),TrainDataDF[,'C20'],FUN = mean)
clickt_C20 <- data.frame(C20P)
clickt_C20 <- cbind(C20=rownames(clickt_C20),clickt_C20)
clickt_C20 <- clickt_C20 %>% add_row(C20="Other",C20P=0.1459172)
mean(clickt_C20$C20P) #0.1459172


## click through rate of C21
C21P <- tapply(as.integer(TrainDataDF[,'click']),TrainDataDF[,'C21'],FUN = mean)
clickt_C21 <- data.frame(C21P)
clickt_C21 <- cbind(C21=rownames(clickt_C21),clickt_C21)
clickt_C21 <- clickt_C21 %>% add_row(C21='Other',C21P=0.1370668)

mean(clickt_C21$C21P) #0.1370668


# Embed click rate column in Training Data ------------------------------------------

#merge with the 5m record data
TrainDataDF$C14P <- clickt14[match(TrainDataDF$C14,clickt14$C14),2]
TrainDataDF$hourP <- clickt_hour[match(TrainDataDF$hour,clickt_hour$hour),2]
TrainDataDF$site_idP <- clickt_site_id[match(TrainDataDF$site_id,clickt_site_id$site_id),2]

TrainDataDF$site_domainP <- clickt_site_domain[match(TrainDataDF$site_domain,clickt_site_domain$site_domain),2]

TrainDataDF$app_idP <- clickt_app_id[match(TrainDataDF$app_id,clickt_app_id$app_id),2]
TrainDataDF$app_domainP <- clickt_app_domain[match(TrainDataDF$app_domain,clickt_app_domain$app_domain),2]

TrainDataDF$app_categoryP <- clickt_app_category[match(TrainDataDF$app_category,clickt_app_category$app_category),2]

TrainDataDF$device_modelP <- clickt_device_model[match(TrainDataDF$device_model,clickt_device_model$device_model),2]

TrainDataDF$device_conn_typeP <- clickt_device_conn_type[match(TrainDataDF$device_conn_type,clickt_device_conn_type$device_conn_type),2]

TrainDataDF$C16P <- clickt_C16[match(TrainDataDF$C16,clickt_C16$C16),2]
TrainDataDF$C18P <- clickt_C18[match(TrainDataDF$C18,clickt_C18$C18),2]
TrainDataDF$C19P <- clickt_C19[match(TrainDataDF$C19,clickt_C19$C19),2]
TrainDataDF$C20P <- clickt_C20[match(TrainDataDF$C20,clickt_C20$C20),2]
TrainDataDF$C21P <- clickt_C21[match(TrainDataDF$C21,clickt_C21$C21),2]

# Merge with the Test Data ------------------------------------------------

#sum(is.na(test$))

test$C14P <- clickt14[match(test$C14,clickt14$C14),2]
test$hourP <- clickt_hour[match(test$hour,clickt_hour$hour),2]
test$site_idP <- clickt_site_id[match(test$site_id,clickt_site_id$site_id),2]

test$site_domainP <- clickt_site_domain[match(test$site_domain,clickt_site_domain$site_domain),2]

test$app_idP <- clickt_app_id[match(test$app_id,clickt_app_id$app_id),2]
test$app_domainP <- clickt_app_domain[match(test$app_domain,clickt_app_domain$app_domain),2]

test$app_categoryP <- clickt_app_category[match(test$app_category,clickt_app_category$app_category),2]

test$device_modelP <- clickt_device_model[match(test$device_model,clickt_device_model$device_model),2]

test$device_conn_typeP <- clickt_device_conn_type[match(test$device_conn_type,clickt_device_conn_type$device_conn_type),2]

test$C16P <- clickt_C16[match(test$C16,clickt_C16$C16),2]
test$C18P <- clickt_C18[match(test$C18,clickt_C18$C18),2]
test$C19P <- clickt_C19[match(test$C19,clickt_C19$C19),2]
test$C20P <- clickt_C20[match(test$C20,clickt_C20$C20),2]
test$C21P <- clickt_C21[match(test$C21,clickt_C21$C21),2]


# Run the Model -----------------------------------------------------------

trainD <- TrainDataDF  %>% select('click','hourP','C14P','site_idP','site_domainP','app_idP','app_domainP',
                                  'app_idP','app_domainP','app_categoryP','device_modelP','device_conn_typeP',
                                  'C16P','C18P','C19P','C20P','C21P')

trainD$click <- as.numeric(trainD$click)

new_Fm <- click ~ hourP + sqrt(site_idP)+sqrt(site_domainP) +
  sqrt(app_idP) + app_domainP + app_categoryP + sqrt(device_modelP) +
  device_conn_typeP + sqrt(C14P) + C16P + C19P + C20P + C21P + C18P

out_new <- glm(new_Fm, family = binomial(link = "logit"), data = trainD)

summary(out_new)

#Test 
LR_pred_new <- predict(out_new, newdata = test, type = "response")

prediction <- LR_pred_new

test1 <- test
test1$prediction <- prediction

# Write out Prediction ----------------------------------------------------

# Read in the submission file with correct data types
Data <- read.table("C:/Users/Joyce/Desktop/Emory MSBA/Machine Learning 1/Final Project/Project Data/Project Data/ProjectSubmission-TeamX.csv",colClasses=c("character","numeric"),header=T,sep=",")

# Your code here puts the probabilities in Data[[2]]
Data[[2]] <- prediction

# Round to 10 digits accuracy and prevent scientific notation.
# This converts Data[[2]] to strings.
Data[[2]] <- format(round(Data[[2]],10), scientific = FALSE)

# Write out the data in the correct format.
write.table(Data,file="ProjectSubmission-TeamX-Output.csv",quote=F,sep=",",
            row.names=F,col.names=c("id","P(click)"))
