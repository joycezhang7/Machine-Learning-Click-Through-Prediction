library(tree)
library(tidyverse)
library(randomForest)
library(MLmetrics)
#get the label
Labs <- scan("C:/Users/Joyce/Desktop/Emory MSBA/Machine Learning 1/Final Project/Project Data/Project Data/ProjectTrainingData.csv",nlines = 1,sep = ",",what = "xx")

#read in the data for only 5m record
Data1m <- matrix(scan("C:/Users/Joyce/Desktop/Emory MSBA/Machine Learning 1/Final Project/Project Data/Project Data/split1m-001.csv",skip = 1, sep = ",",what = "xx"),ncol = length(Labs),byrow = T)

colnames(Data1m) <- Labs

set.seed(1)
Data1mDF <- as.data.frame(Data1m)
#tmpDF15 <- Data1mDF %>% select(c('click','hour','C14','C17','C20','device_model','C15'))

Data1mDF$hour_tran <- substr(Data1mDF$hour,5,8)

# click through rate of C15
C15P <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'C15'],FUN = mean)
clickt15 <- data.frame(C15P)
clickt15 <- cbind(C15=rownames(clickt15),clickt15)


# click through rate of C14
C14P <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'C14'],FUN = mean)
clickt14 <- data.frame(C14P)
clickt14 <- cbind(C14=rownames(clickt14),clickt14)

# click through rate of C17
C17P <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'C17'],FUN = mean)
clickt17 <- data.frame(C17P)
clickt17 <- cbind(C17=rownames(clickt17),clickt17)

# click through rate of hour
hourP <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'hour'],FUN = mean)
clickt_hour <- data.frame(hourP)
clickt_hour <- cbind(hour=rownames(clickt_hour),clickt_hour)

## click through rate of C1
C1P <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'C1'],FUN = mean)
clickt_C1 <- data.frame(C1P)
clickt_C1 <- cbind(C1=rownames(clickt_C1),clickt_C1)

## click through rate of banner_pos
banner_posP <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'banner_pos'],FUN = mean)
clickt_banner_pos <- data.frame(banner_posP)
clickt_banner_pos <- cbind(banner_pos=rownames(clickt_banner_pos),clickt_banner_pos)

## click through rate of site_id
site_idP <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'site_id'],FUN = mean)
clickt_site_id <- data.frame(site_idP)
clickt_site_id <- cbind(site_id=rownames(clickt_site_id),clickt_site_id)

## click through rate of site_domain
site_domainP <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'site_domain'],FUN = mean)
clickt_site_domain <- data.frame(site_domainP)
clickt_site_domain <- cbind(site_domain=rownames(clickt_site_domain),clickt_site_domain)

## click through rate of site_catgory
site_categoryP <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'site_category'],FUN = mean)
clickt_site_category <- data.frame(site_categoryP)
clickt_site_category <- cbind(site_category=rownames(clickt_site_category),clickt_site_category)

## click through rate of app_id
app_idP <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'app_id'],FUN = mean)
clickt_app_id <- data.frame(app_idP)
clickt_app_id <- cbind(app_id=rownames(clickt_app_id),clickt_app_id)

## click through rate of app_domain
app_domainP <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'app_domain'],FUN = mean)
clickt_app_domain <- data.frame(app_domainP)
clickt_app_domain<- cbind(app_domain=rownames(clickt_app_domain),clickt_app_domain)


## click through rate of app_category
app_categoryP <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'app_category'],FUN = mean)
clickt_app_category <- data.frame(app_categoryP)
clickt_app_category<- cbind(app_category=rownames(clickt_app_category),clickt_app_category)

## click through rate of device_model
device_modelP <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'device_model'],FUN = mean)
clickt_device_model <- data.frame(device_modelP)
clickt_device_model<- cbind(device_model=rownames(clickt_device_model),clickt_device_model)


## click through rate of device_type
device_typeP <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'device_type'],FUN = mean)
clickt_device_type <- data.frame(device_typeP)
clickt_device_type<- cbind(device_type=rownames(clickt_device_type),clickt_device_type)

## click through rate of device_conn_type
device_conn_typeP <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'device_conn_type'],FUN = mean)
clickt_device_conn_type <- data.frame(device_conn_typeP)
clickt_device_conn_type<- cbind(device_conn_type=rownames(clickt_device_conn_type),clickt_device_conn_type)

## click through rate of C16
C16P <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'C16'],FUN = mean)
clickt_C16 <- data.frame(C16P)
clickt_C16<- cbind(C16=rownames(clickt_C16),clickt_C16)

## click through rate of C18
C18P <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'C18'],FUN = mean)
clickt_C18 <- data.frame(C18P)
clickt_C18<- cbind(C18=rownames(clickt_C18),clickt_C18)

## click through rate of C19
C19P <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'C19'],FUN = mean)
clickt_C19 <- data.frame(C19P)
clickt_C19<- cbind(C19=rownames(clickt_C19),clickt_C19)

## click through rate of C20
C20P <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'C20'],FUN = mean)
clickt_C20 <- data.frame(C20P)
clickt_C20<- cbind(C20=rownames(clickt_C20),clickt_C20)

## click through rate of C21
C21P <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'C21'],FUN = mean)
clickt_C21 <- data.frame(C21P)
clickt_C21<- cbind(C21=rownames(clickt_C21),clickt_C21)

## click through rate of hour_tran
hour_tranP <- tapply(as.integer(Data1mDF[,'click']),Data1mDF[,'hour_tran'],FUN = mean)
clickt_hour_tran <- data.frame(hour_tranP)
clickt_hour_tran<- cbind(hour_tran=rownames(clickt_hour_tran),clickt_hour_tran)


#merge with the 5m record data
cp15 <- inner_join(Data1mDF,clickt15)
cp14 <- inner_join(cp15,clickt14)
cp17 <- inner_join(cp14,clickt17)
cp_hour <- inner_join(cp17,clickt_hour)
cp_C1 <- inner_join(cp_hour,clickt_C1)
cp_banner_pos <- inner_join(cp_C1,clickt_banner_pos)
cp_site_id <- inner_join(cp_banner_pos,clickt_site_id)
cp_site_domain <- inner_join(cp_site_id,clickt_site_domain)
cp_site_category <- inner_join(cp_site_domain,clickt_site_category)
cp_app_id <- inner_join(cp_site_category,clickt_app_id)
cp_app_domain <- inner_join(cp_app_id,clickt_app_domain)
cp_app_category <- inner_join(cp_app_domain,clickt_app_category)
cp_device_model <- inner_join(cp_app_category,clickt_device_model)  
cp_device_type <- inner_join(cp_device_model,clickt_device_type)
cp_device_conn_type <- inner_join(cp_device_type,clickt_device_conn_type)
cp_C16 <- inner_join(cp_device_conn_type,clickt_C16)
cp_C18 <- inner_join(cp_C16,clickt_C18)
cp_C19 <- inner_join(cp_C18,clickt_C19)
cp_C20 <- inner_join(cp_C19,clickt_C20)
cp_C21 <- inner_join(cp_C20,clickt_C21)
cp_hour_tran <- inner_join(cp_C21,clickt_hour_tran)


cp_hour_tranDF <- as.data.frame(cp_hour_tran)
#cp_20 <- as.data.frame(cp20)
cp_hour_tranDF$click <- as.numeric(cp_hour_tran$click)

ord <- sample(nrow(cp_hour_tranDF))
cp_hour_tranDF <- cp_hour_tranDF[ord,]

# Doing a 60-20-20 split
TrainInd <- ceiling(nrow(cp_hour_tranDF)*0.6)
cp_hour_tranDFTrain <- cp_hour_tranDF[1:TrainInd,]
tmpDF <- cp_hour_tranDF[-(1:TrainInd),]
ValInd <- ceiling(nrow(tmpDF)*0.5)
cp_hour_tranDFFVal <- tmpDF[1:ValInd,]
cp_hour_tranDFFTest <- tmpDF[-(1:ValInd),]

remove(TrainInd,tmpDF,ValInd,ord)

#variables in final model:
#site_idP, app_idP, device_modelP, C14P, app_categoryP, C16P, site_domainP, hourP, C17P, app_domainP, C19P, C20P, C18P, device_typeP, banner_posP, C1P, C15P, device_conn_typeP, C21P, site_categoryP

#apply transformation if necessary
qqnorm(sqrt(cp_C21DFFVal$site_idP))
qqnorm(sqrt(cp_C21DFFVal$app_idP))
qqnorm(sqrt(cp_C21DFFVal$device_modelP))
qqnorm(sqrt(cp_C21DFFVal$C14P))
qqnorm(cp_C21DFFVal$app_categoryP)
qqnorm(cp_C21DFFVal$C16P)
qqnorm(sqrt(cp_C21DFFVal$site_domainP))
qqnorm(cp_C21DFFVal$hourP)
qqnorm(sqrt(cp_C21DFFVal$C17P))
#qqnorm(sqrt(cp_C21DFFVal$app_domainP))
#qqnorm(sqrt(cp_C21DFFVal$C19P))
qqnorm(cp_C21DFFVal$C21P)

# Log Loss ----------------------------------------------------------------

##Loss Function

loss <- function(PHat,Click) {
  Y <- as.integer(Click)
  eps <- 1e-15
  out <- -mean(Y*log(pmax(PHat,eps))+(1-Y)*log(pmax(1-PHat,eps)))
  return(out)
}

# Stepwise ----------------------------------------------------------------


# use stepwise logistic regression to find informative categories
SmallFm <- click ~ 1
#cub_C14P <- (C14P)^(1/3)
library(kader)
#BigFm <- click ~ C15P + sqrt(C14P) + sqrt(C17P)+hourP 
BigFm <- click ~ hourP + C1P+banner_posP+site_idP+site_domainP+site_categoryP+
  app_idP+app_domainP+app_categoryP+device_modelP+device_typeP+
  device_conn_typeP+C14P+C15P+C16P+C17P+C18P+C19P+C20P+C21P

OutSmall <- glm(SmallFm,family=binomial(link = "logit"),data=cp_C21DFTrain)

sc <- list(lower=SmallFm,upper=BigFm)
StepLRout <- step(OutSmall,scope=sc,direction="both")
summary(StepLRout)
#variables in final model:
#site_idP, app_idP, device_modelP, C14P, app_categoryP, C16P, site_domainP, hourP, C17P, app_domainP, C19P, C20P, C18P, device_typeP, banner_posP, C1P, C15P, device_conn_typeP, C21P, site_categoryP


StepLRValP <- predict(StepLRout,newdata=cp_C21DFFVal,type="response")

loss(StepLRValP,cp_C21DFFVal$click)


####Test Formula

new_Fm <- click ~ hourP + sqrt(site_idP)+sqrt(site_domainP) +
  sqrt(app_idP) + app_domainP + app_categoryP + sqrt(device_modelP) +
  device_conn_typeP + sqrt(C14P) + C16P + C19P + C20P + C21P +C18P

new_Fm <- click ~ hourP

out_new <- glm(new_Fm, family = binomial(link = "logit"), data = cp_hour_tranDFTrain)

summary(out_new)

#Validation 
LR_pred_new <- predict(out_new, newdata = cp_hour_tranDFFVal, type = "response")

loss(LR_pred_new,cp_hour_tranDFFVal$click)

#Test
LR_pred_new_test <- predict(out_new, newdata = cp_hour_tranDFFTest, type = "response")

loss(LR_pred_new_test,cp_C21DFFTest$click)

# Double Check ------------------------------------------------------------

#create a new data frame just based on the variables from model
final_model <- cp_hour_tranDF %>% select('click','hourP','site_idP','site_domainP',
                                         'app_idP','app_domainP','app_categoryP',
                                         'device_modelP','device_conn_typeP',
                                         'C14P','C16P','C17P','C19P','C20P','C21P',
                                         'C18P')
#check multi collinear
#final_model$click <- as.numeric(final_model$click)
res <- cor(final_model)
round(res,2)

library(regclass)
VIF(out_new)

# see the plot of the variables
plot(final_model$C14P,final_model$C17P)
plot(final_model$site_idP,final_model$site_domainP)



