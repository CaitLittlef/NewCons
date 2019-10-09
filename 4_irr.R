### Load packages
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("pdftools")
# install.packages("tesseract")
# install.packages("ggplot2")
library(dplyr)
library(tidyr)
library(pdftools)
library(tesseract)
library(ggplot2)


### dir prep
# setwd("//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/NewConsDebate")
setwd("D:/Shared/BackedUp/Caitlin/NewConsDebate")
clean <- ("D:/Shared/BackedUp/Caitlin/NewConsDebate/responses_2cleaned/")
ready.dir <- ("D:/Shared/BackedUp/Caitlin/NewConsDebate/responses_3analysis_ready/")


### Load cleaned data
data <- read.csv("data.ready_190221.csv")


# Looks like ICR package needs NA instead of 99.
# But note that this means MISSING DATA (like Sarah's)...
# gets coded the same as N/A <-- IS THIS A PROBLEM??
temp[temp=="99"]<-NA # checks out
data[data=="99"]<-NA
temp[temp=="#"]<-NA # checks out
data[data=="#"]<-NA

## Don't need org/year data
data %>%
  select(-org_name, year, ID_code)


### IRR (KRIPPENDORF ALPHA) 
# Spread into readable matrix by ICR, one question at a time
# Spread key is the "new" variables; value get shoved underneath
# Krippalpha() defaults to metric = nominal


vars <- NULL
alpha_all <- NULL
# alpha_noCait <- NULL
alpha_noBen <- NULL
alpha_noCait <- NULL
alpha_noNick <- NULL
alpha_noSarahC <- NULL
alpha_noSarahP <- NULL

num.loops <- 6:ncol(data) # Qs start in col 6
for (i in num.loops){
  
  # establish variable
  var <- colnames(data[i])
  
  # set-up matrix with all responses
  temp <- data %>%
    select(name, ID, var) %>%
    spread(key = ID, var) %>%
    select(-name) %>%
    as.matrix()
  
  # run kripp
  a <- krippalpha(temp)[[1]]
  
  # set-up matrix w/o ben responses
  temp <- data %>%
    select(name, ID, var) %>%
    filter(! name == "Ben") %>%
    spread(key = ID, var) %>%
    select(-name) %>%
    as.matrix()
  a_noBen <- krippalpha(temp)[[1]]
  
  # set-up matrix w/o cait responses
  temp <- data %>%
    select(name, ID, var) %>%
    filter(! name == "Caitlin") %>%
    spread(key = ID, var) %>%
    select(-name) %>%
    as.matrix()
  a_noCait <- krippalpha(temp)[[1]]
  
  # set-up matrix w/o nick responses
  temp <- data %>%
    select(name, ID, var) %>%
    filter(! name == "Nick") %>%
    spread(key = ID, var) %>%
    select(-name) %>%
    as.matrix()
  a_noNick <- krippalpha(temp)[[1]]
  
  # set-up matrix w/o sarahp responses
  temp <- data %>%
    select(name, ID, var) %>%
    filter(! name == "SarahP") %>%
    spread(key = ID, var) %>%
    select(-name) %>%
    as.matrix()
  a_noSarahP <- krippalpha(temp)[[1]]
  
  # set-up matrix w/o sarahc responses
  temp <- data %>%
    select(name, ID, var) %>%
    filter(! name == "SarahC") %>%
    spread(key = ID, var) %>%
    select(-name) %>%
    as.matrix()
  a_noSarahC <- krippalpha(temp)[[1]]
  
  vars <- rbind(vars, var)
  alpha_all <- rbind(alpha_all, a)
  alpha_noBen <- rbind(alpha_noBen, a_noBen)
  alpha_noCait <- rbind(alpha_noCait, a_noCait)
  alpha_noNick <- rbind(alpha_noNick, a_noNick)
  alpha_noSarahC <- rbind(alpha_noSarahC, a_noSarahC)
  alpha_noSarahP <- rbind(alpha_noSarahP, a_noSarahP)
}

results <- cbind(vars, alpha_all, alpha_noBen, alpha_noCait, alpha_noNick,
                 alpha_noSarahC, alpha_noSarahP) %>% as.data.frame
colnames(results) <- c("var", "alpha_all", "alpha_noBen", "alpha_noCait", "alpha_noNick",
                       "alpha_noSarahC", "alpha_noSarahP")
results$alpha_all <- as.numeric(as.character(results$alpha_all))
results$alpha_noBen <- as.numeric(as.character(results$alpha_noBen))
results$alpha_noCait <- as.numeric(as.character(results$alpha_noCait))
results$alpha_noNick <- as.numeric(as.character(results$alpha_noNick))
results$alpha_noSarahC <- as.numeric(as.character(results$alpha_noSarahC))
results$alpha_noSarahP <- as.numeric(as.character(results$alpha_noSarahP))
# results$improve <- (results$alpha_all - results$alpha_noSarahP)
View(results)

moo <- colSums(results[,2:7])

# results <- cbind(vars, alpha_all) 
# colnames(results) <- c("var", "alpha_all")
# results$alpha_all <- as.numeric(results$alpha_all)

# 
# write.csv(results, "AnnualReports_images_ROUND2_RESPONSES/k-alpha_image-round2_180830.csv")




c1_humans.v1 <- data %>%
  select(name, ID, c1_humans.v1) %>%
  spread(key = ID, value = c1_humans.v1) %>%
  select(-name) %>%
  as.matrix()
krippalpha(c1_humans.v1, metric = "nominal")

c1_humans.v2 <- data %>%
  select(name, ID, c1_humans.v2) %>%
  spread(key = ID, value = c1_humans.v2) %>%
  select(-name) %>%
  as.matrix()
krippalpha(c1_humans.v2, metric = "nominal")

c1_nonwh <- data %>%
  select(name, ID, c1_nonwh) %>%
  spread(key = ID, value = c1_nonwh) %>%
  select(-name) %>%
  as.matrix()
krippalpha(c1_nonwh, metric = "nominal")
#w/o caitlin
c1_nonwh <- data %>%
  select(name, ID, c1_nonwh) %>%
  filter(! name == "Caitlin") %>%
  spread(key = ID, value = c1_nonwh) %>%
  select(-name) %>%
  as.matrix()
a <- krippalpha(c1_nonwh, metric = "nominal")
a[[1]]

c1_nonmale <- data %>%
  select(name, ID, c1_nonmale) %>%
  spread(key = ID, value = c1_nonmale) %>%
  select(-name) %>%
  as.matrix()
krippalpha(c1_nonmale, metric = "nominal")

c1_disab <- data %>%
  select(name, ID, c1_disab) %>%
  spread(key = ID, value = c1_disab) %>%
  select(-name) %>%
  as.matrix()
krippalpha(c1_disab, metric = "nominal")

c1_nonwest <- data %>%
  select(name, ID, c1_nonwest) %>%
  spread(key = ID, value = c1_nonwest) %>%
  select(-name) %>%
  as.matrix()
krippalpha(c1_nonwest, metric = "nominal")

c1_landurban <- data %>%
  select(name, ID, c1_landurban) %>%
  spread(key = ID, value = c1_landurban) %>%
  select(-name) %>%
  as.matrix()
krippalpha(c1_landurban, metric = "nominal")

c1_resout <- data %>%
  select(name, ID, c1_resout) %>%
  spread(key = ID, value = c1_resout) %>%
  select(-name) %>%
  as.matrix()
krippalpha(c1_resout, metric = "nominal")



