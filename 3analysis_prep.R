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


### Functions
left = function(text, num_char) {
  substr(text, 1, num_char)
}

mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

###########################################################################
### LOAD & CLEAN DATA
rp <- read.csv(paste0(clean, "round_pilot_clean.csv")) 
r2 <- read.csv(paste0(clean, "round2_clean.csv")) 
r3 <- read.csv(paste0(clean, "round3_clean.csv")) 
r47 <- read.csv(paste0(clean, "rounds4-7_clean.csv")) 

colnames(rp) 
colnames(r2)
colnames(r3)
colnames(r47) ; r47$notes <- NULL ; r47$time <- NULL ; colnames(r47)
r47 <- r47 %>% dplyr::filter(!name == "") # extra entries

rp$name <- as.character(rp$name)
r2$name <- as.character(r2$name)
r3$name <- as.character(r3$name)
r47$name <- as.character(r47$name)

rp$ID <- as.character(rp$ID)
r2$ID <- as.character(r2$ID)
r3$ID <- as.character(r3$ID)
r47$ID <- as.character(r47$ID)

rall <- bind_rows(rp, r2, r3, r47) ; rall$X <- NULL

### UPDATE WITH RECODED DATA
recode <- read.csv(paste0(clean, "recode_clean.csv"))
recode$name <- as.character(recode$name)
recode$ID <- as.character(recode$ID)
recode$X <- NULL
temp <- rall %>%
  anti_join(recode, by = c("name", "ID")) %>% # keeps those in rall w/o match in recode
  bind_rows(recode) # tacks on recode thereby replacing (dropped) matches in rall & adding missings (e.g bens 3000s)

# Test to make sure a few I know recoded were updated
rall[rall$ID == "P021",]
temp[temp$ID == "P021",]

rall[rall$ID == "R2013",]
temp[temp$ID == "R2013",]

rall[rall$ID == "R3059",]
temp[temp$ID == "R3059",]

rall[rall$ID == "R3058",]
temp[temp$ID == "R3058",] # yup -- see Ben's now show up.

rall <- temp

lu <- read.csv(paste0("lu_images_190102.csv")) ; lu$X <- NULL
lu$coded1 <- as.character(lu$coded1) %>% toupper()
lu$coded2 <- as.character(lu$coded2) %>% toupper()

lu$coded1 <- gsub(".PNG" ,"", lu$coded1)
lu$coded2 <- gsub(".PNG" ,"", lu$coded2)


##########################################################################
### Add org name and year, reversing this:
# ar.samp <- data.frame(ar.samp) %>%
#   mutate(letter = ifelse(grepl("CIn",org),"A",
#                          ifelse(grepl("DOW",org),"B",
#                                 ifelse(grepl("EDF",org),"C",
#                                        ifelse(grepl("NRD",org),"D",
#                                               ifelse(grepl("OCN",org),"E",
#                                                      ifelse(grepl("SCF",org),"F",
#                                                             ifelse(grepl("TNC",org),"G",
#                                                                    ifelse(grepl("WCS",org),"H", "J")))))))))
# ar.samp$yr <- as.numeric(as.character(ar.samp$yr))
# ar.samp <- ar.samp %>%
#   group_by(org) %>%
#   mutate(code = paste0(letter,(yr-2000)*3)) %>% # Disguising yr numbers by multiplying 2 digits by 3
#   select(-letter)


# Make org key
org_name <- c("CIn", "DOW", "EDF", "NRD", "OCN", "SCF", "TNC", "WCS", "WWF")
org_code <- c(LETTERS[1:8],"J")# had skipped I b/c confusing w/ 1
org_key <- cbind(org_name, org_code) %>% as.data.frame()
org_key$org_name <- as.character(org_key$org_name)
org_key$org_code <- as.character(org_key$org_code)

# Make yr key
yr_num <- 2005:2017
yr_code <- ((2005:2017) - 2000)*3
yr_key <- cbind(yr_num, yr_code) %>% as.data.frame()
yr_key$yr_code <- as.character(yr_key$yr_code)
yr_key$yr_num <- as.character(yr_key$yr_num)

# Extract codes of org & yr from lu
lu$org_code <- left(lu$coded1, 1) %>% as.character()
lu$yr_code <- mid(lu$coded1, 2, 2) %>% as.character()
lu %>% count(org_code)
lu %>% count(yr_code)

# Pull in org name and year number
lu <- lu %>%
  left_join(org_key, by = "org_code") %>%
  left_join(yr_key, by = "yr_code")
  
# Fix the WWF paper records; LHS & RHS need rows specified for yr.
lu$org_name[which(left(lu$coded1,3) == "WWF")] <- "WWF"
lu$yr_num[which(left(lu$coded1,3) == "WWF")] <- right(lu$coded1,4)[which(left(lu$coded1,3) == "WWF")]


# All together now.
data.clean <- rall %>% left_join(lu, by = c("ID" = "coded2")) %>%
  dplyr::select(org_name, yr_num, ID, coded1, everything(), -org_code, -yr_code, ) %>%
  rename(ID_code = coded1, year = yr_num)
data.clean$year <- as.numeric(data.clean$year)

# Set 99s to NA (not coded when no evidence of humans)
data.clean <- na_if(data.clean, "99")

# write.csv(data.clean, "D:/Shared/BackedUp/Caitlin/NewConsDebate/responses_3analysis_ready/data.ready.no.mode_190302.csv")


########################################################
### Any split decisions?
### First, remove SarahP
data <- data.clean %>%
  filter( !name == "SarahP")

## Add to find any twos... will take mode later
moo <- data %>%
  select(-name, -ID_code) %>%
  group_by(org_name, year, ID) %>%
  summarize_at(vars(c1_humans.v1:c1_logo), .funs = sum)

# Get rows with any twos
foo <- which(apply(moo[,4:20], 1, function(r) any(r == "2")))
doo <- moo[foo,]
doo <- doo %>%
  select(ID) %>%
  ungroup(org_name, year) # not sure why this was still carried forth
doo$ID <- as.character(doo$ID)
doo <- doo %>% arrange(ID)
# write.csv(doo, "recode_190221.csv")

# These ones have some humans.v1 = 1 & 3. How will mode show up? How are NAs treated?
picks <- c("R2049", "R4015", "R3024", "P026", "R3068")
data[data$ID %in% picks ,]


######################################################### TAKE MODE
### Get mode of each var. Needs function (top of script).
# NA is treated as a value -- it's either picked if mode or not.
data <- data.clean %>%
  filter( !name == "SarahP")

data <- data %>%
  select(-name, -ID_code) %>%
  group_by(org_name, year, ID) %>%
  summarize_at(vars(c1_humans.v1:c1_logo), .funs = Mode) %>%
  arrange(ID) %>%
  as.data.frame()

colnames(data)
colnames <- c("org", "yr","ID",
              "humans.v1", "humans.v2",
              "rep.nonwh", "rep.nonmale", "rep.whmale", "rep.disab", "rep.nonwest",
              "land.urban", "land.cult",
              "nr.use", "nr.ag", "nr.restore",
              "rec.noext","rec.ext",
              "animal.noext", "animal.ext",
              "logo")
colnames(data) <- colnames
              
# write.csv(data, "D:/Shared/BackedUp/Caitlin/NewConsDebate/responses_3analysis_ready/data.ready.mode_190302.csv")



################################################### EXPLORE
# Prevalence in representation over time
temp <- data[data$org_name =="DOW",]
temp <- data

temp$year <- as.factor(temp$year)
ggplot(temp, aes(x = year, y = c1_landurban), fill = org_name) +
  geom_boxplot()
  geom_jitter()
mod <- glm(c1_landurban ~ year + org_name, data = temp, family=binomial(link="logit"))
mod <- glm(c1_landurban ~ year, data = temp, family=binomial(link="logit"))
summary(mod)

