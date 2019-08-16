rm(list=ls())

setwd("~/QRS/Finance Stuff/APT")

libraries_required = c('lubridate', 'xts')

for(i in seq(libraries_required))
{
  if(!(libraries_required[i] %in% rownames(installed.packages())))
  {
    try(expr = install.packages('libraries_required[i]'), silent = T)
  }
  try(expr = library(libraries_required[i], character.only = T), silent = T)
}

list.files()

India_10_yr_bond <- read.csv(file = "India 10-Year Bond Yield Historical Data Monthly.csv", 
                             header = T, stringsAsFactors = F)

head(India_10_yr_bond,40)

# rownames(India_10_yr_bond) <- as.Date(India_10_yr_bond$Date, format="%d/%m/%y")

India_10_yr_bond$Date <- as.Date(India_10_yr_bond$Date, format="%d/%m/%y")

# India_10_yr_bond <- India_10_yr_bond[,-1]

head(India_10_yr_bond)

India_10_yr_bond$Date <- format(India_10_yr_bond$Date, format="%y-%m")

head(India_10_yr_bond)

NIFTY_monthly <- read.csv(file = "NIFTY_monthly.csv", header = T, stringsAsFactors = F)

head(NIFTY_monthly)

# rownames(NIFTY_monthly) <- as.Date(NIFTY_monthly$Date, format="%d %b %y")

NIFTY_monthly$Date <- as.Date(NIFTY_monthly$Date, format="%d %b %y")

head(NIFTY_monthly)

NIFTY_monthly$Date <- format(NIFTY_monthly$Date, format="%y-%m")

# NIFTY_monthly <- NIFTY_monthly[,-1]

head(NIFTY_monthly)

NIFTY_monthly <- NIFTY_monthly[,1:5]

head(NIFTY_monthly)

temp_name <- colnames(NIFTY_monthly)

# NIFTY_monthly <- to.monthly(as.xts(NIFTY_monthly))

colnames(NIFTY_monthly) <- temp_name

# NIFTY_monthly <- NIFTY_monthly[dim(NIFTY_monthly)[1]:1,]

head(NIFTY_monthly)
head(India_10_yr_bond)

Combined <- merge(x = NIFTY_monthly[,c(1,5)], 
                  y = India_10_yr_bond[,1:2], by = "Date", all.y = TRUE)

head(Combined)

colnames(Combined) <- c("Date","NIFTY_Close","India_10_yr_bond_RF")

head(Combined)

Combined <- na.omit(Combined)

head(Combined)

Combined$NIFTY_ret <- c(NA,100*diff(Combined$NIFTY_Close)/Combined$NIFTY_Close[-1])
  
head(Combined)

Combined <- na.omit(Combined)

head(Combined)

# Converting annual bond yield to monthly

Combined$India_10_yr_bond_RF_monthly <- (((((Combined$India_10_yr_bond_RF/100)+1)^10)^(1/(10*12))) - 1)*100

head(Combined,40)

# Calculating market risk premium (MRP)

for (i in seq(dim(Combined)[[1]]))
{
  Combined[i,"MRP"] <- mean(Combined[1:i,"NIFTY_ret"]) - Combined[i,"India_10_yr_bond_RF_monthly"]
}

head(Combined,20)
tail(Combined,20)

# Importing CPI

list.files()

India_monthly_Inflation <- read.csv(file = "inflation_India_monthly_basis.csv", header = T,
                                    stringsAsFactors = F)

head(India_monthly_Inflation)

for(j in seq(dim(India_monthly_Inflation)[1]))
{
  temp_vector <- rev(strsplit(India_monthly_Inflation[j,1], split = ' ')[[1]])[1:2]
  
  temp_vector_2 <- strsplit(temp_vector[2],split='')[[1]]
  
  temp_vector_2[1] <- toupper(temp_vector_2[1])
  
  temp_vector[2] <- paste0(temp_vector_2, collapse = '')
  
  temp_vector[3] <- "01"
  
  India_monthly_Inflation[j,"Date"] <- paste0(temp_vector, collapse = ' ')
}

head(India_monthly_Inflation)
tail(India_monthly_Inflation)

India_monthly_Inflation <- India_monthly_Inflation[2:3]

head(India_monthly_Inflation)
tail(India_monthly_Inflation)

India_monthly_Inflation$inflation <- as.numeric(gsub("%", "",India_monthly_Inflation$inflation))

head(India_monthly_Inflation)
tail(India_monthly_Inflation)

colnames(India_monthly_Inflation) <- c("Inflation_%","Date")

head(India_monthly_Inflation)
tail(India_monthly_Inflation)

India_monthly_Inflation$Date <- as.Date(India_monthly_Inflation$Date, format="%Y %B %d")

head(India_monthly_Inflation)
tail(India_monthly_Inflation)

India_monthly_Inflation$Date <- format(India_monthly_Inflation$Date, format="%y-%m")

head(India_monthly_Inflation)
tail(India_monthly_Inflation)

head(Combined)
tail(Combined)

All_Factors <- merge(x = Combined, y = India_monthly_Inflation, by = "Date", all.y = TRUE)

head(All_Factors)
tail(All_Factors)

All_Factors <- na.omit(All_Factors)

head(All_Factors)
tail(All_Factors)
dim(All_Factors)

APT_data <- All_Factors[,c("NIFTY_ret", "MRP", "Inflation_%")]

head(APT_data)
tail(APT_data)
dim(APT_data)

summary(lm(APT_data$NIFTY_ret~APT_data$MRP+APT_data$`Inflation_%`))

# We see that with alpha = 0.1, both are significant