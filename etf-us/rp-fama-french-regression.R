library(rmarkdown)

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')

library('tidyverse')
library('lubridate')
library('ggthemes')
library('reshape2')
library('viridis')
library('DT')
library('ggrepel')

options(stringsAsFactors = FALSE)
options("scipen"=100)

#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/Pandoc")

#render("rp-fama-french-regression.Rmd", output_file="rp-fama-french-regression.html")
#q()

#source("C:/stockviz/r/config.r")
source("/mnt/hollandr/config.r")

plotPath <- "analysis/plots-fama-fench"
pdf(NULL)

args = commandArgs(TRUE)
commandFlag <- args[1]

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDt <- as.Date("1980-01-01")
maxDt <- sqlQuery(lconUs2, "select max(time_stamp) from FAMA_FRENCH_5_FACTOR_DAILY") [[1]]

maxRegDt <- sqlQuery(lconUs2, "select max(time_stamp) from FAMA_FRENCH_REGRESSION") [[1]]

if(!is.na(commandFlag) && commandFlag != 'DELTA'){
	if (!is.na(maxRegDt) && maxDt == maxRegDt) q()
} 

factorDf <- sqlQuery(lconUs2, sprintf("select time_stamp, key_id, ret from FAMA_FRENCH_5_FACTOR_DAILY where time_stamp >= '%s' and ret_type = 'TTBIA' order by time_stamp", startDt))
momDf <- sqlQuery(lconUs2, sprintf("select time_stamp, ret as MOM from FAMA_FRENCH_MOMENTUM_DAILY where key_id='MOM' and time_stamp >= '%s' and ret_type = 'M' order by time_stamp", startDt))

retDf <- factorDf %>% mutate(ret=ret/100) %>% pivot_wider(names_from=key_id, values_from=ret) %>% select(!RF) %>% inner_join(momDf, by='time_stamp') %>% mutate(MOM = MOM/100) %>% rename(MKT_RF = `MKT-RF`) %>% as.data.frame()

factorIds <- names(retDf)['time_stamp' != names(retDf)]

doFfIndustry <- function(){
	idefs <- sqlQuery(lconUs2, "select * from FAMA_FRENCH_DEFS order by ID")

	industryDf <- sqlQuery(lconUs2, sprintf("select time_stamp, key_id, ret from FAMA_FRENCH_INDUSTRY_49_DAILY where time_stamp >= '%s' and ret_type = 'AVWRD' order by time_stamp", startDt))

	indusIds <- unique(industryDf$key_id)

	frml <- formula(sprintf("indret ~ %s", paste(factorIds, collapse = " + ")))

	for(i in 1:length(indusIds)){
		indusId <- indusIds[i]
		
		if(!is.na(commandFlag) && commandFlag == 'DELTA'){
			ctr <- sqlQuery(lconUs2, sprintf("select count(*) from FAMA_FRENCH_REGRESSION where KEY_ID = '%s' and KEY_TYPE = '%s'", indusId, 'FF_IND'))[[1]]
			if (ctr > 0) next
		}
		
		print(indusId)
		iName <- idefs[idefs$ID == indusId,]$DEF[1]
		
		regDf <- industryDf %>% filter(key_id == indusId) %>% mutate(indret = ret/100) %>% select(time_stamp, indret) %>% inner_join(retDf, by='time_stamp') %>% as.data.frame()
		
		lmmodel <- lm(frml, tail(regDf, 500))
		
		residCumRet <- Return.cumulative(lmmodel$residuals) * 100
		residSd <- sd(lmmodel$residuals * 100)
		coeffs <- coef(lmmodel) * 100
		rsqd <- summary(lmmodel)$r.squared
		
		sqlQuery(lconUs2, sprintf("delete from FAMA_FRENCH_REGRESSION where KEY_ID = '%s' and KEY_TYPE = '%s'", indusId, 'FF_IND'))
		
		sqlQuery(lconUs2, sprintf("insert into FAMA_FRENCH_REGRESSION (KEY_ID, KEY_TYPE, TIME_STAMP, INTERCEPT, MKT_RF, SMB, HML, RMW, CMA, MOM, R_SQD, RESIDUAL_CUMRET, RESIDUAL_SD) 
									values ('%s', '%s', '%s', %f, %f, %f, %f, %f, %f, %f, %f, %f, %f)",
										indusId, 'FF_IND', maxDt,
										coeffs[[1]], coeffs[[2]], coeffs[[3]], coeffs[[4]], coeffs[[5]], coeffs[[6]], coeffs[[7]], 
										rsqd, residCumRet, residSd))
					
	}
}

doEtfs <- function(){
	etfs <- sqlQuery(lcon, "select SYMBOL, FUND from ETF_META where ASSET_CLASS='EQUITY' and INVERSE=0 and LEVERAGED=0 and ETN=0 and GEO='U.S.' order by SYMBOL")
	
	frml <- formula(sprintf("etf ~ %s", paste(factorIds, collapse = " + ")))
	
	for(i in 1:nrow(etfs)){
		ticker <- etfs$SYMBOL[i]
		
		if(!is.na(commandFlag) && commandFlag == 'DELTA'){
			ctr <- sqlQuery(lconUs2, sprintf("select count(*) from FAMA_FRENCH_REGRESSION where KEY_ID = '%s' and KEY_TYPE = '%s'", ticker, 'ETF'))[[1]]
			if (ctr > 0) next
		}
		
		iName <- etfs$FUND[i]
		
		bDf <- sqlQuery(lconUs2, sprintf("select c, time_stamp from BHAV_EQ_TD where symbol='%s' and time_stamp <= '%s' and time_stamp >= '%s' order by time_stamp", ticker, maxDt, Sys.Date() - 10*365))
		if(nrow(bDf) < 500) next
		
		print(ticker)
		
		bXts <- xts(bDf[,1], bDf[,2])
		bRet <- dailyReturn(bXts)

		bRetDf <- data.frame(bRet)
		colnames(bRetDf) <- c('etf')
		bRetDf$time_stamp <- index(bRet)
		bRetDf <- bRetDf[min( which ( bRetDf$etf != 0 )):nrow(bRetDf),]

		regDf <- bRetDf %>% inner_join(retDf, by='time_stamp') %>% as.data.frame()
		
		lmmodel <- lm(frml, tail(regDf, 500))
		
		residCumRet <- Return.cumulative(lmmodel$residuals) * 100
		residSd <- sd(lmmodel$residuals * 100)
		coeffs <- coef(lmmodel) * 100
		rsqd <- summary(lmmodel)$r.squared
		
		sqlQuery(lconUs2, sprintf("delete from FAMA_FRENCH_REGRESSION where KEY_ID = '%s' and KEY_TYPE = '%s'", ticker, 'ETF'))
		
		sqlQuery(lconUs2, sprintf("insert into FAMA_FRENCH_REGRESSION (KEY_ID, KEY_TYPE, TIME_STAMP, INTERCEPT, MKT_RF, SMB, HML, RMW, CMA, MOM, R_SQD, RESIDUAL_CUMRET, RESIDUAL_SD) 
									values ('%s', '%s', '%s', %f, %f, %f, %f, %f, %f, %f, %f, %f, %f)",
										ticker, 'ETF', maxDt,
										coeffs[[1]], coeffs[[2]], coeffs[[3]], coeffs[[4]], coeffs[[5]], coeffs[[6]], coeffs[[7]], 
										rsqd, residCumRet, residSd))
					
	}
}

doEtfs()
doFfIndustry()
render("rp-fama-french-regression.Rmd", output_file="rp-fama-french-regression.html")
