library('RODBC')
library('RPostgres')
library('tidyverse')
library('rmarkdown')

#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/Pandoc")
#source("/mnt/hollandC/StockViz/R/config.r")
#source("/mnt/data/blog/common/plot.common.r")

source("/mnt/hollandr/config.r")
source("/mnt/hollandr/plot.common.r")

source("../common/fan-chart.R")

plotPath <- "risk/plots"
idPath <- "../"

args = commandArgs(TRUE)
commandFlag <- args[1]

library('quantmod')
library('PerformanceAnalytics')

library('lubridate')
library('ggthemes')
library('reshape2')
library('viridis')
#library('DT')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pgCon <- dbConnect(RPostgres::Postgres(), host = 'sweden', user = ldbuser2, password = ldbpassword2, dbname = 'StockVizDyn', sslmode = 'allow')

benchmarks <- c('NIFTY 50 TR',
                'NIFTY MIDCAP 150 TR',
                'NIFTY SMALLCAP 250 TR',
                'NIFTY MICROCAP 250 TR')

smaLbs <- c(20, 50, 100, 200) #days

maxDt <- sqlQuery(lcon, "select max(time_stamp) from px_history") [[1]]

createSmaStat <- function(){
	smaStats <- data.frame(matrix(c("", smaLbs), nrow=1))
	colnames(smaStats) <- c("SYMBOL", sapply(smaLbs, function(X) paste0("SMA_", X)))
	
	tickers <- dbGetQuery(pgCon, "select distinct ticker from eod_adjusted_nse where date_stamp = $1 order by ticker", params = list(maxDt))[,1]
	
	for(i in 1:length(tickers)){
		ticker <- tickers[i]
		
		iDf1 <- dbGetQuery(pgCon, "select c, date_stamp from eod_adjusted_nse where ticker = $1", params = list(ticker))
		iXts <- xts(iDf1[,1], iDf1[,2])
		
		iMinDate <- as.Date(sprintf("%d-01-01", year(first(index(iXts)))+1))
		iXts <- iXts[paste0(iMinDate, '/')]
		
		retDaily <- dailyReturn(iXts)

		tryCatch({
			allXts <- merge(iXts, stats::lag(retDaily, -1))
			names(allXts) <- c(ticker, 'BH')

			smaLoRets <- NULL
			for(smaLb in smaLbs){
				smaLoRets <- merge.xts(smaLoRets, ifelse(allXts[,1] > SMA(allXts[,1], smaLb), allXts$BH, 0))
			}
			names(smaLoRets) <- sapply(smaLbs, function(X) paste0('SMA_', X))
			
			smaIR <- unlist(lapply(1:ncol(smaLoRets), function(X) as.numeric(UpsidePotentialRatio(smaLoRets[,X]))))
			smaStats <- rbind(smaStats, c(ticker, smaIR))
		}, error = function(e) {})
	}
	smaStats <- smaStats[-1,]
	save(smaStats, file="smaStats.Rdata")
}

plottedTickers <- c()
createPlots <- function(isDelta = F){
  tickers <- dbGetQuery(pgCon, "select distinct ticker from eod_adjusted_nse where date_stamp = $1 order by ticker", params = list(maxDt))[,1]
	metaDf <- sqlQuery(lcon, "select SYMBOL, NAME from equity_ticker")
	
	if(isDelta){
		plotFiles <- list.files(plotPath, pattern="*.png")
		plottedTickers <- unique(unlist(lapply(strsplit(plotFiles, ".", fixed=T), `[[`, 2)))
		tickers <- setdiff(tickers, plottedTickers)
	}	
	
	bSyms <- benchmarks
	bRetDaily <- NULL
	bRetAnn <- NULL
	for(betf in bSyms){
		bDf <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name='%s'", betf))
		bXts <- xts(bDf[,1], bDf[,2])
		bRetDaily <- merge.xts(bRetDaily, dailyReturn(bXts))
		bRetAnn <- merge.xts(bRetAnn, annualReturn(bXts))
	}
	
	names(bRetDaily) <- bSyms
	names(bRetAnn) <- bSyms
	
	smaStats <- data.frame(matrix(c("", smaLbs), nrow=1))
	colnames(smaStats) <- c("SYMBOL", sapply(smaLbs, function(X) paste0("SMA_", X)))
	
	print(sprintf("processing %d tickers", length(tickers)))
	for(i in 1:length(tickers)){
		ticker <- tickers[i]
		fName <- paste0('in.', gsub("[^[:alnum:] ]| ", "", ticker))
		
		cat(ticker, "... ")
		
		tickerName <- metaDf[metaDf$SYMBOL == ticker, ]$NAME[1]

		tickerMaxDate <- dbGetQuery(pgCon, "select max(date_stamp) from eod_adjusted_nse where ticker = $1", params=list(ticker))[[1]]

		if (is.finite(tickerMaxDate) && tickerMaxDate != maxDt){
			print("STALE TICKER PRICES!!!")
			next
		}

		tryCatch({
			iDf1 <- dbGetQuery(pgCon, "select c, date_stamp from eod_adjusted_nse where ticker = $1", params = list(ticker))
			iXts <- xts(iDf1[,1], iDf1[,2])
			
			if(year(first(index(iXts))) == year(Sys.Date())){
			  print("skipping ipo")
			  next
			}
			
			tickerMinDate <- as.Date(sprintf("%d-01-01", year(first(index(iXts)))+1))
			iXts <- iXts[paste0(tickerMinDate, '/')]

			retDaily <- dailyReturn(iXts)
			retAnn <- annualReturn(iXts)

			names(retDaily) <- c(ticker)
			names(retAnn) <- c(ticker)
			
			retDailyDf <- data.frame(retDaily)
			retDailyDf$T <- index(retDaily)
			
			benchDailyDf <- data.frame(bRetDaily)
			benchDailyDf$T <- index(bRetDaily)
			
			mergedDaily <- retDailyDf %>% inner_join(benchDailyDf, by='T') %>% filter(T >= tickerMinDate) %>% relocate(T)
			mergedDailyXts <- na.trim(xts(mergedDaily[, -1], mergedDaily[,1]), sides='left')
			
			Common.PlotCumReturns(mergedDailyXts, ticker, tickerName, #NULL)
			                      sprintf("%s/%s.cumret.png", plotPath, fName))
			
			tryCatch({
				allXts <- merge(iXts, stats::lag(retDaily, -1))
				names(allXts) <- c(ticker, 'BH')

				smaLoRets <- NULL
				for(smaLb in smaLbs){
					smaLoRets <- merge.xts(smaLoRets, ifelse(allXts[,1] > SMA(allXts[,1], smaLb), allXts$BH, 0))
				}
				names(smaLoRets) <- sapply(smaLbs, function(X) paste0('SMA_', X))
				
				smaIR <- unlist(lapply(1:ncol(smaLoRets), function(X) as.numeric(UpsidePotentialRatio(smaLoRets[,X]))))
				smaStats <- rbind(smaStats, c(ticker, smaIR))
				
				smaLoRets <- na.omit(merge(smaLoRets, allXts$BH))
				Common.PlotCumReturns(smaLoRets, sprintf("%s SMA Profile", ticker), "long-only", #NULL)
				                      sprintf("%s/%s.sma.cumret.png", plotPath, fName))
			}, error = function(e) {print(e)})
			
			retAnnDf <- data.frame(retAnn * 100)
			retAnnDf$Y <- year(index(retAnn))
			
			benchAnnDf <- data.frame(bRetAnn * 100)
			benchAnnDf$Y <- year(index(bRetAnn))
			
			toPlotAnn <- retAnnDf %>% inner_join(benchAnnDf, by='Y') %>% relocate(Y) %>% as.data.frame()
						
			maxYear <- length(unique(toPlotAnn$Y))
			toPlotAnn <- melt(toPlotAnn, id='Y')
			minRet <- min(toPlotAnn$value)
			toPlotAnn$Y <- factor(toPlotAnn$Y, levels = unique(toPlotAnn$Y))

			ggplot(toPlotAnn, aes(x=Y, y=value, fill=variable)) +
				theme_economist() +
				geom_bar(stat = "identity", position = position_dodge()) +
				geom_text_repel(aes(label = sprintf('%.2f', value)), position = position_dodge(0.9)) +
				scale_fill_viridis(discrete = TRUE) +
				labs(x = "Year", y="Returns (%)", fill="", color="", size="", 
				     title=sprintf("%s", ticker), 
				     subtitle=sprintf("Annual Returns [%s:%s]", first(index(retDaily)), last(index(retDaily))),
				     caption = '@StockViz') 
				
			ggsave(sprintf("%s/%s.annret.png", plotPath, fName), width=12, height=6)
				
			colnames(iDf1) <- c('c', 'time_stamp')
			fanPlot <- common.CreateFanChart(iDf1, ticker, sprintf("%d:%s", min(year(iDf1$time_stamp)), max(iDf1$time_stamp)))
			ggsave(sprintf("%s/%s.fan.png", plotPath, fName), fanPlot, width=12, height=6)
			
			plottedTickers <- c(plottedTickers, ticker)
		}, error=function(e){print(e)})
	}
	smaStats <- smaStats[-1,]
	save(smaStats, file="smaStats.Rdata")
}

renderTickers <- function(){
	if(length(plottedTickers) == 0){
		plotFiles <- list.files(plotPath, pattern="*.png")
		plottedTickers <- unique(unlist(lapply(strsplit(plotFiles, ".", fixed=T), `[[`, 2)))
	}
	
	for(i in 1:length(plottedTickers)){
		
		iName <- plottedTickers[i]
		fName <- paste0('in.', gsub("[^[:alnum:] ]| ", "", iName))
		
		print(iName)
		tryCatch({
			render("risk/rp-eq-in-risk.Rmd", output_file=paste0("in.", fName, ".html"), params=list(ticker = iName, fName = fName))
		}, error=function(e){print(e)})
	}
}

#renderTickers()
#q()

if(is.na(commandFlag) || is.null(commandFlag)){
	#print("creating plots...")
	#createPlots()
	print("rendering etfs...")
	renderTickers()

	#print("rendering master page...")
	#render("rp-risk.Rmd", output_file="rp-risk.html")
	q()
}

if(commandFlag == "MASTER_ONLY"){
	#render("rp-risk.Rmd", output_file="rp-risk.html")
	q()
}

if(commandFlag == "STAT_ONLY"){
	createSmaStat()
	q()
}

if(commandFlag == "DELTA"){
	print("creating plots...")
	createPlots(T)
	plottedTickers <- c()
	print("rendering etfs...")
	renderTickers()

	#print("rendering master page...")
	#render("rp-risk.Rmd", output_file="rp-risk.html")
	q()
}