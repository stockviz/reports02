library('RODBC')
library('RPostgres')
library('tidyverse')
library('rmarkdown')
library('patchwork')
library('geomtextpath')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('ggthemes')
library('reshape2')
library('viridis')
#library('DT')
library('ggrepel')

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

options("scipen"=100)
options(stringsAsFactors = FALSE)

print("opening norway...")
lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

print("opening sweden...")
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
		
		iDf1 <- dbGetQuery(pgCon, "select c, date_stamp from eod_adjusted_nse where ticker = $1 and date_stamp >= '2010-01-01'", params = list(ticker))
		iXts <- xts(iDf1[,1], iDf1[,2])
		
		if(year(first(index(iXts))) == year(Sys.Date())){
		  print("skipping ipo")
		  next
		}
		
		iMinDate <- as.Date(sprintf("%d-01-01", year(first(index(iXts)))+1))
		iMinDate <- max(iMinDate, as.Date('2010-01-01'))
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

		#price charts
		tryCatch({
			iDf1 <- dbGetQuery(pgCon, "select c, date_stamp from eod_adjusted_nse where ticker = $1 and date_stamp >= '2010-01-01'", params = list(ticker))
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
		
		#fundamental charts
		tryCatch({
		  mainTitle <- sprintf("%s Quarterly EBIT", ticker)
		  revenueDf <- sqlQuery(lcon, sprintf("select * from FUNDA_XBRL 
                                    where symbol = '%s' 
                                    and period_type='quarterly'
                                    and ITEM_KEY = 'EBIT'",
		                                      ticker))
		  
		  if(nrow(revenueDf) == 0){
		    revenueDf <- sqlQuery(lcon, sprintf("select * from FUNDA_XBRL 
                                    where symbol = '%s' 
                                    and period_type='quarterly'
                                    and ITEM_KEY = 'Income'",
		                                        ticker))
		    
		    mainTitle <- sprintf("%s Quarterly Income", ticker)
		  }
		  p1 <- NULL
		  p2 <- NULL
		  
		  if(nrow(revenueDf |> filter(IS_CONSOLIDATED == 1)) > 0 
		     && nrow(revenueDf |> filter(IS_CONSOLIDATED == 0)) > 0){
		    revenueTb <- revenueDf |> mutate(CONSOL=if_else(IS_CONSOLIDATED == 0, 'CONSOL', 'NON_CONSOL')) |>
		      select(PERIOD_END, CONSOL, IS_AUDITED, ITEM_VAL) |>
		      group_by(PERIOD_END, CONSOL) |>
		      summarise(VAL = mean(ITEM_VAL)/10000) |>
		      pivot_wider(id_cols = PERIOD_END, names_from = CONSOL, values_from = VAL) |>
		      ungroup() |>
		      mutate(CONSOL_GROWTH = 100*(CONSOL/lag(CONSOL) - 1),
		             NON_CONSOL_GROWTH = 100*(NON_CONSOL/lag(NON_CONSOL) - 1))
		    
		    p1 <- revenueTb |> select(PERIOD_END, CONSOL, NON_CONSOL) |>
		      pivot_longer(-PERIOD_END) |>
		      ggplot(aes(x=PERIOD_END, y=value, fill = factor(name))) +
		      theme_economist() +
		      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		      scale_fill_viridis_d() +
		      geom_bar(stat='identity', position = 'dodge2') +
		      scale_x_date(date_breaks = '3 months', date_labels = '%Y-%b') +
		      labs(x = '', y = '(Rs. crore)', color='', fill='')
		    
		    p2 <- revenueTb |> select(PERIOD_END, CONSOL_GROWTH, NON_CONSOL_GROWTH) |>
		      rename(CONSOL = CONSOL_GROWTH, NON_CONSOL = NON_CONSOL_GROWTH) |>
		      pivot_longer(-PERIOD_END) |>
		      ggplot(aes(x=PERIOD_END, y=value, fill = factor(name))) +
		      theme_economist() +
		      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		      scale_fill_viridis_d() +
		      geom_bar(stat='identity', position = 'dodge2') +
		      guides(fill='none') +
		      scale_x_date(date_breaks = '3 months', date_labels = '%Y-%b') +
		      labs(x = '', y = 'QoQ Change (%)', color='', fill='')
		    
		  } else {
		    barColor <- viridis_pal()(2)[1]
		    revenueTb <- revenueDf |> 
		      select(PERIOD_END, IS_AUDITED, ITEM_VAL) |>
		      group_by(PERIOD_END) |>
		      summarise(VAL = mean(ITEM_VAL)/10000) |>
		      mutate(GROWTH = 100*(VAL/lag(VAL) - 1))
		    
		    p1 <- revenueTb |> 
		      ggplot(aes(x=PERIOD_END, y=VAL)) +
		      theme_economist() +
		      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		      scale_fill_viridis_d() +
		      geom_bar(stat='identity', position = 'dodge2', fill = barColor) +
		      scale_x_date(date_breaks = '3 months', date_labels = '%Y-%b') +
		      labs(x = '', y = '(Rs. crore)', color='', fill='')
		    
		    p2 <- revenueTb |> 
		      ggplot(aes(x=PERIOD_END, y=GROWTH)) +
		      theme_economist() +
		      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		      scale_fill_viridis_d() +
		      geom_bar(stat='identity', position = 'dodge2', fill = barColor) +
		      guides(fill='none') +
		      scale_x_date(date_breaks = '3 months', date_labels = '%Y-%b') +
		      labs(x = '', y = 'QoQ Change (%)', color='', fill='')
		  }
		  
		  p1 / p2 + plot_layout(axes = "collect_x") + 
		    plot_annotation(title = mainTitle, 
		                    subtitle = sprintf("%s:%s", min(revenueTb$PERIOD_END), max(revenueTb$PERIOD_END)),
		                    theme = theme_economist(),
		                    caption = '@StockViz')
		  
		  ggsave(sprintf("%s/%s.ebit.png", plotPath, fName), width=12, height=12)
		}, error=function(e){print(e)})
		
		################
		
		tryCatch({
		  mainTitle <- sprintf("%s Quarterly Revenue From Operations", ticker)
		  revenueDf <- sqlQuery(lcon, sprintf("select * from FUNDA_XBRL 
                                    where symbol = '%s' 
                                    and period_type='quarterly'
                                    and ITEM_KEY = 'RevenueFromOperations'",
		                                  ticker))
		  
		  if(nrow(revenueDf) == 0){
		    mainTitle <- sprintf("%s Quarterly Revenue on Investments", ticker)
		    revenueDf <- sqlQuery(lcon, sprintf("select * from FUNDA_XBRL 
                                    where symbol = '%s' 
                                    and period_type='quarterly'
                                    and ITEM_KEY = 'RevenueOnInvestments'",
		                                        ticker))
		  }
		  p1 <- NULL
		  p2 <- NULL
		  
		  if(nrow(revenueDf |> filter(IS_CONSOLIDATED == 1)) > 0 
		     && nrow(revenueDf |> filter(IS_CONSOLIDATED == 0)) > 0){
  		  revenueTb <- revenueDf |> mutate(CONSOL=if_else(IS_CONSOLIDATED == 0, 'CONSOL', 'NON_CONSOL')) |>
  		    select(PERIOD_END, CONSOL, IS_AUDITED, ITEM_VAL) |>
  		    group_by(PERIOD_END, CONSOL) |>
  		    summarise(VAL = mean(ITEM_VAL)/10000) |>
  		    pivot_wider(id_cols = PERIOD_END, names_from = CONSOL, values_from = VAL) |>
  		    ungroup() |>
  		    mutate(CONSOL_GROWTH = 100*(CONSOL/lag(CONSOL) - 1),
  		           NON_CONSOL_GROWTH = 100*(NON_CONSOL/lag(NON_CONSOL) - 1))
  
  	    p1 <- revenueTb |> select(PERIOD_END, CONSOL, NON_CONSOL) |>
  	      pivot_longer(-PERIOD_END) |>
  	      ggplot(aes(x=PERIOD_END, y=value, fill = factor(name))) +
  	      theme_economist() +
  	      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  	      scale_fill_viridis_d() +
  	      geom_bar(stat='identity', position = 'dodge2') +
  	      scale_x_date(date_breaks = '3 months', date_labels = '%Y-%b') +
  	      labs(x = '', y = '(Rs. crore)', color='', fill='')
  		  
  	    p2 <- revenueTb |> select(PERIOD_END, CONSOL_GROWTH, NON_CONSOL_GROWTH) |>
  	      rename(CONSOL = CONSOL_GROWTH, NON_CONSOL = NON_CONSOL_GROWTH) |>
  	      pivot_longer(-PERIOD_END) |>
  	      ggplot(aes(x=PERIOD_END, y=value, fill = factor(name))) +
  	      theme_economist() +
  	      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  	      scale_fill_viridis_d() +
  	      geom_bar(stat='identity', position = 'dodge2') +
  	      guides(fill='none') +
  	      scale_x_date(date_breaks = '3 months', date_labels = '%Y-%b') +
  	      labs(x = '', y = 'QoQ Change (%)', color='', fill='')

		  } else {
		    barColor <- viridis_pal()(2)[1]
		    revenueTb <- revenueDf |> 
		      select(PERIOD_END, IS_AUDITED, ITEM_VAL) |>
		      group_by(PERIOD_END) |>
		      summarise(VAL = mean(ITEM_VAL)/10000) |>
		      mutate(GROWTH = 100*(VAL/lag(VAL) - 1))
		    
		    p1 <- revenueTb |> 
		      ggplot(aes(x=PERIOD_END, y=VAL)) +
		      theme_economist() +
		      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		      scale_fill_viridis_d() +
		      geom_bar(stat='identity', position = 'dodge2', fill = barColor) +
		      scale_x_date(date_breaks = '3 months', date_labels = '%Y-%b') +
		      labs(x = '', y = 'Revenue (Rs. crore)', color='', fill='')
		    
		    p2 <- revenueTb |> 
		      ggplot(aes(x=PERIOD_END, y=GROWTH)) +
		      theme_economist() +
		      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		      scale_fill_viridis_d() +
		      geom_bar(stat='identity', position = 'dodge2', fill = barColor) +
		      guides(fill='none') +
		      scale_x_date(date_breaks = '3 months', date_labels = '%Y-%b') +
		      labs(x = '', y = 'QoQ Change (%)', color='', fill='')
		  }
		  
		  p1 / p2 + plot_layout(axes = "collect_x") + 
		    plot_annotation(title = mainTitle, 
		                    subtitle = sprintf("%s:%s", min(revenueTb$PERIOD_END), max(revenueTb$PERIOD_END)),
		                    theme = theme_economist(),
		                    caption = '@StockViz')
	    
		  ggsave(sprintf("%s/%s.RevenueFromOperations.png", plotPath, fName), width=12, height=12)
		}, error=function(e){print(e)})
		
		################
		
		tryCatch({
		  marginAmt <- sqlQuery(lcon, sprintf("select TIME_STAMP, TOT_FINANCED_LAKHS from MTF_REPORT where symbol = '%s'", ticker))
		  if (nrow(marginAmt) == 0) stop("no rows in MTF_REPORT")
		  
		  ffMktCap <- sqlQuery(lcon, sprintf("select TIME_STAMP, FF_MKT_CAP_CR from equity_misc_info where symbol = '%s'", ticker))
		  if (nrow(ffMktCap) == 0) stop("no rows in equity_misc_info")
		  
		  marginXts <- xts(marginAmt[,2], marginAmt[,1])
		  ffMktCapXts <- xts(ffMktCap[,2], ffMktCap[,1])
		  
		  allXts <- merge(marginXts, ffMktCapXts)
		  names(allXts) <- c('AMT', 'FF')
		  allXts$FF <- na.locf(allXts$FF)
		  allXts$FF <- na.locf(allXts$FF, fromLast = TRUE)
		  allXts <- na.omit(allXts)
		  
		  mtfPct <- data.frame(allXts) |> 
		    mutate(TIME_STAMP = index(allXts), 
		           FF = if_else(TIME_STAMP >= '2024-03-01', FF * 100, FF), 
		           FF_PCT = 100*AMT/(100*FF))
		  
		  labelData <- mtfPct |> filter(TIME_STAMP == max(TIME_STAMP)) |>
		    mutate(label1 = format(AMT, big.mark = ','), label2 = paste0(round(FF_PCT, 2), '%'))
		  
		  lineColor <- viridis_pal()(2)[1]
		  
		  monthsData <- as.numeric(max(mtfPct$TIME_STAMP) - min(mtfPct$TIME_STAMP))/30
		  labelInterval <- 1
		  if (monthsData > 24 && monthsData < 48) {
		    labelInterval <- 3  
		  } else if (monthsData > 48) {
		    labelInterval <- 6  
		  }
		  
      p1 <- ggplot(mtfPct, aes(x=TIME_STAMP, y = AMT)) +
        theme_economist() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        geom_line(color = lineColor) +
        geom_text_repel(data = labelData, aes(label = label1), color='black') +
        scale_y_log10() +
        scale_x_date(date_breaks = sprintf('%d months', labelInterval), date_labels = '%Y-%b') +
        labs(x = '', y = 'Total Financed (log Rs. Lakhs)', color='', fill='',
             title = 'Amount (Lakhs)')
      
      p2 <- ggplot(mtfPct, aes(x=TIME_STAMP, y = FF_PCT)) +
        theme_economist() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        geom_line(color = lineColor) +
        geom_text_repel(data = labelData, aes(label = label2), color='black') +
        scale_x_date(date_breaks = sprintf('%d months', labelInterval), date_labels = '%Y-%b') +
        labs(x = '', y = 'Total Financed of Freefloat(%)', color='', fill='',
             title = 'Freefloat')
      
      p1 / p2 + plot_layout(axes = "collect_x") + 
        plot_annotation(title = sprintf("%s Total Financed under MTF", ticker), 
                        subtitle = sprintf("%s:%s", min(mtfPct$TIME_STAMP), max(mtfPct$TIME_STAMP)),
                        theme = theme_economist(),
                        caption = '@StockViz')
      
      ggsave(sprintf("%s/%s.mtf.png", plotPath, fName), width=12, height=12)
      
		}, error=function(e){print(e)})
		
		################
		
		tryCatch({
		  indianFunds <- sqlQuery(lcon, sprintf("select as_of, sum(v) from SHARE_HOLDING_PATTERN_XBRL
		                                        where (k1 = 'VentureCapitalFunds' or k1 = 'AlternativeInvestmentFunds' or k1 = 'MutualFundsOrUti')
		                                        and symbol ='%s'
		                                        and k2='PercentageOfTotalVotingRights' 
		                                        group by as_of", ticker))
		  
		  indianIndividuals <- sqlQuery(lcon, sprintf("select as_of, sum(v) from SHARE_HOLDING_PATTERN_XBRL
		                                        where k1 like '%%IndividualShareholders%%'
		                                        and symbol ='%s'
		                                        and k2='PercentageOfTotalVotingRights' 
		                                        group by as_of", ticker))
		  
		  foreignFunds <- sqlQuery(lcon, sprintf("select as_of, sum(v) from SHARE_HOLDING_PATTERN_XBRL
		                                        where (k1 = 'ForeignVentureCapitalFunds' or k1 = 'ForeignPortfolioInvestor')
		                                        and symbol ='%s'
		                                        and k2='PercentageOfTotalVotingRights' 
		                                        group by as_of", ticker))
		  
		  promoter <- sqlQuery(lcon, sprintf("select as_of, sum(v) from SHARE_HOLDING_PATTERN_XBRL
		                                        where k1 = 'ShareholdingOfPromoterAndPromoterGroup'
		                                        and symbol ='%s'
		                                        and k2='PercentageOfTotalVotingRights' 
		                                        group by as_of", ticker))
		  
		  indianGovt <- sqlQuery(lcon, sprintf("select as_of, sum(v) from SHARE_HOLDING_PATTERN_XBRL
		                                        where (k1 = 'CentralGovernmentOrPresidentOfIndia' or k1 = 'StateGovernmentsOrGovernors' or k1 = 'CentralGovernmentOrStateGovernmentS')
		                                        and symbol ='%s'
		                                        and k2='PercentageOfTotalVotingRights' 
		                                        group by as_of", ticker))
		  
		  allPct <- indianFunds |> full_join(indianIndividuals, join_by(as_of)) |> 
		    full_join(foreignFunds, join_by(as_of)) |> 
		    full_join(promoter, join_by(as_of)) |> 
		    full_join(indianGovt, join_by(as_of))
		    
		    
		  names(allPct) <- c('AsOf', 'IndianFunds', 'IndianIndividuals', 'ForeignFunds', 'Promoter', 'IndianGovt')
		  
		  tpPct <- allPct |> mutate(across(-AsOf, as.numeric)) |>
		    select_if(~ is.Date(.x) || sum(.x) > 0)
		  
		  if(nrow(tpPct) > 5){
		    labelData <- tpPct |> filter(AsOf == max(AsOf)) |>
		      pivot_longer(-AsOf) |>
		      mutate(label = paste0(value, '%'))
		    
  		  p1 <- tpPct |> pivot_longer(-AsOf) |>
  		    ggplot(aes(x = AsOf, y=value, color = name)) +
  		    theme_economist() +
  		    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  		    scale_color_viridis_d() +
  		    geom_line() +
  		    geom_text_repel(data = labelData, aes(label = label), color='black') +
  		    scale_y_log10() +
  		    scale_x_date(date_breaks = '6 months', date_labels = '%Y-%b') +
  		    labs(x = '', y = 'Ownership (log %)', color='', fill='',
  		         title = sprintf('%s Ownership', ticker),
  		         subtitle = sprintf("%s:%s", min(tpPct$AsOf), max(tpPct$AsOf)),
  		         caption = '@StockViz')
		  } else {
  		  p1 <- tpPct |> pivot_longer(-AsOf) |>
          ggplot(aes(x = factor(AsOf), y=value, fill = name)) +
    		    theme_economist() +
    		    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    		    scale_fill_viridis_d() +
    		    geom_bar(stat='identity', position = position_dodge()) +
  		      geom_text(aes(label = paste0(value, '%')), position = position_dodge(width = 0.9), vjust=-0.5) +
  		      scale_y_log10() +
    		    #scale_x_date(date_breaks = '1 months', date_labels = '%Y-%b') +
    		    labs(x = '', y = 'Ownership (log %)', color='', fill='',
    		         title = sprintf('%s Ownership', ticker),
    		         subtitle = sprintf("%s:%s", min(tpPct$AsOf), max(tpPct$AsOf)),
    		         caption = '@StockViz')
		  }
		  
		  ggsave(sprintf("%s/%s.ownership.png", plotPath, fName), plot=p1, width=12, height=12)
		}, error=function(e){print(e)})
		
		################
		tryCatch({
		  bankDf <- sqlQuery(lcon, sprintf("select period_end, item_key, item_val from FUNDA_XBRL a
                                    where symbol = '%s' 
                                    and period_type='quarterly'
                                    and ITEM_KEY in ('NIM', 'ROA', 'ROE', 
                                      'EFFICIENCY', 'GNPA_PCT', 'NNPA_PCT', 
		                                  'BVPS', 'ABVPS', 'ASSETS_LAKH_CR')
		                                and is_consolidated = (select min(convert(integer, is_consolidated)) from FUNDA_XBRL b 
		                                                        where b.symbol = a.symbol
		                                                        and b.period_end = a.period_end
		                                                        and b.item_key = a.item_key)
		                                order by period_end",
		                                  ticker))
		  
		  if(nrow(bankDf) > 0){
		    pDf <- bankDf |> filter(item_key %in% c('NIM', 'ROA', 'ROE', 'EFFICIENCY') & is.finite(item_val) & item_val != 0)
		    
		    leastRow <- (pDf |> 
		                   select(item_key, item_val) |> 
		                   group_by(item_key) |> 
		                   summarise(cnt = sum(if_else(item_val != 0, 1, 0))) |> 
		                   slice_min(cnt, n=1))[1,2][[1]]
		    
		    if(leastRow > 6){
  		    p1 <- ggplot(bankDf |> filter(item_key %in% c('NIM') & is.finite(item_val) & item_val != 0), aes(x=period_end, y = item_val)) +
  		      theme_economist() +
  		      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  		      scale_x_date(date_breaks = '6 months', date_labels = '%Y-%b') +
  		      geom_line() +
  		      labs(x = '', y = 'NIM (%)', subtitle = 'Net Interest Margin')
  		    
  		    p2 <- ggplot(bankDf |> filter(item_key == 'ROA' & is.finite(item_val) & item_val != 0), aes(x=period_end, y = item_val)) +
  		      theme_economist() +
  		      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  		      scale_x_date(date_breaks = '6 months', date_labels = '%Y-%b') +
  		      geom_line() +
  		      labs(x = '', y = 'ROA (%)', subtitle = 'Return on Assets')
  		    
  		    p3 <- ggplot(bankDf |> filter(item_key == 'ROE' & is.finite(item_val) & item_val != 0), aes(x=period_end, y = item_val)) +
  		      theme_economist() +
  		      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  		      scale_x_date(date_breaks = '6 months', date_labels = '%Y-%b') +
  		      geom_line() +
  		      labs(x = '', y = 'ROE (%)', subtitle = 'Return on Equity')
  		    
  		    p4 <- ggplot(bankDf |> filter(item_key == 'EFFICIENCY' & is.finite(item_val) & item_val != 0), aes(x=period_end, y = item_val)) +
  		      theme_economist() +
  		      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  		      scale_x_date(date_breaks = '6 months', date_labels = '%Y-%b') +
  		      geom_line() +
  		      labs(x = '', y = 'Efficiency (%)', subtitle = 'Efficiency')
  		    
  		    p1/p2/p3/p4 + plot_layout(axes = "collect_x") + 
  		      plot_annotation(title = sprintf("%s Key Metrics", ticker), 
  		                      subtitle = sprintf("%s:%s", min(bankDf$period_end), max(bankDf$period_end)),
  		                      theme = theme_economist(),
  		                      caption = '@StockViz')
  		    
  		    ggsave(sprintf("%s/%s.metric-1.png", plotPath, fName), width=12, height=6*4)
		    }
		    
		    pDf <- bankDf |> filter(item_key %in% c('GNPA_PCT', 'NNPA_PCT', 'ASSETS_LAKH_CR') & is.finite(item_val) & item_val != 0)
		    
		    leastRow <- (pDf |> 
		                   select(item_key, item_val) |> 
		                   group_by(item_key) |> 
		                   summarise(cnt = sum(if_else(item_val != 0, 1, 0))) |> 
		                   slice_min(cnt, n=1))[1,2][[1]]
		    
		    if(leastRow > 6){
  		    p1 <- ggplot(bankDf |> filter(item_key == 'GNPA_PCT' & is.finite(item_val) & item_val != 0), aes(x=period_end, y = item_val)) +
  		      theme_economist() +
  		      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  		      scale_x_date(date_breaks = '6 months', date_labels = '%Y-%b') +
  		      geom_line() +
  		      labs(x = '', y = 'GNPA (%)', subtitle = 'Gross Non-Performing Assets')
  		    
  		    p2 <- ggplot(bankDf |> filter(item_key == 'NNPA_PCT' & is.finite(item_val) & item_val != 0), aes(x=period_end, y = item_val)) +
  		      theme_economist() +
  		      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  		      scale_x_date(date_breaks = '6 months', date_labels = '%Y-%b') +
  		      geom_line() +
  		      labs(x = '', y = 'NNPA (%)', subtitle = 'Net Non-Performing Assets')
  		    
  		    p3 <- ggplot(bankDf |> filter(item_key == 'ASSETS_LAKH_CR' & is.finite(item_val) & item_val != 0), aes(x=period_end, y = item_val)) +
  		      theme_economist() +
  		      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  		      scale_x_date(date_breaks = '6 months', date_labels = '%Y-%b') +
  		      geom_line() +
  		      labs(x = '', y = 'Assets (Lakh Cr.)', subtitle = 'Total Assets')
  		    
  		    p1/p2/p3 + plot_layout(axes = "collect_x") + 
  		      plot_annotation(title = sprintf("%s Asset Metrics", ticker), 
  		                      subtitle = sprintf("%s:%s", min(bankDf$period_end), max(bankDf$period_end)),
  		                      theme = theme_economist(),
  		                      caption = '@StockViz')
  		    
  		    ggsave(sprintf("%s/%s.metric-2.png", plotPath, fName), width=12, height=6*3)
		    }
		    
		    pDf <- bankDf |> filter(item_key %in% c('BVPS', 'ABVPS') & is.finite(item_val) & item_val != 0)
		    
		    leastRow <- (pDf |> 
		                   select(item_key, item_val) |> 
		                   group_by(item_key) |> 
		                   summarise(cnt = sum(if_else(item_val != 0, 1, 0))) |> 
		                   slice_min(cnt, n=1))[1,2][[1]]
		    
		    if(leastRow > 6){
		      startPeriodEnd <- min(pDf$period_end)
		      priceDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from px_history 
		                                        where symbol = '%s' 
		                                        and time_stamp >= '%s'
		                                        and series in ('eq', 'be')",
		                                        ticker, startPeriodEnd))
		      priceXts <- xts(priceDf[,2], priceDf[,1])
		      
		      abvpsDf <- pDf |> filter(item_key == 'ABVPS') |> select(period_end, item_val)
		      abvpsXts <- xts(abvpsDf$item_val, abvpsDf$period_end)
		      
		      p2b <- merge(priceXts, abvpsXts)
		      names(p2b) <- c('PX', 'ABVPS')
		      p2b$ABVPS <- na.locf(p2b$ABVPS)
		      p2b$MUL <- p2b$PX/p2b$ABVPS
		      
		      p2bDf <- data.frame(p2b$MUL)
		      p2bDf$ts <- index(p2b)
		      
		      bvMultiple <- p2bDf |> full_join(abvpsDf, join_by(ts == period_end)) |> 
		        mutate(period_end = if_else(!is.na(item_val), ts, NA)) |>
		        fill(period_end, period_end, .direction='updown') |>
		        select(period_end, MUL)
		      
  		    p1 <- ggplot(pDf, aes(x=period_end, y = item_val, color=item_key)) +
  		      theme_economist() +
  		      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  		      scale_x_date(date_breaks = '6 months', date_labels = '%Y-%b') +
  		      geom_line() +
  		      scale_color_viridis_d() +
  		      labs(x = '', y = 'Book Value per Share', color = '')
  		    
  		    p2 <- ggplot(bvMultiple, aes(x=period_end, y = MUL, group = period_end)) +
  		      theme_economist() +
  		      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  		      scale_x_date(date_breaks = '6 months', date_labels = '%Y-%b') +
  		      geom_violin() +
  		      labs(x = '', y = 'Book Value Multiple', subtitle = 'Adjusted Multiple')
  		    
  		    p1/p2 + plot_layout(axes = "collect_x") + 
  		      plot_annotation(title = sprintf("%s Book Value", ticker), 
  		                      subtitle = sprintf("%s:%s", min(bankDf$period_end), max(bankDf$period_end)),
  		                      theme = theme_economist(),
  		                      caption = '@StockViz')
  		    
  		    ggsave(sprintf("%s/%s.book-value.png", plotPath, fName), width=12, height=6*2)
		    }
		    
		  }
		}, error=function(e){print(e)})
		  
		
		tryCatch({
		  epsDf <- sqlQuery(lcon, sprintf("select * from FUNDA_XBRL 
                                    where symbol = '%s' 
                                    and period_type='quarterly'
                                    and ITEM_KEY like 'eps%%'",
		                                ticker))
		  
		  p1 <- NULL
		  p2 <- NULL
		  
		  startPeriod <- min(epsDf$PERIOD_END)
		  endPeriod <- max(epsDf$PERIOD_END) #endPeriod <- as.Date("2025-09-30")
		  corpActs <- sqlQuery(lcon, sprintf("select EX_DATE, PURPOSE from CORP_ACTION
		                                     where SYMBOL = '%s'
		                                     and EX_DATE >= '%s'
		                                     and EX_DATE <= '%s'
		                                     and (
		                                      PURPOSE like '%%merg%%'
		                                      or PURPOSE like '%%bonus%%'
		                                      or PURPOSE like '%%split%%'
		                                      or PURPOSE like '%%splt%%'
		                                      or PURPOSE like '%%amal%%'
		                                      or PURPOSE like '%%right%%'
		                                     )",
		                                     ticker, startPeriod, endPeriod))
		  
		  corpActs <- corpActs |> group_by(EX_DATE) |> 
		    summarise(REASON = str_c(PURPOSE, collapse = ' & ')) |>
		    ungroup()
		  
		  if(nrow(epsDf |> filter(IS_CONSOLIDATED == 1)) > 0){
  		  p1 <- epsDf |> filter(IS_CONSOLIDATED == 1) |>
  		    select(PERIOD_END, IS_AUDITED, ITEM_KEY, ITEM_VAL) |>
  		    group_by(PERIOD_END, ITEM_KEY) |>
  		    summarise(VAL = mean(ITEM_VAL)) |> 
  		    ggplot(aes(x=PERIOD_END, y=VAL, color = ITEM_KEY)) +
  		    theme_economist() +
  		    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  		    scale_color_viridis_d() +
  		    geom_line() +
  		    scale_x_date(date_breaks = '3 months', date_labels = '%Y-%b') +
  		    geom_textvline(data = corpActs, mapping = aes(xintercept = EX_DATE, label = REASON), 
  		                   linetype = "dashed", size=3) +
  		    labs(x = '', y = 'EPS (Rs.)', color='', title = 'Consolidated')
		  }
		  
		  if(nrow(epsDf |> filter(IS_CONSOLIDATED == 0)) > 0){
        p2 <- epsDf |> filter(IS_CONSOLIDATED == 0) |>
  		      select(PERIOD_END, IS_AUDITED, ITEM_KEY, ITEM_VAL) |>
  		      group_by(PERIOD_END, ITEM_KEY) |>
  		      summarise(VAL = mean(ITEM_VAL)) |> 
  		      ggplot(aes(x=PERIOD_END, y=VAL, color = ITEM_KEY)) +
  		        theme_economist() +
  		        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  		        scale_color_viridis_d() +
  		        geom_line() +
  		        scale_x_date(date_breaks = '3 months', date_labels = '%Y-%b') +
              geom_textvline(data = corpActs, mapping = aes(xintercept = EX_DATE, label = REASON), 
                         linetype = "dashed", size=3) +
  		        labs(x = '', y = 'EPS (Rs.)', color='', title = 'Non-Consolidated')
		  }
		  
		  if(!is.null(p1) && !is.null(p2)){
        p1 / p2 + plot_layout(axes = "collect") + 
          plot_annotation(title = sprintf("%s Quarterly Earnings Per Share", ticker), 
                          subtitle = sprintf("%s:%s", min(epsDf$PERIOD_END), max(epsDf$PERIOD_END)),
                          theme = theme_economist(),
                          caption = '@StockViz')
  		  
        ggsave(sprintf("%s/%s.eps.png", plotPath, fName), width=12, height=12)
		  } else if(!is.null(p1)){
		    p1 + plot_annotation(title = sprintf("%s Quarterly Earnings Per Share", ticker), 
		                      subtitle = sprintf("%s:%s", min(epsDf$PERIOD_END), max(epsDf$PERIOD_END)),
		                      theme = theme_economist(),
		                      caption = '@StockViz')
		    
		    ggsave(sprintf("%s/%s.eps.png", plotPath, fName), width=12, height=6)
		  } else if(!is.null(p2)){
		    p2 + plot_annotation(title = sprintf("%s Quarterly Earnings Per Share", ticker), 
		                         subtitle = sprintf("%s:%s", min(epsDf$PERIOD_END), max(epsDf$PERIOD_END)),
		                         theme = theme_economist(),
		                         caption = '@StockViz')
		    
		    ggsave(sprintf("%s/%s.eps.png", plotPath, fName), width=12, height=6)
		  }
		  
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
			render("risk/rp-eq-in-risk.Rmd", output_file=paste0(fName, ".html"), params=list(ticker = iName, fName = fName))
		}, error=function(e){print(e)})
	}
}

#renderTickers()
#q()

if(is.na(commandFlag) || is.null(commandFlag)){
	print("creating plots...")
	createPlots()
	print("rendering etfs...")
	renderTickers()

	render("rp-margin.Rmd", output_file="rp-margin.html")
	q()
}

if(commandFlag == "MASTER_ONLY"){
	render("rp-margin.Rmd", output_file="rp-margin.html")
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

	render("rp-margin.Rmd", output_file="rp-margin.html")
	q()
}