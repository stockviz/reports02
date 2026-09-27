library('rmarkdown')
#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/Pandoc")

#render("rp-fama-french-factors.Rmd", output_file="rp-fama-french-factors.html")
#q()

library('RODBC')
library('tidyverse')

#source("C:/stockviz/r/config.r")
source("/mnt/hollandr/config.r")
#source("C:/stockviz/r/plot.common.r")
source("/mnt/hollandr/plot.common.r")

source("../common/fan-chart.R")

plotPath <- "analysis/plots-fama-fench"

pdf(NULL)

library('quantmod')
library('PerformanceAnalytics')
#library('tidyquant')

library('lubridate')
library('ggthemes')
library('reshape2')
library('viridis')
#library('DT')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)

lconUs2 <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

################# factors charts

startDt <- as.Date("1980-01-01")
maxDt <- sqlQuery(lconUs2, "select max(time_stamp) from FAMA_FRENCH_5_FACTOR_DAILY") [[1]]
retTypes <- sqlQuery(lconUs2, "select distinct ret_type from FAMA_FRENCH_5_FACTOR_DAILY")[,1]

factorDf <- sqlQuery(lconUs2, sprintf("select time_stamp, key_id, ret from FAMA_FRENCH_5_FACTOR_DAILY where time_stamp >= '%s' and ret_type = '%s' order by time_stamp", startDt, retTypes[1]))
momDf <- sqlQuery(lconUs2, sprintf("select time_stamp, ret as MOM from FAMA_FRENCH_MOMENTUM_DAILY where key_id='MOM' and time_stamp >= '%s' and ret_type = 'M' order by time_stamp", startDt))

retDf <- factorDf %>% mutate(ret=ret/100) %>% pivot_wider(names_from=key_id, values_from=ret) %>% select(!RF) %>% inner_join(momDf, by='time_stamp') %>% mutate(MOM = MOM/100) %>% as.data.frame()

factorIds <- names(retDf)['time_stamp' != names(retDf)]

retXts <- xts(retDf[,-1], retDf[,1])

annRetsAll <- do.call(cbind, lapply(retXts, function(X) apply.yearly(X, Return.cumulative)))

toPlot <- data.frame(annRetsAll)
toPlot$Y <- year(index(annRetsAll))
toPlot <- melt(toPlot, id='Y')
toPlot$value <- 100*toPlot$value
toPlot$Y <- factor(toPlot$Y, levels=unique(toPlot$Y))
toPlot$variable <- factor(toPlot$variable, levels=unique(toPlot$variable))

toPlot$color <- NA
reds <- colorRampPalette(c("darkred", "#FFFDD0"))(nrow(toPlot[!is.na(toPlot$value) & toPlot$value < 0,]))
greens <- colorRampPalette(c("#FFFDD0", "darkgreen"))(nrow(toPlot[!is.na(toPlot$value) & toPlot$value >= 0,]))
toPlot[order(toPlot$value),]$color <- c(reds, greens, rep(NA, nrow(toPlot) - length(reds) - length(greens)))

ggplot(toPlot, aes(y=variable, x=Y, fill=color)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_tile() +
	geom_text(aes(label= ifelse(is.finite(value), sprintf("%+.2f%%", value), "")), hjust = "center", size=2.2) +
	scale_fill_identity() +
	guides(fill="none") +
	labs(x='', y='', title="Fama-French Factor Returns", subtitle=sprintf("Annual Returns [%s:%s]", min(factorDf$time_stamp), max(factorDf$time_stamp))) +
	annotate("text", x=1, y=1, label = "@StockViz", hjust='left', vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)

ggsave(sprintf("%s/fama-french-factors.all.png", plotPath), width=20, height=6)	

ffRetXts <- subset(retXts["2010/",], , !names(retXts) %in% 'MKT-RF')
Common.PlotCumReturns(ffRetXts, "Fama-French 5-Factors", "w/o Rm-Rf", sprintf("%s/fama-french-factors.cumret.png", plotPath))

toPlot <- data.frame(annRetsAll["2010/"])
toPlot$Y <- year(index(annRetsAll["2010/"]))
startYr <- min(toPlot$Y)
endYr <- max(toPlot$Y)
maxYear <- length(unique(toPlot$Y))
toPlot <- melt(toPlot, id='Y')
toPlot$value <- 100*toPlot$value
minRet <- min(toPlot$value)
toPlot$Y <- factor(toPlot$Y, levels=unique(toPlot$Y))

ggplot(toPlot, aes(x=Y, y=value, fill=variable)) +
	theme_economist() +
	geom_bar(stat = "identity", position = position_dodge()) +
	geom_text_repel(aes(label = sprintf('%.2f', value)), position = position_dodge(0.9)) +
	scale_fill_viridis(discrete = TRUE) +
	labs(x = "Year", y="Returns (%)", fill="", color="", size="", title="Fama-French Factor Returns", subtitle=sprintf("Annual Returns [%d:%d]", startYr, endYr)) +
	annotate("text", x=maxYear, y=minRet, label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
	
ggsave(sprintf("%s/fama-french-factors.annret.png", plotPath), width=12, height=6)

ffdesc <- read.csv("fama-french-factors.csv")

for(i in 1:length(factorIds)){
	fName <- factorIds[i]
	iName <- ffdesc[ffdesc$ABBR == factorIds[i],]$EXP[1]
	
	iDf1 <- retDf[, c(factorIds[i], 'time_stamp')]
	iDf1$px <- cumprod(1 + iDf1[,1])
	iDf1 <- iDf1[, c('px', 'time_stamp')]
	
	fanPlot <- common.CreateFanChart(iDf1, sprintf("%s: %s", fName, iName), sprintf("%d:%s", min(year(iDf1$time_stamp)), max(iDf1$time_stamp)))
	ggsave(sprintf("%s/factor.%s.fan.png", plotPath, fName), fanPlot, width=12, height=6)
}

#################### size factors

sorts <- list(teriles = c('HI_30', 'MED_40', 'LO_30'),
			quintiles = c('HI_20', 'QNT_2', 'QNT_3', 'QNT_4', 'LO_20'),
			deciles = c('HI_10', 'DEC_2', 'DEC_3', 'DEC_4', 'DEC_5', 'DEC_6', 'DEC_7', 'DEC_8', 'DEC_9', 'LO_10'))

bDf <- sqlQuery(lconUs2, sprintf("select KEY_ID, RET, TIME_STAMP from FAMA_FRENCH_SIZE_DAILY where RET_TYPE='AVWRD' and time_stamp >= '%s'", startDt))

ffdesc <- read.csv("fama-french-size.csv")

for(srt in names(sorts)){
	fctr <- sorts[[srt]]
	retDf <- bDf %>% filter(KEY_ID %in% fctr) %>% mutate(RET = RET/100) %>% pivot_wider(names_from = KEY_ID, values_from = RET) 
	toPlotXts <- xts(retDf[,fctr], retDf$TIME_STAMP)
	
	Common.PlotCumReturns(toPlotXts["2010/",], "Fama-French Size Sorts", srt, sprintf("%s/fama-french-size.%s.cumret.png", plotPath, srt))
	
	annRetsAll <- do.call(cbind, lapply(toPlotXts, function(X) apply.yearly(X, Return.cumulative)))
	
	toPlot <- data.frame(annRetsAll["2010/"])
	toPlot$Y <- year(index(annRetsAll["2010/"]))
	startYr <- min(toPlot$Y)
	endYr <- max(toPlot$Y)
	maxYear <- length(unique(toPlot$Y))
	toPlot <- melt(toPlot, id='Y')
	toPlot$value <- 100*toPlot$value
	minRet <- min(toPlot$value)
	toPlot$Y <- factor(toPlot$Y, levels=unique(toPlot$Y))

	ggplot(toPlot, aes(x=Y, y=value, fill=variable)) +
		theme_economist() +
		geom_bar(stat = "identity", position = position_dodge()) +
		geom_text_repel(aes(label = sprintf('%.2f', value)), position = position_dodge(0.9)) +
		scale_fill_viridis(discrete = TRUE) +
		labs(x = "Year", y="Returns (%)", fill="", color="", size="", title=sprintf("Fama-French Size Sorts (%s)", srt), subtitle=sprintf("Annual Returns [%d:%d]", startYr, endYr)) +
		annotate("text", x=maxYear, y=minRet, label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(sprintf("%s/fama-french-size.%s.annret.png", plotPath, srt), width=12, height=6)
	
	for(fName in c(first(fctr), last(fctr))){
		iName <- ffdesc[ffdesc$ABBR == fName,]$EXP[1]
		
		iDf1 <- data.frame(retDf[, c(fName, 'TIME_STAMP')])
		iDf1$px <- cumprod(1 + iDf1[,1])
		iDf1 <- iDf1[, c('px', 'TIME_STAMP')]
		names(iDf1) <- c('px', 'time_stamp')
		
		fanPlot <- common.CreateFanChart(iDf1, sprintf("%s: %s", fName, iName), sprintf("%d:%s", min(year(iDf1$time_stamp)), max(iDf1$time_stamp)))
		ggsave(sprintf("%s/factor-size.%s.%s.fan.png", plotPath, srt, fName), fanPlot, width=12, height=6)
	}
}

render("rp-fama-french-factors.Rmd", output_file="rp-fama-french-factors.html")
