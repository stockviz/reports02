library('rmarkdown')
#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/Pandoc")

#render("rp-fama-french-industry.Rmd", output_file="rp-fama-french-industry.html")
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
library('tidyquant')

library('lubridate')
library('ggthemes')
library('reshape2')
library('viridis')
library('DT')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)

lconUs2 <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

################# industry charts

benchmarks <- c('HI_30', 'MED_40', 'LO_30')
startDt <- as.Date("2010-01-01")
maxDt <- sqlQuery(lconUs2, "select max(time_stamp) from FAMA_FRENCH_INDUSTRY_49_DAILY") [[1]]

bXts <- NULL
for(betf in benchmarks){
	bDf <- sqlQuery(lconUs2, sprintf("select ret, time_stamp from FAMA_FRENCH_SIZE_DAILY where KEY_ID='%s' and RET_TYPE='AVWRD' and time_stamp >= '%s'", betf, startDt))
	bXts <- merge.xts(bXts, xts(bDf[,1], bDf[,2]))
}

names(bXts) <- benchmarks

bXts <- bXts/100

bRetDaily <- bXts
bRetMonthly <- apply.monthly(bXts, Return.cumulative)
bRetAnn <- apply.yearly(bXts, Return.cumulative)

names(bRetDaily) <- benchmarks
names(bRetMonthly) <- benchmarks
names(bRetAnn) <- benchmarks

industryDf <- sqlQuery(lconUs2, sprintf("select time_stamp, key_id, ret from FAMA_FRENCH_INDUSTRY_49_DAILY where time_stamp >= '%s' and ret_type = 'AVWRD' order by time_stamp", startDt))
idefs <- sqlQuery(lconUs2, "select * from FAMA_FRENCH_DEFS")

iCount <- industryDf %>% group_by(key_id) %>% summarize(N = n(), MD = max(time_stamp))

annRetsAll <- industryDf %>% mutate(Y = year(time_stamp), ret=ret/100) %>% group_by(key_id, Y) %>% tq_transmute(mutate_fun = Return.cumulative, select=ret, col_rename = 'RET') %>% as.data.frame()
names(annRetsAll) <- c('IND', 'Y', 'RET')

toPlot <- annRetsAll
toPlot$RET <- 100*toPlot$RET
toPlot$Y <- factor(toPlot$Y, levels=unique(toPlot$Y))
toPlot$IND <- factor(toPlot$IND, levels=unique(toPlot$IND))

toPlot$color <- NA
reds <- colorRampPalette(c("darkred", "#FFFDD0"))(nrow(toPlot[!is.na(toPlot$RET) & toPlot$RET < 0,]))
greens <- colorRampPalette(c("#FFFDD0", "darkgreen"))(nrow(toPlot[!is.na(toPlot$RET) & toPlot$RET >= 0,]))
toPlot[order(toPlot$RET),]$color <- c(reds, greens, rep(NA, nrow(toPlot) - length(reds) - length(greens)))

ggplot(toPlot, aes(y=Y, x=IND, fill=color)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_tile() +
	geom_text(aes(label= ifelse(is.finite(RET), sprintf("%+.2f%%", RET), "")), hjust = "center", size=2.2) +
	scale_fill_identity() +
	guides(fill="none") +
	labs(x='', y='', title="Fama-French Industry Returns", subtitle=sprintf("Annual Returns [%s:%s]", min(industryDf$time_stamp), max(industryDf$time_stamp))) +
	annotate("text", x=nrow(idefs), y=1, label = "@StockViz", hjust='right', vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)

ggsave(sprintf("%s/fama-french-industry.all.png", plotPath), width=20, height=6)	

#render("rp-fama-french-industry.Rmd", output_file="rp-fama-french-industry.html")
#q()


lm_eqn <- function(frml, df){
	yStr <- as.character(frml)[2]
	xStr <- as.character(frml)[3]
    m <- lm(frml, df)
    eq <- substitute(italic(yStr) == a + b %.% italic(xStr)*","~~italic(r)^2~"="~r2, 
         list(a = format(unname(coef(m)[1]), digits = 2),
              b = format(unname(coef(m)[2]), digits = 2),
             r2 = format(summary(m)$r.squared, digits = 3),
			 yStr = yStr,
			 xStr = xStr))
    as.character(as.expression(eq));
}

for(i in 1:nrow(iCount)){
	if(iCount$N[i] != max(iCount$N) || iCount$MD[i] != max(iCount$MD)) next

	fName <- iCount$key_id[i]
	iName <- idefs[idefs$ID == iCount$key_id[i],]$DEF[1]
	
	iDf1 <- data.frame(px=cumprod(1+industryDf[industryDf$key_id == iCount$key_id[i],]$ret/100), time_stamp=industryDf[industryDf$key_id == iCount$key_id[i],]$time_stamp)
	fanPlot <- common.CreateFanChart(iDf1, sprintf("%s: %s", fName, iName), sprintf("%d:%s", min(year(iDf1$time_stamp)), max(iDf1$time_stamp)))
	ggsave(sprintf("%s/%s.fan.png", plotPath, fName), fanPlot, width=12, height=6)

	iXts <- xts(iDf1[,1], iDf1[,2])

	retDaily <- dailyReturn(iXts)
	retMontly <- monthlyReturn(iXts)
	retAnn <- annualReturn(iXts)
			
	names(retDaily) <- c(fName)
	names(retMontly) <- c(fName)
	names(retAnn) <- c(fName)
	
	retDailyDf <- data.frame(retDaily)
	retDailyDf$T <- index(retDaily)
	
	benchDailyDf <- data.frame(bRetDaily)
	benchDailyDf$T <- index(bRetDaily)
	mergedDaily <- retDailyDf %>% inner_join(benchDailyDf, by='T') %>% filter(T >= startDt) %>% relocate(T)
	mergedDailyXts <- na.trim(xts(mergedDaily[, -1], mergedDaily[,1]), sides='left')
	
	Common.PlotCumReturns(mergedDailyXts, sprintf("%s: %s", fName, iName), "Fama-French Industry Returns", sprintf("%s/%s.cumret.png", plotPath, fName))
	
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
		labs(x = "Year", y="Returns (%)", fill="", color="", size="", title=sprintf("%s: %s", fName, iName), subtitle=sprintf("Annual Returns [%s:%s]", first(index(retDaily)), last(index(retDaily)))) +
		annotate("text", x=maxYear, y=minRet, label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(sprintf("%s/%s.annret.png", plotPath, fName), width=12, height=6)
}

render("rp-fama-french-industry.Rmd", output_file="rp-fama-french-industry.html")
