library('quantmod')
library('PerformanceAnalytics')

library('reshape2')
library('lubridate')
library('tidyverse')

library('ggrepel')
library('ggthemes')


common.CreateFanChart <- function(fanDf, title, subtitle){
	years <- fanDf %>% mutate(Y = year(time_stamp)) %>% group_by(Y) %>% summarize(ctr = n()) %>% filter(ctr > 210) %>% select(Y) %>% arrange(Y)
	years <- years$Y
	
	pXts <- xts(fanDf[,1], fanDf[,2])
	names(pXts) <- c('X')
	
	annualRets <- data.frame(Y=0, R=0.0)
	dailyByYrXts<-NULL
	for(i in 1:(length(years)-1)){
		yr<-years[i]
		yrSeries <- pXts[toString(yr)]
		drets <- dailyReturn(yrSeries)
		icol <- cumprod(1+drets)
		dcol<-as.Date(sprintf("2000-%s", strftime(index(icol), "%m-%d")))
		dailyByYrXts <- merge.xts(dailyByYrXts, xts(icol, dcol))
		annualRets <- rbind(annualRets, c(yr, as.numeric(Return.cumulative(drets))))
	}
	dailyByYrXts<-na.locf(dailyByYrXts)

	annualRets <- annualRets[-1,]
	annualRets$Y <- as.integer(annualRets$Y)
	annualRets$R <- as.numeric(annualRets$R)

	i<-length(years)
	yr<-years[i]
	
	yrSeries <- pXts[toString(yr)]
	drets <- dailyReturn(yrSeries)
	icol <- cumprod(1+drets)
	dcol<-as.Date(sprintf("2000-%s", strftime(index(yrSeries), "%m-%d")))
	lastCol <- xts(icol, dcol)
	dailyByYrXts <- merge.xts(dailyByYrXts, lastCol)
	dailyByYrXts[sprintf("%s/%s", first(index(lastCol)), last(index(lastCol))), ncol(dailyByYrXts)] <- na.locf(dailyByYrXts[sprintf("%s/%s", first(index(lastCol)), last(index(lastCol))), ncol(dailyByYrXts)])

	names(dailyByYrXts) <- sapply(years, function(X) toString(X))

	dailyByYear <- data.frame(dailyByYrXts)
	dailyByYear$T <- index(dailyByYrXts)

	toPlot <- melt(dailyByYear, id='T')
	toPlot$variable <- gsub("X", "", as.character(toPlot$variable))
	#toPlot$T <- factor(toPlot$T, levels=unique(toPlot$T))

	greys <- colorRampPalette(c("#FFFDD0", "black"))(length(years))
	toPlot$color <- greys[as.integer(toPlot$variable) - min(years) + 1]
	toPlot$sz <- (as.integer(toPlot$variable) - min(years) + 1)/(2*length(years))
	toPlot[toPlot$variable == toString(max(years)),]$sz <- 1.5

	minYr <- annualRets[annualRets$R == min(annualRets$R),1]
	maxYr <- annualRets[annualRets$R == max(annualRets$R),1]
	
	minYr2 <- annualRets[annualRets$R == min(annualRets[annualRets$R > min(annualRets$R),2]), 1]
	maxYr2 <- annualRets[annualRets$R == max(annualRets[annualRets$R < max(annualRets$R),2]), 1]

	toPlot[as.integer(toPlot$variable) == minYr,]$sz <- 1
	toPlot[as.integer(toPlot$variable) == maxYr,]$sz <- 1
	
	toPlot[as.integer(toPlot$variable) == minYr2,]$sz <- 0.75
	toPlot[as.integer(toPlot$variable) == maxYr2,]$sz <- 0.75

	toPlot[as.integer(toPlot$variable) == minYr,]$color <- "darkred"
	toPlot[as.integer(toPlot$variable) == maxYr,]$color <- "darkgreen"
	
	toPlot[as.integer(toPlot$variable) == minYr2,]$color <- "darkred"
	toPlot[as.integer(toPlot$variable) == maxYr2,]$color <- "darkgreen"

	toPlot$label <- NA
	toPlot[as.integer(toPlot$variable) == minYr,]$label[nrow(toPlot[as.integer(toPlot$variable) == minYr,])] <- minYr
	toPlot[as.integer(toPlot$variable) == maxYr,]$label[nrow(toPlot[as.integer(toPlot$variable) == maxYr,])] <- maxYr
	toPlot[as.integer(toPlot$variable) == minYr2,]$label[nrow(toPlot[as.integer(toPlot$variable) == minYr2,])] <- minYr2
	toPlot[as.integer(toPlot$variable) == maxYr2,]$label[nrow(toPlot[as.integer(toPlot$variable) == maxYr2,])] <- maxYr2
	toPlot[as.integer(toPlot$variable) == max(years) & !is.na(toPlot$value),]$label[nrow(toPlot[as.integer(toPlot$variable) == max(years) & !is.na(toPlot$value),])] <- max(years)

	dateBreaks <- index(dailyByYrXts)
	day(dateBreaks) <- days_in_month(dateBreaks)
	dateBreaks <- unique(dateBreaks)

	fanPlot <- ggplot(toPlot, aes(x=T, y=value, group=variable)) +
		theme_economist() +
		theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		geom_line(aes(color=color, size=sz)) +
		scale_color_identity() +
		scale_size_identity() +
		scale_x_date(breaks = dateBreaks, date_labels="%b-%d", expand=c(0, 0)) +
		geom_label_repel(aes(label = label), alpha=0.5) +
		labs(x='', y='growth of Rs. 1', title=title, subtitle=subtitle) +
		annotate("text", x=toPlot$T[1], y=min(toPlot$value, na.rm=T), label = "@StockViz", hjust=0, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	return(fanPlot)

}