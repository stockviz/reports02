library('RODBC')
library('rmarkdown')
library('tidyverse')
library('lubridate')

#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/Pandoc")

#source("C:/stockviz/r/config.r")
source("/mnt/hollandr/config.r")

options("scipen"=100)
options(stringsAsFactors = FALSE)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

cutoff <- Sys.Date() - 500

etfs <- sqlQuery(lconUs2, "select ETF, min(time_stamp) st, max(time_stamp) ed from ETF_CONSTITUENTS group by ETF")
etfNames <- sqlQuery(lcon, "select SYMBOL as ETF, FUND as NAME from ETF_META")

etfs <- etfs %>% filter(ed > cutoff & st < ed) %>% inner_join(etfNames) %>% select(ETF, NAME, ed, st) %>% arrange(ETF)

#etfs <- head(etfs, 2)

changeLog <- NULL

tryCatch({
	load("changeLog.RData")
}, error = function(e){ print(e) })

if(is.null(changeLog)){
	changeLog <- data.frame(ETF = "", NAME = "", CHANGE_DATE = Sys.Date(), REPORT_UPDATE_DATE = Sys.Date())
} 

changeLog <- etfs %>% full_join(changeLog) %>% 
							mutate(CHANGE_DATE = ed, REPORT_UPDATE_DATE = st) %>% 
							select(ETF, NAME, CHANGE_DATE, REPORT_UPDATE_DATE) %>% 
							as.data.frame()

#print(changeLog)
							
for(i in 1:nrow(changeLog)){
	#print(changeLog[i,])

	if(!is.na(changeLog$REPORT_UPDATE_DATE[i]) && is.Date(changeLog$REPORT_UPDATE_DATE[i]) && changeLog$REPORT_UPDATE_DATE[i] >= changeLog$CHANGE_DATE[i]) next
	
	changeLog$REPORT_UPDATE_DATE[i] <- Sys.Date()
	
	print(paste("processing", changeLog$ETF[i]))
	
	tryCatch({
		render("analysis/rp-changes.Rmd", 
					output_file=paste0("rp-", changeLog$ETF[i], ".html"), 
					params=list(etf = changeLog$ETF[i], name = changeLog$NAME[i]))
	}, error=function(e){print(e)})
}

save(changeLog, file="changeLog.RData")

