# Demo create bubble chart
#
# To install baseball demo dataset in Aster
# download baseball.zip from
# https://bitbucket.org/grigory/toaster/downloads/baseball.zip
# and run
# sh load_baseball_data.sh -d mydbname -U beehive 

library(toaster)
library(ggplot2)

# update ODBC data source name
dsn = "PresalesPartnersDB"
uid = "beehive"
pwd = "beehive"
close(conn)
conn = odbcConnect(dsn, uid, pwd)

# Bubble chart example
bubble = computeHeatmap(conn, 'teams_enh', 'franchid', 'decadeid',
                        aggregates=c("SUM(BA*AB)/SUM(AB)ba", 
                                       "SUM(IPOuts*ERA)/SUM(IPOuts) era",
                                       "ROUND(AVG(8-rank)) rank",
                                       "MIN(lgid) lgid"),
                        where="yearid between 1970 and 2009")


createBubblechart(bubble, "ba", "era", "rank", label="franchid", fill="franchid",
                  facet=c("decadeid","lgid"), #ncol=1, 
                  scaleSize = FALSE, shapeSizeRange=c(1,15), shapeMaxSize = 15,
                  title="Team Ranks by BA and ERA", 
                  labelSize = 5, labelColour = "black", labelVJust = 1,
                  legendPosition="none", themeExtra = guides(fill = "legend", size = "legend"))

createBubblechart(bubble, "ba", "era", "rank", label="franchid", fill="franchid",
                  facet=c("lgid", "decadeid"), ncol=1, shapeSizeRange=c(1,20),
                  title="Team Ranks by BA and ERA", 
                  labelSize = 5, labelColour = "black", labelVJust = 1,
                  legendPosition="none", themeExtra = guides(fill = "legend", size = "legend"))