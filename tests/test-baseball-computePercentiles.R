require(toaster)

dsn = "PresalesPartnersDB"
uid = "beehive"
pwd = "beehive"
close(conn)
conn = odbcConnect(dsn, uid, pwd)

# single set of percentiles
allBAPerc = computePercentiles(conn, "batting_enh", "ba")
createBoxplot(allBAPerc, fill=NULL, title="BA Boxplot")

teamBA2000sPerc = computePercentiles(conn, "batting_enh", "ba",
                                     by="teamid", where="yearid >= 2000")
createBoxplot(teamBA2000sPerc, x="teamid", fill=NULL, useIQR=TRUE,
              title="Team BA in 2000s", xlab="Team", ylab=NULL,
              coordFlip=TRUE, legendPosition="none")

teamsBAbyDecadePerc = computePercentiles(conn, "batting_enh", "ba",
                                         by=c("lgid","decadeid"), 
                                         where="yearid>=1960 and lgid in ('AL','NL')")
createBoxplot(teamsBAbyDecadePerc, facet=c("lgid","decadeid"), fill="lgid",
              paletteValues = c("red","blue"), legendPosition="none",
              title="League BA by Decade")
