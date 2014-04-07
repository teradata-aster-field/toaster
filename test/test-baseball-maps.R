require(toaster)
require(memoise)

dsn = "PresalesPartnersDB"
uid = "beehive"
pwd = "beehive"
close(asterConn)
asterConn = odbcConnect(dsn, uid, pwd)

data = sqlQuery(asterConn, 
           "select name || ', ' || park teamname, lgid, teamid, decadeid, min(name) name, min(park) park, avg(rank) rank, avg(g) g, avg(w) w, avg(l) l, avg(r) r, avg(ab) ab,
            avg(h) h, avg(x2b) x2b, avg(x3b) x3b, avg(hr) hr, avg(bb) bb, avg(so) so, avg(sb) sb, avg(cs) cs, avg(hbp) hbp, avg(sf) sf,
            avg(ra) ra, avg(er) er, avg(era) era, avg(cg) cg, avg(sho) sho, avg(sv) sv, avg(ipouts) ipouts, avg(ha) ha, avg(hra) hra, 
            avg(bba) bba, avg(soa) soa, avg(e) e, avg(dp) dp, avg(fp) fp, avg(attendance) attendance, avg(bpf) bpf, avg(ppf) ppf, 
            avg(ba) ba, avg(slg) slg, avg(whip) whip, avg(ktobb) ktobb
             from teams_enh 
            group  by 1, 2, 3, 4", 
                stringsAsFactors=FALSE)

data = computeAggregates(asterConn, "pitching",
               columns = c("name || ', ' || park teamname", "lgid", "teamid", "decadeid"),
               aggregates = c("min(name) name", "min(park) park", "avg(rank) rank", "avg(attendance) attendance")
               )

geocodeMem = memoise(geocode)

createMap(data=data[data$decadeid>=2000,], source = "stamen", maptype = "watercolor", zoom=4, 
              facet=c("lgid", "decadeid"),
              locationName='teamname', metricName='attendance', labelName='name',
              shapeColour="blue", scaleRange = c(2,12), textColour="black",
              title='Team Attendance by Decade and Leage',
              geocodeFun=geocodeMem)