time_srs<-function(JOR){
  
  population <- JOR$population
  date_pop_JO <<- as.character(as.Date(population[[5]][3]))
  time <- data.frame(population[[7]][3])
  time <- time[-1,]
  
  time$date <- as.Date(time$date)
  
  time$total_individuals <- as.integer(time$total_individuals)
  time$ind <- rep(time$total_individuals[1],nrow(time))
  for (i in 2:nrow(time)){
    time$ind[i] <-  time$total_individuals[i]- time$total_individuals[i-1]
  }
  
  time$month <- as.integer(format(time$date, "%m"))
  time$year <- as.integer(format(time$date, "%Y"))
  
  time$date <- NULL
  cur_date <-Sys.Date()
  cur_year <- as.integer(format(cur_date,"%Y"))
  cur_month <- as.integer(format(cur_date,"%m")) - 1
  delt_time <- cur_year - 2012
  


  if (cur_month != 0){
    months <- data.frame(month=c(rep(1:12,delt_time),1:cur_month%%12))
    years <- arrange(data.frame(year=c(rep(2012:(cur_year-1),12),rep(cur_year,cur_month%%12))), year)
  }else{
    months <- data.frame(month=rep(1:12,delt_time))
    years <- data.frame(year=rep(2012:(cur_year-1),12))
  }
  
  bench <-cbind(arrange(years, year),months)

  time <- group_by(time, year, month)
  time2 <- summarise(time, Net =sum(ind), Cumulitive = max(total_individuals))
  time2 <-merge(bench,time2, all = T)
  
  
  total <- ts(time2$Cumulitive, deltat=1/12, star = c(2012,1))
  recent <- ts(time2$Net, deltat=1/12, star = c(2012,1))
  df <- cbind(Cumulative = total, Recent = recent)
  df
}


demo_JOR <- function(df){
  population <- df$population
  date_demo_JO <<- as.character(as.Date(population[[5]][2]))
  demo_JO <- population[6][2,]
  females <- demo_JO[grepl("F", colnames(demo_JO))] 
  males <- demo_JO[grepl("M", colnames(demo_JO))]
  
  ranges <- c("0-4","5-11","12-17","18-59","60 and above")
  
  both <- data.frame()
  
  for(i in 1:ncol(females)){
    both[i,1] <- ranges[i]
    both[i,2] <-females[i]
    both[i,3] <-males[i]
  }
  
  colnames(both) <- c("Range", "Females", "Males")
  both$Range <- ordered(both$Range,
                        levels = c("0-4","5-11","12-17","18-59","60 and above"))
  
  both$Females <- as.integer(both$Females)
  both$Males <- as.integer(both$Males)
  
  both
}



who_JOR <- function(df){
  
  who <- df$whos_doing_what_where
  who[,"sector_name_ar"] <- NULL
  colnames(who) <- c("Sector","URL_Address","Organization_Acronym")
  for (i in 1:nrow(who)){
    who[i,2]<-paste0("<a href='",who[i,2],"' target='_blank'>"
                     ,who[i,2],"</a>")}
  who
}


doc_JOR <- function(df){
  
  doc <- df$documents
  doc<- doc[,-c(3,4,6,7,8)]
  
  colnames(doc) <- c("Title","URL_Link", "Publication_Date")
  
  doc[,3] <- as.Date(doc[,3])
  
  doc <- arrange(doc, desc(Publication_Date))
  for (i in 1:nrow(doc)){
    doc[i,2]<-paste0("<a href='",doc[i,2],"' target='_blank'>"
                     ,doc[i,2],"</a>")}
  
  doc
}

JOR_gov <- function(df){
  
  gov <- df$regions
  names <- strsplit(gov[[1]], " ")
  names <- unlist(names)
  names <- names[seq(1,23,2)]
  names <- tolower(names)
  
  url_reg<- rep(NA,12)
  
  link <- "http://data.unhcr.org/api/population/search.json?name=insert&level=region&instance_id=syria"
  
  for (i in 1:12){
    
    url_reg[i] <- gsub("insert", names[i],link)
  }
  
  
  for (i in 1:12){
    assign(names[i], fromJSON(url_reg[i]))
  }
  
  all <- c(ajlun,amman, aqaba,balqa,irbid,  
           jarash, karak,maan,madaba,mafraq, tafilah, zarqa)
  all
}


gov_demos <- function(df)
{
  demos <- data.frame(matrix(ncol = 10, nrow = 12))
  colnames(demos) <- colnames(df[[3]][,5])
  updates <- rep(NA,12)
  step = 0
  
  
  for(i in 1:12){
    demos[i,] <- as.integer(df[[I(3+step)]][,5])
    updates[i] <- df[[I(3+step)]][6]
    step <- step + 9
  }
  
  updates<<-as.character(as.Date(unlist(updates)))
  names <- c("ajlun","amman", "aqaba", "balqa", "irbid",  
             "jarash", "karak","maan","madaba","mafraq", "tafilah", "zarqa")
  demos$total <- rowSums(demos)
  demos <- cbind(demos,names)
  demos
}

select_demo_gov <- function(df,n){
  
  females_gov <- df[grepl("F", colnames(df))]
  males_gov <- df[grepl("M", colnames(df))]
  
  select_gov <- function(t){
    females <- females_gov[t,]
    males <- males_gov[t,]
    both_gov <- data.frame()
    ranges <- c("0-4","5-11","12-17","18-59","60 and above")
    
    for(i in 1:ncol(females_gov)){
      both_gov[i,1] <- ranges[i]
      both_gov[i,2] <-females[i]
      both_gov[i,3] <-males[i]
    }
    colnames(both_gov) <- colnames(both_gov) <- c("Range", "Females", "Males")
    both_gov$Range <- ordered(both_gov$Range,
                              levels = c("0-4","5-11","12-17","18-59","60 and above"))
    both_gov$Females <- as.integer(both_gov$Females)
    both_gov$Males <- as.integer(both_gov$Males)
    both_gov
  }
  gov_selected <-select_gov(n)
  gov_selected
}                   

gov_who <- function(df){
  
  save <- 0
  for(i in seq(9,108,9)){
    save <-  save+nrow(df[[i]])
  }
  
  whos <- matrix(ncol = 5, nrow = save)
  ind = 1
  for (i in seq(9,108,9)){
    lower <- ind
    upp <- ind + nrow(df[[i]])-1
    whos[lower:upp,] <- as.matrix(cbind(df[[i]],rep(i/9,nrow(df[[i]]))))
    ind <- upp+1
  }
  
  whos <- data.frame(whos)
  whos[,2]<- NULL
  colnames(whos) <- c("Sector","URL_Address","Organization_Acronym","Gov")
  whos
}

select_who_gov <- function(df,n){
  
  select_who <- function(t){
    who_gov <- data.frame()
    who_gov <- df[df$Gov==t,]
    who_gov$Gov <- NULL
    who_gov[,2] <- as.character(who_gov[,2])
    colnames(who_gov) <- c("Sector","URL_Address","Organization_Acronym")
    for (i in 1:nrow(who_gov)){
      who_gov[i,2]<-paste0("<a href='",who_gov[i,2],"' target='_blank'>"
                           ,who_gov[i,2],"</a>")}
    who_gov
  }
  who_selected <-select_who(n)
  who_selected
}

plot_map_jor <- function(df){
  jor<-get_admin1_map("jordan")
  jor$name<-gsub("ma`an","maan",jor$name)
  
  jor <- merge(jor,df,by.x="name",by.y="names")
  jor$name <- toupper(jor$name)
  jor
}


plot_bar <- function(df){
  
  object <-gvisBarChart(df, xvar="Range", yvar=c("Females", "Males"), 
               options = list(legend="bottom"))
  object
}

table <- function(df){
  renderDataTable((df), escape = FALSE, options = list(pageLength = 10))
}

plot_dygraph <- function(df){
  
  dygraph(df) %>% 
    dyOptions(stackedGraph = TRUE) %>% dyRangeSelector()
}

gov_name <- function(n){
  gov_vector <- c("Ajlun", "Amman", "Aqaba",
                  "Balqa", "Irbid", "Jarash",
                  "Karak", "Maan", "Madaba",
                  "Mafraq", "Tafilah",
                  "Zarqa")
  gov_vector[as.integer(n)]
}



demo_gov_time <- function(df,n){
  df[as.integer(n)]
}