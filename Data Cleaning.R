setwd("~/Desktop/FINAL_REVIEW/")

find_boundary <- function(v){
  # find upper bound and lower bound for the outlier
  M <- quantile(v, 0.75, na.rm=TRUE) + (IQR(v, na.rm=TRUE) * 1.5 )
  m <- quantile(v, 0.25, na.rm=TRUE) - (IQR(v, na.rm=TRUE) * 1.5 )
  return(c(m, M))
}

# remove outlier for a single package, single type(pre or post)
outlier_removal <- function(df, col_index){
  row.names(df) = 1:nrow(df)
  v = unique(df[,col_index])
  if (length(v)<=5){
    return(df) # no outlier removal if too less data
  }
  
  bdry = find_boundary(v)
  outlier = c()
  for (r in 1:nrow(df)){
    if (is.na(df[r,col_index]) | df[r,col_index] <= bdry[1] | df[r,col_index] >= bdry[2]){
      outlier = c(outlier, r)
    }
  }
  print("outlier idx")
  print(outlier)
  if (length(outlier) > 0){
    df = df[-outlier,]
  }
  return(df)
}

clean_dataframe <- function(df, col_index){
  packages = unique(df$PACKAGE_ID)
  clean_df = c()
  for (p in packages){
    # get the package specific data
    P = subset(df, PACKAGE_ID==p)
    P[,col_index] = as.numeric(paste(P[,col_index]))
    z = length(which(P[,col_index] == 0))
    if (!z > nrow(P)*2/3){
      for (tp in c("Pre", "Post")){
        print("--- package ID ---")
        print(p)
        dd = subset(P, IDENTIFIER==tp)
        print("original row #")
        print(nrow(dd))
        clean = outlier_removal(dd, col_index)
        print("cleaned row #")
        print(nrow(clean))
        clean_df = rbind(clean_df, clean)
      }
    }
  }
  return(clean_df)
}

## clean df
d = read.csv("result.csv")
t = clean_dataframe(d, 16)

write.csv(t,"clean_data.csv", row.names=FALSE)

t_1 <- read.csv("clean_data.csv")

psp = c(196004, 196228, 196243, 192109, 195576, 184558, 190802, 190801)
P = subset(t_1, PACKAGE_ID %in% psp)
write.csv(P,"prospecting.csv", row.names=FALSE)

rtr = c(195808, 195806, 196153, 196847, 196844, 195653,  195650, 195651, 195652, 196587, 195543, 195675, 196246, 196416, 196213, 195997)
R = subset(t_1, PACKAGE_ID %in% rtr)
write.csv(R,"retargeting.csv", row.names=FALSE)

mdl = c(190802, 184558, 195675, 195650, 195543, 196847, 196844, 196153, 196228, 195997, 196213)
M = subset(t_1, PACKAGE_ID %in% mdl)
write.csv(M,"with_model.csv", row.names=FALSE)

nmdl = c(190801, 196587, 196004, 195576, 196243, 196246, 195651, 195806, 195808, 195653, 195652, 196416, 192109)
NM = subset(t_1, PACKAGE_ID %in% nmdl)
write.csv(NM,"wo_model.csv", row.names=FALSE)



## clean df 5days
d_1 = read.csv("result_final_review.csv")
d_1 <- subset(d_1, d_1$IDENTIFIER!= "Other")
t1 = clean_dataframe(d_1, 16)

write.csv(t1,"clean_data_5days.csv", row.names=FALSE)
unique(t1$PACKAGE_ID)

t_2 <- read.csv("clean_data_5days.csv")

psp = c(195564, 196850, 196851, 197442, 197443, 197444, 199040, 199051,199052,199150,199410,199445,199460)
P = subset(t_2, PACKAGE_ID %in% psp)
write.csv(P,"prospecting_5days.csv", row.names=FALSE)

rtr = c(197187, 198247, 199338)
R = subset(t_2, PACKAGE_ID %in% rtr)
write.csv(R,"retargeting_5days.csv", row.names=FALSE)

mdl = c(195564, 196850, 196851, 197442, 197443, 197444, 198247, 199051, 199052, 199150, 199338, 199410,199445,199460)
M = subset(t_2, PACKAGE_ID %in% mdl)
write.csv(M,"with_model_5days.csv", row.names=FALSE)

nmdl = c(197187, 199040)
NM = subset(t_2, PACKAGE_ID %in% nmdl)
write.csv(NM,"wo_model_5days.csv", row.names=FALSE)

