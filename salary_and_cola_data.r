library(dplyr)

emp_orig <- read.csv("C:/Users/mj91d/Downloads/oesm19ma/oesm19ma/MSA_M2019_dl.csv")
names(emp_orig)[1] <- 'fips'

nums <- c("h_mean","a_mean" ,"mean_prse","h_pct10","h_pct25","h_median" ,
          "h_pct75", "h_pct90","a_pct10","a_pct25","a_median","a_pct75","a_pct90" ,
          "tot_emp", "emp_prse", "jobs_1000", "loc_quotient")
emp_orig[nums] <- sapply(emp_orig[nums], function(x) {gsub(",", "", x)})
emp_orig[nums] <- sapply(emp_orig[nums], as.numeric)

pp <- read.csv('pp.csv')
names(pp)[1] <- 'fips'
names(pp)[3] <- 'pp'
pp <- pp[,c('fips', 'pp')]

df <- merge(pp, emp_orig)
inc_var <- c('a_mean', 'a_pct10', 'a_pct25', 'a_median', 'a_pct75', 'a_pct90')
for (var in inc_var){
  df[,paste0(var,'_adj')] <- df[,var]/(df$pp/100)
}
names(df)

grep("Data", unique(df$occ_title), ignore.case = T, value = T)

ds <- df[df$occ_title == "Data Scientists and Mathematical Science Occupations, All Other" & !is.na(df$a_mean_adj),]
nrow(ds)
head(ds)
table <- ds[order(-ds$a_median_adj), c('area_title', 'pp', 'tot_emp', 'jobs_1000', 'a_median', 'a_median_adj', 'a_pct10', 'a_pct10_adj', 'a_pct90', 'a_pct90_adj')]
rownames(table) <- NULL
write.csv(table, 'jobs.csv')

grep("statis", unique(df$occ_title), ignore.case = T, value = T)

sub <- df[df$occ_title == "Statisticians" & !is.na(df$a_mean_adj),]
nrow(sub)
sub[order(-sub$a_median_adj),]


grep("nurse", unique(df$occ_title), ignore.case = T, value = T)

sub <- df[df$occ_title == "Registered Nurses" & !is.na(df$a_mean_adj),]
nrow(sub)
sub[order(-sub$a_median_adj),]


grep("lawyer", unique(df$occ_title), ignore.case = T, value = T)

sub <- df[df$occ_title == "Lawyers" & !is.na(df$a_mean_adj),]
nrow(sub)
sub[order(-sub$a_median_adj),]


grep("econ", unique(df$occ_title), ignore.case = T, value = T)

sub <- df[df$occ_title == "Economists" & !is.na(df$a_mean_adj),]
nrow(sub)
sub[order(-sub$a_median_adj),]
sum(is.na(df$a_mean))
sum(is.na(df$a_median))

grep("rail", unique(df$occ_title), ignore.case = T, value = T)

sub <- df[df$occ_title == "Railroad Conductors and Yardmasters" & !is.na(df$a_mean_adj),]
nrow(sub)
sub[order(-sub$a_median_adj),]
sum(is.na(df$a_mean))
sum(is.na(df$a_median))
