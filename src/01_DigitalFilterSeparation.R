## 01_DigitalFilterSeparation.R
#' This script will read in all data and use a variety of digital filter techniques for separation.
#' 
#' NOTE: right now missing dates are just ignored, for detailed analysis we should do some gap-filling

source(file.path("src", "paths+packages.R"))

## load baseflow separation functions from repository szipper/GlobalBaseflow
source(file.path("..", "GlobalBaseflow", "src", "BaseflowSeparationFunctions.R"))

## load raw data
df.raw <- 
  file.path("data", "raw", "AllSites.csv") %>% 
  read.csv(stringsAsFactors=F)

# convert string to date
df.raw$date <- mdy(df.raw$date)

# add empty columns to site metadata
df.site.metadata$k <- NaN
df.site.metadata$BFImax <- NaN

# list of sites
sites.all <- unique(df.raw$site)

# scroll through sites
start.flag <- T
for (s in sites.all){
  # site index
  i_s <- which(df.site.metadata$site==s)
  
  # subset site data
  df.site <- 
    df.raw %>% 
    subset(site==s)
  
  # estimate recession constant and BFImax
  k <- baseflow_RecessionConstant(df.site$discharge.m3_s, UB_prc=0.95, method="Brutsaert")
  df.site.metadata$k[i_s] <- k
  
  # perform baseflow separations
  df.site$HYSEP_fixed <- baseflow_HYSEP(Q = df.site$discharge.m3_s, 
                                        area_mi2 = df.site.metadata$area.km2[i_s]*km2.to.mi2, 
                                        method="fixed")
  df.site$HYSEP_slide <- baseflow_HYSEP(Q = df.site$discharge.m3_s, 
                                        area_mi2 = df.site.metadata$area.km2[i_s]*km2.to.mi2, 
                                        method="sliding")
  df.site$HYSEP_local <- baseflow_HYSEP(Q = df.site$discharge.m3_s, 
                                        area_mi2 = df.site.metadata$area.km2[i_s]*km2.to.mi2, 
                                        method="local")
  df.site$UKIH <- baseflow_UKIH(Q = df.site$discharge.m3_s, endrule="B")
  df.site$BFLOW_1pass <- baseflow_BFLOW(Q = df.site$discharge.m3_s, beta=0.925, passes=1)
  df.site$BFLOW_3pass <- baseflow_BFLOW(Q = df.site$discharge.m3_s, beta=0.925, passes=3)
  if (is.finite(k)){
    if (k>1) stop("Error: k > 1")
    BFImax <- baseflow_BFImax(df.site$discharge.m3_s, k=k)
    df.site.metadata$BFImax[i_s] <- BFImax
    df.site$Eckhardt <- baseflow_Eckhardt(Q = df.site$discharge.m3_s, BFImax=BFImax, k=k)
  } else {
    df.site$Eckhardt <- NaN
  }
  
  # combine with other sites
  if (start.flag){
    df.out <- df.site
    start.flag <- F
  } else {
    df.out <- rbind(df.out, df.site)
  }
  
  # status update
  print(paste0(s, " complete"))
}

## save output
write.csv(df.out, file.path("data", "AllSites_DigitalFilterSeparation.csv"),
          row.names=F, quote=F)

## summarize and join with site metadata
df.sites.all <-
  df.out %>% 
  melt(id=c("date", "discharge.m3_s", "site"),
       value.name="baseflow.m3_s", variable.name="method") %>% 
  group_by(site, method) %>% 
  summarize(discharge.total = sum(discharge.m3_s, na.rm=T),
            baseflow.total = sum(baseflow.m3_s, na.rm=T),
            BFI = baseflow.total/discharge.total) %>% 
  left_join(df.site.metadata, by=c("site"))

## plot
ggplot(df.sites.all, aes(x=BFI_CMB, y=BFI, shape=site)) +
  geom_abline(intercept=0, slope=1, color="grey65") +
  geom_point() +
  facet_wrap(~method) +
  scale_y_continuous(name="BFI, Digital Filters", limits=c(0,1), expand=c(0,0)) +
  scale_x_continuous(name="BFI, CMB", limits=c(0,1), expand=c(0,0)) +
  scale_shape_discrete(name="Site") +
  NULL +
  ggsave(file.path("results", "01_DigitalFilterSeparation_AnnualComparison.png"),
         width=8, height=8, units="in")
