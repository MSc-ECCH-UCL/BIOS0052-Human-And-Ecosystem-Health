# package dependencies
# use the "install.packages()" command if not already installed
library(terra); library(dplyr); library(magrittr); library(ggplot2); library(sf)
library(rstudioapi); library(tidyr); library(stringr); library(mgcv); library(gratia)
# additional packages you may need to install
library(caret); library(ranger); library(pROC)
# automatically set working directory
# (or if this doesn't work,
# manually set your working directory to the folder "Week3-Measuring-Env-Effects")
PATH = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(PATH)


# dengue surveillance data
# format dates
dd = read.csv("./data/dengue/dengue_central_ob.csv") %>%
  dplyr::mutate(date = as.Date(date))
# an accompanying shapefile of vietnam districts
shp = sf::st_read("./data/shapefiles/central.shp")
# shapefile of Vietnam border (for mapping)
shp_vnm = sf::st_read("./data/shapefiles/gadm36_VNM_0.shp")



# calculate incidence per 100,000 persons
dd$incidence = dd$cases / (dd$population_census / 100000)
# visualise a histogram of incidence
# what do you notice about this distribution?
ggplot() +
  geom_histogram(data=dd, aes(x=incidence), binwidth=10, fill="skyblue4", color="black") +
  theme_classic() + xlab("Dengue incidence") + ylab("Count")
# visualise time series of incidence
# each coloured line is a district
# broken into separate panels for each province
ggplot() +
  geom_line(data=dd, aes(date, incidence, group=areaid, color=factor(areaid))) +
  theme_classic() +
  facet_wrap(~province, nrow=1) +
  theme(legend.position="none",
        strip.background = element_blank(),
        strip.text = element_text(size=12)) +
  xlab("Month") + ylab("Dengue incidence per 100,000")


# calculate incidence per 100,000 persons
dd$incidence = dd$cases / (dd$population_census / 100000)
# visualise a histogram of incidence
# what do you notice about this distribution?
ggplot() +
  geom_histogram(data=dd, aes(x=incidence), binwidth=10, fill="skyblue4", color="black") +
  theme_classic() + xlab("Dengue incidence") + ylab("Count")


# broken into separate panels for each province
ggplot() +
  geom_line(data=dd, aes(date, incidence, group=areaid, color=factor(areaid))) +
  theme_classic() +
  facet_wrap(~province, nrow=1) +
  theme(legend.position="none",
        strip.background = element_blank(),
        strip.text = element_text(size=12)) +
  xlab("Month") + ylab("Incidence")

ggplot() + 
  geom_point(data=dd, aes(month_dengue, incidence), alpha=0.6) + theme_bw()


# plot line graph of incidence
# add points for "outbreak" months
ggplot() +
  geom_point(data=dd_test, aes(date, cases), color="skyblue4") +
  geom_point(data=dd_test[ dd_test$outbreak_95 ==1, ], aes(date, cases), color="red", size=1) +
  theme_classic() +
  facet_wrap(~district, scales="free_y", ncol=3) +
  theme(strip.background = element_blank())


# calculate means across entire time series
dd_mean = dd %>%
  dplyr::group_by(areaid) %>%
  dplyr::summarise(incidence = mean(incidence))
# combine with shapefile
shp_dd = shp %>%
  dplyr::left_join(dd_mean)
# plot with vietnam country as background
ggplot() +
  geom_sf(data=shp_vnm, color=NA, fill="grey80") +
  geom_sf(data=shp_dd, aes(fill=incidence), color="grey20") +
  theme_void() +
  scale_fill_viridis_c(option="inferno", direction=-1, name="Dengue\nincidence") +
  ggtitle("Mean monthly dengue incidence, 2005-2020") +
  theme(plot.title = element_text(size=11, hjust=0.5))



# how many outbreaks? 591 (i.e. 15% o observations)
table(dd$outbreak_95)

# plot number of outbreaks by month
mm = dd %>%
  dplyr::group_by(month_dengue) %>%
  dplyr::summarise(n = sum(outbreak_95))
ggplot() + 
  geom_bar(data=mm, aes(month_dengue, n), stat="identity", fill="skyblue4") + 
  theme_classic() + 
  xlab("Month") + ylab("Num. outbreaks (2005-2020)") +
  scale_x_continuous(breaks=1:12, labels=1:12)

# plot number of outbreaks by year
yy = dd %>%
  dplyr::group_by(year_dengue) %>%
  dplyr::summarise(n = sum(outbreak_95))
ggplot() + 
  geom_bar(data=yy, aes(year_dengue, n), stat="identity", fill="skyblue4") + 
  theme_classic() + 
  xlab("Year") + ylab("Num. outbreaks") 


# set categorical variables as factors
# to ensure model recognises them correctly
dd_m = dd %>%
  dplyr::mutate(areaid = as.factor(areaid),
                year_dengue = as.factor(year_dengue),
                month_dengue = as.factor(month_dengue))

# fit baseline model
# includes random intercept for areaid
# categorical effects of year and month
m_base = mgcv::gam(outbreak_95 ~ s(areaid, bs="re") + year_dengue + month_dengue,
                   data=dd_m,
                   family="binomial",
                   method="REML")

summary(m_base)


# create column defining set for each observation
dd$set = ifelse(
  dd$year_dengue %in% 2017:2020 & dd$month_dengue %in% 4:9,
  "test", "train"
)

# split out into train and test datasets
# training on 3,502 obervations
dd_train = dd %>%
  dplyr::filter(set == "train") %>%
  dplyr::mutate(areaid = as.factor(areaid),
                year_dengue = as.factor(year_dengue),
                month_dengue = as.factor(month_dengue))

# testing on 504 observations (around 13% of data)
dd_test = dd %>%
  dplyr::filter(set == "test") %>%
  dplyr::mutate(areaid = as.factor(areaid),
                year_dengue = as.factor(year_dengue),
                month_dengue = as.factor(month_dengue))



m0 = mgcv::gam(outbreak_95 ~ s(areaid, bs="re") + year_dengue + month_dengue,
                   data=dd_train,
                   family="binomial",
                   method="REML")

# predict for each observation in the test dataset
predicted = mgcv::predict.gam(m0, dd_test, type="response")

# add as a column into the test data
dd_test$predicted = predicted

# plot a boxplot of the predicted probability
# for outbreak and non-outbreak observations
# what do you notice?
dd_test %>%
  ggplot() +
  geom_boxplot(aes(factor(outbreak_95), predicted, fill=factor(outbreak_95))) +
  theme_bw() +
  xlab("Was outbreak observed?") + ylab("Predicted outbreak probability") +
  scale_fill_viridis_d(begin=0.3, end=0.75) +
  theme(legend.position="none")

# generate a ROC curve using observed and predicted values
# how good does the shape of this curve look?
roc_m0 = pROC::roc(dd_test$outbreak_95, dd_test$predicted)
plot(roc_m0)

auc_baseline = roc_m0$auc


# use the roc to extract the best cut-off threshold
# suggested threshold for defining an outbreak = 0.45
# this provides a sensitivity of 0.48 and a specificity of 0.92
# what does this tell you about the model?
cutoff = coords(roc_m0, "best", best.method="youden", transpose = FALSE)

# apply this threshold to predictions
# to produce a binary predicted outcome
dd_test$predicted_binary = as.numeric(dd_test$predicted > cutoff$threshold)

# tabulate observed and predicted
table(dd_test$outbreak_95, dd_test$predicted_binary)



pred_plot_bl = ggplot() +
  geom_tile(data=dd_test, aes(date, areaid, fill=factor(outbreak_95))) +
  scale_fill_viridis_d(option="magma", direction=-1, begin=0.5, end=0.9, name="Observed\noutbreak") +
  theme_bw() +
  geom_point(data=dd_test[dd_test$predicted_binary==1,], aes(date, areaid), pch=4, size=2) +
  xlab("Date") +
  ylab("District") +
  ggtitle("Baseline (benchmark) model") +
  theme(plot.title = element_text(size=12, hjust=0.5))
pred_plot_bl
