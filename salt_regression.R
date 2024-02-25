rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(dplyr, tidyr, ggplot2, readxl, countrycode)   

#install.packages("devtools")
#devtools::install_github("cardiomoon/ggiraphExtra")

##
salt<-read.csv("GBD_sodium.csv", stringsAsFactors = F)%>%
  rename(iso3 = AreaID) #this is technically SODIUM

gdp_pc<-read.csv("API_NY.GDP.PCAP.CD_DS2_en_csv_v2_184.csv")%>%
  select(-Indicator.Name, -Indicator.Code)%>%
  gather(year, gdp, -Country.Name, -Country.Code)%>%
  mutate(year = as.numeric(gsub("X", "", year)))%>%
  rename(iso3 = Country.Code)

region<-read.csv("Country_groupings_HLI.csv", stringsAsFactors = F)%>%
  select(iso3, Super_region)

#take 2019 GDP except for countries with no data that year

df<-read_excel("salt_data_modelled.xlsx")%>%
  mutate(iso3 = countrycode::countrycode(location, "country.name", "iso3c"))%>%
  left_join(., salt%>%select(iso3, salt=DataValue))%>%
  left_join(., gdp_pc%>%filter(year==2019)%>%select(iso3, gdp))%>%
  left_join(., region)

###

ggplot(df, aes(x=gdp, y=discretionary, color=Super_region))+
  geom_point(size =2)

ggplot(df, aes(x=gdp, y=discretionary))+
  geom_point(size =2)+
  geom_smooth(method="lm", se=FALSE)

# adjust for super region?
lm_fit <- lm(discretionary ~ gdp + Super_region, data=df)
summary(lm_fit)
#not significant

#total salt a predictor?
lm_fit <- lm(discretionary ~ salt, data=df)
summary(lm_fit) #not significant

#simple
lm_fit <- lm(discretionary ~ gdp, data=df)
summary(lm_fit)

#regress on GDP, logrithmic?
fit_log=glm(discretionary~gdp,data=df,family=binomial)
summary(fit_log) #worse fit

ggplot(df, aes(x=gdp, y=discretionary))+
  geom_point(size =2)+
  stat_smooth(method="glm", se=FALSE, 
              method.args = list(family=binomial))


##Simple regression of discretionary salt on GDP
predicted_df <- data.frame(disc_pred = predict(lm_fit, df), iso3 = df$iso3, gdp=df$gdp)

ggplot(predicted_df, aes(x=gdp, y=disc_pred))+
  geom_point(size =2)


### is GDP a predictor for processed salt intake?
# not enough data
ggplot(df, aes(x=gdp, y=processed))+
  geom_point(size =2)+
  geom_smooth(method = "lm", se=FALSE)

#does discretionary predict processed?
ggplot(df, aes(x=discretionary, y=processed))+
  geom_point(size =2)+
  geom_smooth(method = "lm", se = FALSE)

## take acatual literature value if not NA
out_df<-left_join(df, predicted_df)%>%
  rename(discretionary_predicted = disc_pred)%>%
  mutate(discretionary_predicted = ifelse(is.na(discretionary), discretionary_predicted, discretionary))

lm_fit2<-lm(processed ~ discretionary_predicted, data=out_df)
summary(lm_fit2) #it is significant

#but is outside the home a better fit?
lm_fit3<-lm(`outside the home` ~ discretionary_predicted, data=out_df)
summary(lm_fit3) #not significant

predicted_df2<-data.frame(processed_predicted = predict(lm_fit2, out_df), iso3 = df$iso3)

out_df<-left_join(out_df, predicted_df2)%>%
  mutate(processed_predicted = ifelse(processed_predicted<0, 0.02 , processed_predicted),
         outside_the_home_predicted = 1-processed_predicted-discretionary_predicted)%>%
  mutate(source = "literature")


ggplot(out_df, aes(x=gdp, y=processed_predicted))+
  geom_point()

## complete set, based on GDP ##
cdf<-gdp_pc%>%
  filter(year==2019)%>%
  mutate(discretionary_predicted = lm_fit$coefficients[1] + lm_fit$coefficients[2]*gdp)%>%
  mutate(discretionary_predicted = ifelse(discretionary_predicted<0.1,0.1,discretionary_predicted))%>%
  mutate(processed_predicted = lm_fit2$coefficients[1] + lm_fit2$coefficients[2]*discretionary_predicted,
         processed_predicted = ifelse(processed_predicted<0.02,0.02,processed_predicted),
         outside_the_home_predicted = 1-discretionary_predicted-processed_predicted)%>%
  mutate(source = "fitted")


out_df2<-bind_rows(out_df, cdf)

ggplot(out_df2, aes(x=gdp, y=discretionary_predicted, color=source))+
  geom_point(size=2)

'%!in%' <- function(x,y)!('%in%'(x,y)) # Function "Not In"

final_df<-bind_rows(out_df%>%select(iso3, location, discretionary_predicted, processed_predicted, outside_the_home_predicted, source),
                    cdf%>%select(iso3, location=Country.Name, discretionary_predicted, processed_predicted, outside_the_home_predicted, source)%>%
                      filter(iso3 %!in% unique(out_df$iso3))
                    )%>%
  filter(iso3 %in% unique(region$iso3))%>%
  left_join(., region)%>%
  left_join(., salt%>%select(iso3, sodium = DataValue))%>%
  mutate(discretionary = sodium*discretionary_predicted,
         processed = sodium * processed_predicted,
         outside = sodium * outside_the_home_predicted)

#Missing: Eritrea (Eritrea), North KOrea (PRK), South Sudan (SSD), Venezuela, (VEN)
# Most recent GDP year for these: 2011, NONE, 2015, 2014

### Projected increase in processed foods
#Cap at average high-income diet or max?
mean(final_df%>%filter(Super_region == "High-income")%>%pull(processed))
max(final_df%>%filter(Super_region == "High-income")%>%pull(processed))

unique(region$Super_region)

67.3/14
57.60/14
10/14
3/14

#Vandevijvere et al. Global trends in ultraprocessed food and drink product sales and their association with adult body mass index trajectories (2019) 
unique(region$Super_region)

inc<-data.frame(Super_region = unique(region$Super_region),
                annual_inc = c(0.048, 0.0071, 0.041, 0.048, -0.0021, 0, 0))

final_df<-left_join(final_df, inc)%>%merge(data.frame(year=2019:2040))%>%
  mutate(inc = ifelse(year>2019, annual_inc*processed, 0))%>%
  group_by(iso3)%>%
  arrange(year)%>%
  mutate(inc = cumsum(inc),
         processed = processed+inc,
         processed = ifelse(processed>4.3, 4.3, processed))


ggplot(final_df%>%filter(location=="Thailand"),
       aes(x=year, y=processed))+
  geom_point()+
  ylim(0,5)

write.csv(final_df%>%select(iso3, location, year, inc, discretionary, processed, outside), "salt_df.csv", row.names = F)

#Make shiny


