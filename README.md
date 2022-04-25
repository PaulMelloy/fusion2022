# fusion2022
Election data analysis to assist Fusion party campaign

The following is an example of analyses  

## Load libraries  
Loading libraries  

```r 
library(eechidna)
library(dplyr)
```

## Inspect 2019 two party prefered  

```r
tpp19 %>%
  filter(StateAb == "QLD") %>%
  head()
```

## Inspect 2019 first preferences  

```r
fp19 %>%
  filter(StateAb == "QLD",
         DivisionNm == "RYAN") %>%
  arrange(-Percent)
```


## Polling booth preferences  

Peek at the data  

```r
fppb <- firstpref_pollingbooth_download()
head(fppb)
```

Which polling booth in the Ryan division had the highest voter turnout in 2016

```r
fppb %>%
  filter(DivisionNm == "RYAN",
         year == 2016) %>%
  group_by(PollingPlace) %>%
  summarise(voter_turnout = sum(OrdinaryVotes)) %>%
  arrange(-voter_turnout)
```
This R package does not have the 2019 election data so we will download it for QLD from the [AEC website](https://results.aec.gov.au/24310/Website/HouseDownloadsMenu-24310-Csv.htm.)  

```r
fp19_qld <- read.csv("https://results.aec.gov.au/24310/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-24310-QLD.csv",skip = 1)

head(fp19_qld)
```

```r
fp19_qld %>%
  filter(DivisionNm == "Ryan") %>%
  group_by(PollingPlace) %>%
  summarise(voter_turnout = sum(OrdinaryVotes)) %>%
  arrange(-voter_turnout)
```


Add a progressiveness score to each party

```r
fp19_qld <-
  fp19_qld %>%
  mutate(progresive =
           case_when(
             PartyAb == "FACN" ~ 0,
             PartyAb == "ALP" ~ 65,
             PartyAb == "ON" ~ 20,
             PartyAb == "LNP" ~ 45,
             PartyAb == "UAPP" ~ 25,
             PartyAb == "DLP" ~ 50,
             PartyAb == "GRN" ~ 80,
             PartyAb == "SAL" ~ 85,
             PartyAb == "KAP" ~ 50,
             PartyAb == "AJP" ~ 75,
             PartyAb == "LDP" ~ 23,
             PartyAb == "SPP" ~ 70,
             PartyAb == "LAOL" ~ 15,
             PartyAb == "AUP" ~ 77,
             PartyAb == "AFN" ~ 35,
             PartyAb == "SEP" ~ 70,
             TRUE ~ NA_real_))
```

Which booths in the division of *Ryan* had the higher average progressive vote

```{r}
fp19_qld %>%
  filter(DivisionNm == "Ryan") %>%
  group_by(PollingPlace) %>%
  summarise(progresiveness = mean(progresive * OrdinaryVotes, na.rm = TRUE),
            votes = sum(OrdinaryVotes, na.rm = TRUE)) %>%
  mutate(progresivenessP = progresiveness / votes) %>%
  arrange(-progresivenessP)
```

