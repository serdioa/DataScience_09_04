# Load and cleanup prices of DAX and it's components.

library(quantmod)
library(tidyverse)

prepareTs <- function(TS) {
    as_tibble(TS) %>%
        mutate(ts = time(TS)) %>%
        rename(open = 1,
               close = 4,
               low = 3,
               high = 2,
               adjusted = 6,
               volume = 5)
}

loadTs <- function(symbol) {
    src <- getSymbols(symbol, src = "yahoo", auto.assign = FALSE)
    prepareTs(src)
}

if (FALSE) {
    GDAXI <- loadTs("^GDAXI")
    saveRDS(GDAXI, "GDAXI.RDS")

    EOAN <- loadTs("EOAN.DE")
    saveRDS(EOAN, "EOAN.RDS")

    DB1 <- loadTs("DB1.DE")
    saveRDS(DB1, "DB1.RDS")

    VNA <- loadTs("VNA.DE")
    saveRDS(VNA, "VNA.RDS")

    DTE <- loadTs("DTE.DE")
    saveRDS(DTE, "DTE.RDS")

    RWE <- loadTs("RWE.DE")
    saveRDS(RWE, "RWE.RDS")

    MRK <- loadTs("MRK.DE")
    saveRDS(MRK, "MRK.RDS")

    BEI <- loadTs("BEI.DE")
    saveRDS(BEI, "BEI.RDS")

    DPW <- loadTs("DPW.DE")
    saveRDS(DPW, "DPW.RDS")

    MUV2 <- loadTs("MUV2.DE")
    saveRDS(MUV2, "MUV2.RDS")

    FME <- loadTs("FME.DE")
    saveRDS(FME, "FME.RDS")

    ALV <- loadTs("ALV.DE")
    saveRDS(ALV, "ALV.RDS")

    VOW3 <- loadTs("VOW3.DE")
    saveRDS(VOW3, "VOW3.RDS")

    HEN3 <- loadTs("HEN3.DE")
    saveRDS(HEN3, "HEN3.RDS")

    LHA <- loadTs("LHA.DE")
    saveRDS(LHA, "LHA.RDS")

    ADS <- loadTs("ADS.DE")
    saveRDS(ADS, "ADS.RDS")

    SIE <- loadTs("SIE.DE")
    saveRDS(SIE, "SIE.RDS")

    LIN <- loadTs("LIN.DE")
    saveRDS(LIN, "LIN.RDS")

    CON <- loadTs("CON.DE")
    saveRDS(CON, "CON.RDS")

    BAYN <- loadTs("BAYN.DE")
    saveRDS(BAYN, "BAYN.RDS")

    BMW <- loadTs("BMW.DE")
    saveRDS(BMW, "BMW.RDS")

    HEI <- loadTs("HEI.DE")
    saveRDS(HEI, "HEI.RDS")

    SAP <- loadTs("SAP.DE")
    saveRDS(SAP, "SAP.RDS")

    DAI <- loadTs("DAI.DE")
    saveRDS(DAI, "DAI.RDS")

    BAS <- loadTs("BAS.DE")
    saveRDS(BAS, "BAS.RDS")

    COV <- loadTs("1COV.DE")
    saveRDS(COV, "COV.RDS")

    WDI <- loadTs("WDI.DE")
    saveRDS(WDI, "WDI.RDS")

    DBK <- loadTs("DBK.DE")
    saveRDS(DBK, "DBK.RDS")

    TKA <- loadTs("TKA.DE")
    saveRDS(TKA, "TKA.RDS")

    FRE <- loadTs("FRE.DE")
    saveRDS(FRE, "FRE.RDS")

    IFX <- loadTs("IFX.DE")
    saveRDS(IFX, "IFX.RDS")
}

reduceTs <- function(symbol) {
    filename <- paste0(symbol, ".RDS")
    ticker <- readRDS(file = filename)
    ticker <- ticker %>%
        select(ts, close, adjusted) %>%
        filter(!is.na(close))
    as.data.frame(ticker)
}

makeItem <- function(symbol, name) {
    list("symbol" = symbol, "name" = name, "ts" = reduceTs(symbol))
}

DAX <- list(
    "GDAXI" = makeItem("GDAXI", "DAX Performance Index"),
    "EOAN" = makeItem("EOAN", "E.ON SE"),
    "DB1" = makeItem("DB1", "Deutsche Börse Aktiengesellschaft"),
    "VNA" = makeItem("VNA", "Vonovia SE"),
    "DTE" = makeItem("DTE", "Deutsche Telekom AG"),
    "RWE" = makeItem("RWE", "RWE Aktiengesellschaft"),
    "MRK" = makeItem("MRK", "MERCK Kommanditgesellschaft auf Aktien"),
    "BEI" = makeItem("BEI", "Beiersdorf Aktiengesellschaft"),
    "DPW" = makeItem("DPW", "Deutsche Post AG"),
    "MUV2" = makeItem("MUV2", "Münchener Rückversicherungs-Gesellschaft Aktiengesellschaft"),
    "FME" = makeItem("FME", "Fresenius Medical Care AG & Co. KGaA"),
    "ALV" = makeItem("ALV", "Allianz SE"),
    "VOW3" = makeItem("VOW3", "Volkswagen AG"),
    "HEN3" = makeItem("HEN3", "Henkel AG & Co. KGaA"),
    "LHA" = makeItem("LHA", "Deutsche Lufthansa AG"),
    "ADS" = makeItem("ADS", "Adidas AG"),
    "SIE" = makeItem("SIE", "Siemens Aktiengesellschaft"),
    "LIN" = makeItem("LIN", "Linde plc"),
    "CON" = makeItem("CON", "Continental Aktiengesellschaft"),
    "BAYN" = makeItem("BAYN", "Bayer Aktiengesellschaft"),
    "BMW" = makeItem("BMW", "Bayerische Motoren Werke Aktiengesellschaft"),
    "HEI" = makeItem("HEI", "HeidelbergCement AG"),
    "SAP" = makeItem("SAP", "SAP SE"),
    "DAI" = makeItem("DAI", "Daimler AG"),
    "BAS" = makeItem("BAS", "BASF SE"),
    "COV" = makeItem("COV", "Covestro AG"),
    "WDI" = makeItem("WDI", "Wirecard AG"),
    "DBK" = makeItem("DBK", "Deutsche Bank Aktiengesellschaft"),
    "TKA" = makeItem("TKA", "Thyssenkrupp AG"),
    "FRE" = makeItem("FRE", "Fresenius SE & Co. KGaA"),
    "IFX" = makeItem("IFX", "Infineon Technologies AG")
)

saveRDS(DAX, "DAX.RDS")

# EOAN.DE -> E.ON SE
# DB1.DE -> Deutsche Börse Aktiengesellschaft
# VNA.DE -> Vonovia SE
# DTE.DE -> Deutsche Telekom AG
# RWE.DE -> RWE Aktiengesellschaft
# MRK.DE -> MERCK Kommanditgesellschaft auf Aktien
# BEI.DE -> Beiersdorf Aktiengesellschaft
# DPW.DE -> Deutsche Post AG
# MUV2.DE -> Münchener Rückversicherungs-Gesellschaft Aktiengesellschaft
# FME.DE -> Fresenius Medical Care AG & Co. KGaA
# ALV.DE -> Allianz SE
# VOW3.DE -> Volkswagen AG
# HEN3.DE -> Henkel AG & Co. KGaA
# LHA.DE -> Deutsche Lufthansa AG
# ADS.DE -> adidas AG
# SIE.DE -> Siemens Aktiengesellschaft
# LIN.DE -> Linde plc
# CON.DE -> Continental Aktiengesellschaft
# BAYN.DE -> Bayer Aktiengesellschaft
# BMW.DE -> Bayerische Motoren Werke Aktiengesellschaft
# HEI.DE -> HeidelbergCement AG
# SAP.DE -> SAP SE
# DAI.DE -> Daimler AG
# BAS.DE -> BASF SE
# 1COV.DE -> Covestro AG
# WDI.DE -> Wirecard AG
# DBK.DE -> Deutsche Bank Aktiengesellschaft
# TKA.DE -> thyssenkrupp AG
# FRE.DE -> Fresenius SE & Co. KGaA
# IFX.DE -> Infineon Technologies AG



