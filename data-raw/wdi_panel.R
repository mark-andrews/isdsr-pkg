################################################################################
# data-raw/wdi_panel.R
#
# PURPOSE:
#   Creates **wdi_panel**, a tidy country-year panel of headline development
#   statistics (1990–2024).  Indicators come from the **World Development
#   Indicators (WDI)** web-API maintained by the World Bank. There are 30
#   indicators covering economy, people, environment, gender and governance,
#   with good coverage for >90 % of countries.
#
#   Data source & licence:
#     World Bank. (2025). *World Development Indicators* (database).
#     Washington, DC: World Bank. https://databank.worldbank.org/source/world-development-indicators
#     Licensed under Creative Commons Attribution 4.0 International (CC BY 4.0)
#     https://creativecommons.org/licenses/by/4.0/
#
# The script below
#   1. Defines a named vector `wb_codes` that maps friendly column names to
#      the official WDI indicator codes.
#   2. Calls WDI() once to pull **all** countries, 1990–2024, plus the metadata
#      columns `iso3c`, `region`, `income`.
#   3. Saves the resulting tibble verbatim -- no values altered -- into an
#      internal data object `wdi_panel`.
#
################################################################################

library(WDI) # CRAN ≥ 2.7.9 recommended for up-to-date indicator cache

## ---------------------------------------------------------------------------
## 1. Indicator dictionary (friendly name  ->  WDI code)
##    Duplicates removed; categories grouped for readability
## ---------------------------------------------------------------------------

wb_codes <- c(
  # Economy
  gdp_ppp      = "NY.GDP.MKTP.PP.KD", # total GDP, PPP, 2017 intl $
  gdp_pc_ppp   = "NY.GDP.PCAP.PP.KD", # GDP per capita, PPP, 2017 intl $
  gdp_growth   = "NY.GDP.MKTP.KD.ZG", # real GDP growth, %
  inflation    = "FP.CPI.TOTL.ZG", # CPI inflation, %
  priv_credit  = "FS.AST.PRVT.GD.ZS", # domestic credit to private sector, % GDP
  trade_open   = "NE.TRD.GNFS.ZS", # trade openness, % GDP
  fdi_inflow   = "BX.KLT.DINV.WD.GD.ZS", # FDI net inflows, % GDP

  # People & demographics
  pop_tot      = "SP.POP.TOTL",
  urban_pct    = "SP.URB.TOTL.IN.ZS",
  life_exp     = "SP.DYN.LE00.IN",
  u5_mort      = "SH.DYN.MORT",
  sec_enrol    = "SE.SEC.ENRR",
  health_pc    = "SH.XPD.CHEX.PC.CD",

  # Labour & inequality
  unemployment = "SL.UEM.TOTL.ZS",
  poverty215   = "SI.POV.DDAY",
  gini         = "SI.POV.GINI",
  lab_part     = "SL.TLF.CACT.ZS",

  # Gender-specific
  lfp_ratio    = "SL.TLF.CACT.FM.NE.ZS",
  lfp_female   = "SL.TLF.CACT.FE.ZS",
  lfp_male     = "SL.TLF.CACT.MA.ZS",
  hci_all      = "HD.HCI.OVRL",
  hci_female   = "HD.HCI.OVRL.FE",
  hci_male     = "HD.HCI.OVRL.MA",

  # Infrastructure & tech
  elec_acc     = "EG.ELC.ACCS.ZS",
  net_users    = "IT.NET.USER.ZS",

  # Environment
  renew_elec   = "EG.ELC.RNEW.ZS",
  forest_pct   = "AG.LND.FRST.ZS",
  homicide     = "VC.IHR.PSRC.P5" # security / governance
)

## ---------------------------------------------------------------------------
## 2. Pull data from the API
## ---------------------------------------------------------------------------

wdi_panel <- WDI(
  country   = "all", # 217 economies + aggregates
  indicator = wb_codes,
  start     = 1990,
  end       = 2024,
  extra     = TRUE # adds iso3c, region, income group, lending type
)

## ---------------------------------------------------------------------------
## 3. Save for package use (compressed = "xz" keeps file tiny)
## ---------------------------------------------------------------------------

usethis::use_data(wdi_panel, overwrite = TRUE, compress = "xz") # last run: 27 June 2025
