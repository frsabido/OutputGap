library(shinydashboard)
library(shiny)
library(dashboardthemes)
library(WDI)
library(wbstats)
library(ggplot2)
library(DT)
library(dplyr)
library(highcharter)
library(broom)
library(mFilter)
library(dlm)
library(KFAS)
library(rdbnomics)
library(forecast)
library(readxl)
library(knitr)

customTheme <- shinyDashboardThemeDIY(
    
    ### general
    appFontFamily = "Arial"
    ,appFontColor = "rgb(0,0,0)"
    ,primaryFontColor = "rgb(0,0,0)"
    ,infoFontColor = "rgb(0,0,0)"
    ,successFontColor = "rgb(0,0,0)"
    ,warningFontColor = "rgb(0,0,0)"
    ,dangerFontColor = "rgb(0,0,0)"
    ,bodyBackColor = "rgb(256,256,256)"
    
    ### header
    ,logoBackColor = "rgb(23,103,124)"
    
    ,headerButtonBackColor = "rgb(238,238,238)"
    ,headerButtonIconColor = "rgb(75,75,75)"
    ,headerButtonBackColorHover = "rgb(210,210,210)"
    ,headerButtonIconColorHover = "rgb(0,0,0)"
    
    ,headerBackColor = "rgb(238,238,238)"
    ,headerBoxShadowColor = "#aaaaaa"
    ,headerBoxShadowSize = "2px 2px 2px"
    
    ### sidebar
    ,sidebarBackColor = cssGradientThreeColors(
        direction = "down"
        ,colorStart = "rgb(20,97,117)"
        ,colorMiddle = "rgb(56,161,187)"
        ,colorEnd = "rgb(3,22,56)"
        ,colorStartPos = 0
        ,colorMiddlePos = 50
        ,colorEndPos = 100
    )
    ,sidebarPadding = 0
    
    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 0
    
    ,sidebarShadowRadius = "3px 5px 5px"
    ,sidebarShadowColor = "#aaaaaa"
    
    ,sidebarUserTextColor = "rgb(255,255,255)"
    
    ,sidebarSearchBackColor = "rgb(55,72,80)"
    ,sidebarSearchIconColor = "rgb(153,153,153)"
    ,sidebarSearchBorderColor = "rgb(55,72,80)"
    
    ,sidebarTabTextColor = "rgb(255,255,255)"
    ,sidebarTabTextSize = 13
    ,sidebarTabBorderStyle = "none none solid none"
    ,sidebarTabBorderColor = "rgb(35,106,135)"
    ,sidebarTabBorderWidth = 1
    
    ,sidebarTabBackColorSelected = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgba(44,222,235,1)"
        ,colorMiddle = "rgba(44,222,235,1)"
        ,colorEnd = "rgba(0,255,213,1)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
    )
    ,sidebarTabTextColorSelected = "rgb(0,0,0)"
    ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
    
    ,sidebarTabBackColorHover = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgba(44,222,235,1)"
        ,colorMiddle = "rgba(44,222,235,1)"
        ,colorEnd = "rgba(0,255,213,1)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
    )
    ,sidebarTabTextColorHover = "rgb(50,50,50)"
    ,sidebarTabBorderStyleHover = "none none solid none"
    ,sidebarTabBorderColorHover = "rgb(75,126,151)"
    ,sidebarTabBorderWidthHover = 1
    ,sidebarTabRadiusHover = "0px 20px 20px 0px"
    
    ### boxes
    ,boxBackColor = "rgb(255,255,255)"
    ,boxBorderRadius = 5
    ,boxShadowSize = "0px 1px 1px"
    ,boxShadowColor = "rgba(0,0,0,.1)"
    ,boxTitleSize = 16
    ,boxDefaultColor = "rgb(210,214,220)"
    ,boxPrimaryColor = "rgba(44,222,235,1)"
    ,boxInfoColor = "rgb(210,214,220)"
    ,boxSuccessColor = "rgba(0,255,213,1)"
    ,boxWarningColor = "rgb(244,156,104)"
    ,boxDangerColor = "rgb(255,88,55)"
    
    ,tabBoxTabColor = "rgb(255,255,255)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "rgb(0,0,0)"
    ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
    ,tabBoxBackColor = "rgb(255,255,255)"
    ,tabBoxHighlightColor = "rgba(44,222,235,1)"
    ,tabBoxBorderRadius = 5
    
    ### inputs
    ,buttonBackColor = "rgb(245,245,245)"
    ,buttonTextColor = "rgb(0,0,0)"
    ,buttonBorderColor = "rgb(200,200,200)"
    ,buttonBorderRadius = 5
    
    ,buttonBackColorHover = "rgb(235,235,235)"
    ,buttonTextColorHover = "rgb(100,100,100)"
    ,buttonBorderColorHover = "rgb(200,200,200)"
    
    ,textboxBackColor = "rgb(255,255,255)"
    ,textboxBorderColor = "rgb(200,200,200)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(245,245,245)"
    ,textboxBorderColorSelect = "rgb(200,200,200)"
    
    ### tables
    ,tableBackColor = "rgb(255,255,255)"
    ,tableBorderColor = "rgb(240,240,240)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1
    
)

load(file = "codigos.Rda")

#TABLAS DE DATOS
#tparo<-rdb(ids = paste0('AMECO/ZUTN/',codigos$CODE,'.1.0.0.0.ZUTN')) %>%  filter(!is.na(value)) #Tasa de Paro
#gdpdefl <- rdb(ids = paste0('AMECO/PVGD/',codigos$CODE,'.3.1.0.0.PVGD')) %>%  filter(!is.na(value))  #Deflactor del PIB
#cpi<-rdb(ids = paste0('AMECO/PCPH/',codigos$CODE,'.3.1.0.0.PCPH')) %>%  filter(!is.na(value))  #Deflactor de Consumo
#gdpq<-rdb(ids = paste0('AMECO/OVGD/',codigos$CODE,'.1.1.0.0.OVGD')) %>%  filter(!is.na(value))  #PIB a precios de 2015
#gdpn<-rdb(ids = paste0('AMECO/UVGD/',codigos$CODE,'.1.0.0.0.UVGD')) %>%  filter(!is.na(value))  #PIB a precios corrientes
#pibpc<-rdb(ids = paste0('AMECO/HVGDP/',codigos$CODE,'.1.0.99.0.HVGDP')) %>%  filter(!is.na(value))  #PIB per capita
#ras<-rdb(ids = paste0('AMECO/UWCD/',codigos$CODE,'.1.0.0.0.UWCD')) %>%  filter(!is.na(value))  #RAS
#alcd0<-rdb(ids = paste0('AMECO/ALCD0/',codigos$CODE,'.1.0.0.0.ALCD0')) %>%  filter(!is.na(value))  #RAS por empleado como porcentaje del PIBpc
#dlprod<-rdb(ids = paste0('AMECO/RVGDE/',codigos$CODE,'.1.1.0.0.RVGDE')) %>%  filter(!is.na(value))  #PIB a precios de 2015 por persona empleada
#hwcdw<-rdb(ids = paste0('AMECO/HWCDW/',codigos$CODE,'.1.0.0.0.HWCDW')) %>%  filter(!is.na(value))  #RAS por empleado
#hpere<-rdb(ids = paste0('AMECO/NLHA/',codigos$CODE,'.1.0.0.0.NLHA')) %>%  filter(!is.na(value))  #Horas medias anuales por persona empleada
#ocuEPA<-rdb(ids = paste0('AMECO/NECN/',codigos$CODE,'.1.0.0.0.NECN')) %>%  filter(!is.na(value))  #Personas Ocupadas EPA
#ocuCN<-rdb(ids = paste0('AMECO/NETD/',codigos$CODE,'.1.0.0.0.NETD')) %>%  filter(!is.na(value))  #Personas Ocupadas Cuentas Nacionales
#nulc<-rdb(ids = paste0('AMECO/PLCD/',codigos$CODE,'.3.1.0.0.PLCD')) %>%  filter(!is.na(value))  #Coste Laboral Unitario (RASpe/RealGDPpc)
#popCN<-rdb(ids = paste0('AMECO/NPTD/',codigos$CODE,'.1.0.0.0.NPTD')) %>%  filter(!is.na(value))  #Población Total Cuentas Nacionales
#pop<-rdb(ids = paste0('AMECO/NPTN/',codigos$CODE,'.1.0.0.0.NPTN')) %>%  filter(!is.na(value))  #Población Total
#pop_15_64<-rdb(ids = paste0('AMECO/NPAN/',codigos$CODE,'.1.0.0.0.NPAN')) %>%  filter(!is.na(value))  #Población Total entre 15 y 64 años
#pact<-rdb(ids = paste0('AMECO/NLTN/',codigos$CODE,'.1.0.0.0.NLTN')) %>%  filter(!is.na(value))  #Población Activa
#pactCN<-rdb(ids = paste0('AMECO/NLCN/',codigos$CODE,'.1.0.0.0.NLCN')) %>%  filter(!is.na(value))  #Población Activa Cuentas Nacionales
#fbkfq<-rdb(ids = paste0('AMECO/OIGT/',codigos$CODE,'.1.1.0.0.OIGT')) %>%  filter(!is.na(value))  #FBCF a precios de 2015
#kq<-rdb(ids = paste0('AMECO/OKND/',codigos$CODE,'.1.0.0.0.OKND')) %>%  filter(!is.na(value))  #Stock de Capital a precios de 2015
#nawru<-rdb(ids = paste0('AMECO/ZNAWRU/',codigos$CODE,'.1.0.0.0.ZNAWRU')) %>%  filter(!is.na(value))  #Nawru
#potgdp<-rdb(ids = paste0('AMECO/OVGDP/',codigos$CODE,'.1.0.0.0.OVGDP')) %>%  filter(!is.na(value))  #PIB potencial
#outpg<-rdb(ids = paste0('AMECO/AVGDGP/',codigos$CODE,'.1.0.0.0.AVGDGP')) %>%  filter(!is.na(value))  #Output Gap
#trendgdp<-rdb(ids = paste0('AMECO/OVGDT/',codigos$CODE,'.1.0.0.0.OVGDT')) %>%  filter(!is.na(value))  #PIB tendencial
#privconscont<-rdb(ids = paste0('AMECO/CVGD0/',codigos$CODE,'.1.0.0.0.CVGD0')) %>%  filter(!is.na(value))  #Contribución Consumo Privado a Variación PIB
#pubconscont<-rdb(ids = paste0('AMECO/CVGD1/',codigos$CODE,'.1.0.0.0.CVGD1')) %>%  filter(!is.na(value))  #Contribución Consumo Público a Variación PIB
#fbcfcont<-rdb(ids = paste0('AMECO/CVGD2/',codigos$CODE,'.1.0.0.0.CVGD2')) %>%  filter(!is.na(value))  #Contribución FBCF a Variación PIB
#expcont<-rdb(ids = paste0('AMECO/CVGD6/',codigos$CODE,'.1.0.0.0.CVGD6')) %>%  filter(!is.na(value))  #Contribución Exportaciones a Variación PIB
#impcont<-rdb(ids = paste0('AMECO/CVGD8/',codigos$CODE,'.1.0.0.0.CVGD8')) %>%  filter(!is.na(value))  #Contribución Importaciones a Variación PIB
#tfp<-rdb(ids = paste0('AMECO/ZVGDF/',codigos$CODE,'.3.0.0.0.ZVGDF')) %>%  filter(!is.na(value))  #Total Factor Productivity
#cab<-rdb(ids = paste0('AMECO/UBCABOP/',codigos$CODE,'.1.0.310.0.UBCABOP')) %>%  filter(!is.na(value))  #Current Account Balance
#ner<-rdb(ids = paste0('AMECO/XUNNQ/',codigos$CODE,'.3.0.30.437.XUNNQ')) %>%  filter(!is.na(value))  #Nominal Effective Exchange Rate (2015=100)
#rer<-rdb(ids = paste0('AMECO/XUNRQ/',codigos$CODE,'.3.0.30.437.XUNRQ')) %>%  filter(!is.na(value))  #Real Effective Exchange Rate based on unit labour cost (2015=100)
#nstir<-rdb(ids = paste0('AMECO/ISN/',codigos$CODE,'.1.1.0.0.ISN')) %>%  filter(!is.na(value))  #Nominal Short Term Interest Rates
#rstir<-rdb(ids = paste0('AMECO/ISRV/',codigos$CODE,'.1.1.0.0.ISRV')) %>%  filter(!is.na(value))  #Real Short Term Interest Rates
#nltir<-rdb(ids = paste0('AMECO/ILN/',codigos$CODE,'.1.1.0.0.ILN')) %>%  filter(!is.na(value))  #Nominal Long Term Interest Rates
#rltir<-rdb(ids = paste0('AMECO/ILRV/',codigos$CODE,'.1.1.0.0.ILRV')) %>%  filter(!is.na(value))  #Real Long Term Interest Rates
#yc<-rdb(ids = paste0('AMECO/IYN/',codigos$CODE,'.1.1.0.0.IYN')) %>%  filter(!is.na(value))  #Yield Curve
#ctb<-rdb(ids = paste0('AMECO/UTCT/',codigos$CODE,'.1.0.319.0.UTCT')) %>%  filter(!is.na(value))  #Current Tax Burden (% GDP)
#govrev<-rdb(ids = paste0('AMECO/URTG/',codigos$CODE,'.1.0.319.0.URTG')) %>%  filter(!is.na(value))  #Total Gov Revenue (% GDP)
#govexp<-rdb(ids = paste0('AMECO/UUCG/',codigos$CODE,'.1.0.319.0.UUCG')) %>%  filter(!is.na(value))  #Total Gov Expenditure (% GDP)
#nlnb<-rdb(ids = paste0('AMECO/UBLG/',codigos$CODE,'.1.0.319.0.UBLG')) %>%  filter(!is.na(value))  #Net Lending or Borrowing (% GDP)
#debt<-rdb(ids = paste0('AMECO/UDGGL/',codigos$CODE,'.1.0.319.0.UDGGL')) %>%  filter(!is.na(value))  #Consolidated Gross Debt (% GDP)

tablapais<-function(pais){
    tparo<-rdb(ids = paste0('AMECO/ZUTN/',pais,'.1.0.0.0.ZUTN')) %>%  filter(!is.na(value)) #Tasa de Paro
    gdpdefl <- rdb(ids = paste0('AMECO/PVGD/',pais,'.3.1.0.0.PVGD')) %>%  filter(!is.na(value))  #Deflactor del PIB
    cpi<-rdb(ids = paste0('AMECO/PCPH/',pais,'.3.1.0.0.PCPH')) %>%  filter(!is.na(value))  #Deflactor de Consumo
    gdpq<-rdb(ids = paste0('AMECO/OVGD/',pais,'.1.1.0.0.OVGD')) %>%  filter(!is.na(value))  #PIB a precios de 2015
    gdpn<-rdb(ids = paste0('AMECO/UVGD/',pais,'.1.0.0.0.UVGD')) %>%  filter(!is.na(value))  #PIB a precios corrientes
    pibpc<-rdb(ids = paste0('AMECO/HVGDP/',pais,'.1.0.99.0.HVGDP')) %>%  filter(!is.na(value))  #PIB per capita
    ras<-rdb(ids = paste0('AMECO/UWCD/',pais,'.1.0.0.0.UWCD')) %>%  filter(!is.na(value))  #RAS
    alcd0<-rdb(ids = paste0('AMECO/ALCD0/',pais,'.1.0.0.0.ALCD0')) %>%  filter(!is.na(value))  #RAS por empleado como porcentaje del PIBpc
    dlprod<-rdb(ids = paste0('AMECO/RVGDE/',pais,'.1.1.0.0.RVGDE')) %>%  filter(!is.na(value))  #PIB a precios de 2015 por persona empleada
    hwcdw<-rdb(ids = paste0('AMECO/HWCDW/',pais,'.1.0.0.0.HWCDW')) %>%  filter(!is.na(value))  #RAS por empleado
    hpere<-rdb(ids = paste0('AMECO/NLHA/',pais,'.1.0.0.0.NLHA')) %>%  filter(!is.na(value))  #Horas medias anuales por persona empleada
    ocuEPA<-rdb(ids = paste0('AMECO/NECN/',pais,'.1.0.0.0.NECN')) %>%  filter(!is.na(value))  #Personas Ocupadas EPA
    ocuCN<-rdb(ids = paste0('AMECO/NETD/',pais,'.1.0.0.0.NETD')) %>%  filter(!is.na(value))  #Personas Ocupadas Cuentas Nacionales
    nulc<-rdb(ids = paste0('AMECO/PLCD/',pais,'.3.1.0.0.PLCD')) %>%  filter(!is.na(value))  #Coste Laboral Unitario (RASpe/RealGDPpc)
    popCN<-rdb(ids = paste0('AMECO/NPTD/',pais,'.1.0.0.0.NPTD')) %>%  filter(!is.na(value))  #Población Total Cuentas Nacionales
    pop<-rdb(ids = paste0('AMECO/NPTN/',pais,'.1.0.0.0.NPTN')) %>%  filter(!is.na(value))  #Población Total
    pop_15_64<-rdb(ids = paste0('AMECO/NPAN/',pais,'.1.0.0.0.NPAN')) %>%  filter(!is.na(value))  #Población Total entre 15 y 64 años
    pact<-rdb(ids = paste0('AMECO/NLTN/',pais,'.1.0.0.0.NLTN')) %>%  filter(!is.na(value))  #Población Activa
    pactCN<-rdb(ids = paste0('AMECO/NLCN/',pais,'.1.0.0.0.NLCN')) %>%  filter(!is.na(value))  #Población Activa Cuentas Nacionales
    fbkfq<-rdb(ids = paste0('AMECO/OIGT/',pais,'.1.1.0.0.OIGT')) %>%  filter(!is.na(value))  #FBCF a precios de 2015
    kq<-rdb(ids = paste0('AMECO/OKND/',pais,'.1.0.0.0.OKND')) %>%  filter(!is.na(value))  #Stock de Capital a precios de 2015
    nawru<-rdb(ids = paste0('AMECO/ZNAWRU/',pais,'.1.0.0.0.ZNAWRU')) %>%  filter(!is.na(value))  #Nawru
    potgdp<-rdb(ids = paste0('AMECO/OVGDP/',pais,'.1.0.0.0.OVGDP')) %>%  filter(!is.na(value))  #PIB potencial
    outpg<-rdb(ids = paste0('AMECO/AVGDGP/',pais,'.1.0.0.0.AVGDGP')) %>%  filter(!is.na(value))  #Output Gap
    trendgdp<-rdb(ids = paste0('AMECO/OVGDT/',pais,'.1.0.0.0.OVGDT')) %>%  filter(!is.na(value))  #PIB tendencial
    privconscont<-rdb(ids = paste0('AMECO/CVGD0/',pais,'.1.0.0.0.CVGD0')) %>%  filter(!is.na(value))  #Contribución Consumo Privado a Variación PIB
    pubconscont<-rdb(ids = paste0('AMECO/CVGD1/',pais,'.1.0.0.0.CVGD1')) %>%  filter(!is.na(value))  #Contribución Consumo Público a Variación PIB
    fbcfcont<-rdb(ids = paste0('AMECO/CVGD2/',pais,'.1.0.0.0.CVGD2')) %>%  filter(!is.na(value))  #Contribución FBCF a Variación PIB
    expcont<-rdb(ids = paste0('AMECO/CVGD6/',pais,'.1.0.0.0.CVGD6')) %>%  filter(!is.na(value))  #Contribución Exportaciones a Variación PIB
    impcont<-rdb(ids = paste0('AMECO/CVGD8/',pais,'.1.0.0.0.CVGD8')) %>%  filter(!is.na(value))  #Contribución Importaciones a Variación PIB
    tfp<-rdb(ids = paste0('AMECO/ZVGDF/',pais,'.3.0.0.0.ZVGDF')) %>%  filter(!is.na(value))  #Total Factor Productivity
    cab<-rdb(ids = paste0('AMECO/UBCABOP/',pais,'.1.0.310.0.UBCABOP')) %>%  filter(!is.na(value))  #Current Account Balance
    #ner<-rdb(ids = paste0('AMECO/XUNNQ/',pais,'.3.0.30.437.XUNNQ')) %>%  filter(!is.na(value))  #Nominal Effective Exchange Rate (2015=100)
    #rer<-rdb(ids = paste0('AMECO/XUNRQ/',pais,'.3.0.30.437.XUNRQ')) %>%  filter(!is.na(value))  #Real Effective Exchange Rate based on unit labour cost (2015=100)
    nstir<-rdb(ids = paste0('AMECO/ISN/',pais,'.1.1.0.0.ISN')) %>%  filter(!is.na(value))  #Nominal Short Term Interest Rates
    rstir<-rdb(ids = paste0('AMECO/ISRV/',pais,'.1.1.0.0.ISRV')) %>%  filter(!is.na(value))  #Real Short Term Interest Rates
    nltir<-rdb(ids = paste0('AMECO/ILN/',pais,'.1.1.0.0.ILN')) %>%  filter(!is.na(value))  #Nominal Long Term Interest Rates
    rltir<-rdb(ids = paste0('AMECO/ILRV/',pais,'.1.1.0.0.ILRV')) %>%  filter(!is.na(value))  #Real Long Term Interest Rates
    yc<-rdb(ids = paste0('AMECO/IYN/',pais,'.1.1.0.0.IYN')) %>%  filter(!is.na(value))  #Yield Curve
    ctb<-rdb(ids = paste0('AMECO/UTCT/',pais,'.1.0.319.0.UTCT')) %>%  filter(!is.na(value))  #Current Tax Burden (% GDP)
    govrev<-rdb(ids = paste0('AMECO/URTG/',pais,'.1.0.319.0.URTG')) %>%  filter(!is.na(value))  #Total Gov Revenue (% GDP)
    govexp<-rdb(ids = paste0('AMECO/UUCG/',pais,'.1.0.319.0.UUCG')) %>%  filter(!is.na(value))  #Total Gov Expenditure (% GDP)
    nlnb<-rdb(ids = paste0('AMECO/UBLG/',pais,'.1.0.319.0.UBLG')) %>%  filter(!is.na(value))  #Net Lending or Borrowing (% GDP)
    debt<-rdb(ids = paste0('AMECO/UDGGL/',pais,'.1.0.319.0.UDGGL')) %>%  filter(!is.na(value))  #Consolidated Gross Debt (% GDP)
    #popaux<-rdb(ids = paste0('Eurostat/tipslm16/A.THS.T.TOTAL.Y15-74.POP.',pais)) %>%  filter(!is.na(value))  #Consolidated Gross Debt (% GDP)
    
    datos<-as.data.frame(rbind(tparo,gdpdefl,cpi,gdpq,gdpn,pibpc,ras,alcd0,dlprod,hwcdw,hpere,ocuEPA,ocuCN,nulc,popCN,pop,pop_15_64,pact,pactCN,fbkfq,kq,nawru,potgdp,outpg,trendgdp,privconscont,pubconscont,fbcfcont,expcont,impcont,tfp,cab,nstir,rstir,nltir,rltir,yc,ctb,govrev,govexp,nlnb,debt))
    return(datos)
}


#INDICADORES
pibkID="OVGD"
pibcorrID="UVGD"
pibpcID="HVGDP"
tparoID="ZUTN"
inflID="PCPH"
cabID="UBCABOP"
gtoID="UUCG"
ingID="URTG"
compempID="HWCDW"
empID="NETD"
popID="NPAN"
defID="UBLG"
privconsID="CVGD0"
pubconsID="CVGD1"
gcfID="CVGD2"
expID="CVGD6"
impID="CVGD8"
labforID="NLTN"
ktID="OKND"
prodwork<-"RVGDE"
horasID<-"NLHA"
ogcID<-"AVGDGP"
nawID<-"ZNAWRU"
#privconsKID="NE.CON.PRVT.KD"
#pubconsKID="NE.CON.GOVT.KD"
#gcfKID="NE.GDI.TOTL.KD"
#expKID="NE.EXP.GNFS.KD"
#impKID="NE.IMP.GNFS.KD"
#varprivconsKID="NE.CON.PRVT.KD.ZG"
#varpubconsKID="NE.CON.GOVT.KD.ZG"
#vargcfKID="NE.GDI.TOTL.KD.ZG"
#varexpKID="NE.EXP.GNFS.KD.ZG"
#varimpKID="NE.IMP.GNFS.KD.ZG"

#salworID="SL.EMP.WORK.ZS"

#indicadores=c(pibkID,pibcorrID,varpibID,tparoID,inflID,cabID,gtoID,ingID,compempID,empID,privconsID,pubconsID,gcfID,expID,impID,privconsKID,pubconsKID,gcfKID,expKID,impKID,varprivconsKID,varpubconsKID,vargcfKID,varexpKID,varimpKID,labforID,popID,salworID,pibpcID)

filtra<-function(datos,ind){
    colnames(datos)[9]<-"date"
    datos%>%filter(dataset_code==ind)%>%select(date,period,value)%>%mutate(base2015=100*value/value[date==2015],incremental_rate=(value/lag(value)-1)+1,growth_rate=value/lag(value)-1,growth_rate_pp=(value/lag(value)-1)*100)%>%arrange(date)
}

phillips<-function(datos){
    auxi<-na.omit(left_join(filtra(datos,tparoID),filtra(datos,compempID),by="date"))
    auxi<-na.omit(auxi[,c(1,13,3)])
    colnames(auxi)<-c("Date","Inflation","UR")
    auxi<-auxi[auxi$Date>1999,]
    auxi1<-auxi[auxi$Date<2008,]
    auxi2<-auxi[auxi$Date>2007&auxi$Date<2014,]
    auxi3<-auxi[auxi$Date>2013,]
    modlss1 <- lm(Inflation ~ UR, data = auxi1)
    fit1 <- arrange(augment(modlss1), UR) %>% 
        mutate(.se = predict(modlss1, se = TRUE)$se.fit)
    modlss2 <- lm(Inflation ~ UR, data = auxi2)
    fit2 <- arrange(augment(modlss2), UR) %>% 
        mutate(.se = predict(modlss2, se = TRUE)$se.fit)
    modlss3 <- lm(Inflation ~ UR, data = auxi3)
    fit3 <- arrange(augment(modlss3), UR) %>% 
        mutate(.se = predict(modlss3, se = TRUE)$se.fit)
    hc <- hchart(
        auxi,
        type = "scatter",
        hcaes(x = UR, y = Inflation),
        name = "Phillips Curve",
        showInLegend = FALSE
    ) %>%
        hc_plotOptions(
            series = list(
                dataLabels = list(
                    enabled = TRUE,
                    format="{point.Date}",
                    color="grey",
                    style = list(textOutline = FALSE)
                )
            )
        )%>%
        hc_tooltip(pointFormat = "Inflation: {point.y:.2f} <br> UR: {point.x:.2f}")%>%
        hc_title(text="Phillips Curve")%>%
        hc_add_series(
            fit1,
            type = "spline",
            hcaes(x=UR,y=.fitted),
            name = "Fit",
            id = "fit", # this is for link the arearange series to this one and have one legend
        )%>% 
        hc_add_series(
            fit2,
            type = "spline",
            hcaes(x=UR,y=.fitted),
            name = "Fit",
            id = "fit", # this is for link the arearange series to this one and have one legend
        )%>% 
        hc_add_series(
            fit3,
            type = "spline",
            hcaes(x=UR,y=.fitted),
            name = "Fit",
            id = "fit", # this is for link the arearange series to this one and have one legend
        )%>% 
        hc_add_theme(hc_theme_gridlight())
}

cabur<-function(datos){
    auxi<-na.omit(left_join(filtra(datos,tparoID),filtra(datos,cabID),by="date"))
    auxi<-auxi[,c(1,9,3)]
    colnames(auxi)<-c("Date","CAB","UR")
    auxi<-auxi[auxi$Date>1999,]
    auxi1<-auxi[auxi$Date<2008,]
    auxi2<-auxi[auxi$Date>2007&auxi$Date<2014,]
    auxi3<-auxi[auxi$Date>2013,]
    modlss1 <- lm(CAB ~ UR, data = auxi1)
    fit1 <- arrange(augment(modlss1), UR) %>% 
        mutate(.se = predict(modlss1, se = TRUE)$se.fit)
    modlss2 <- lm(CAB ~ UR, data = auxi2)
    fit2 <- arrange(augment(modlss2), UR) %>% 
        mutate(.se = predict(modlss2, se = TRUE)$se.fit)
    modlss3 <- lm(CAB ~ UR, data = auxi3)
    fit3 <- arrange(augment(modlss3), UR) %>% 
        mutate(.se = predict(modlss3, se = TRUE)$se.fit)
    hc <- hchart(
        auxi,
        type = "scatter",
        hcaes(x = UR, y = CAB),
        name = "UR vs CAB",
        showInLegend = FALSE
    ) %>%
        hc_plotOptions(
            series = list(
                dataLabels = list(
                    enabled = TRUE,
                    format="{point.Date}",
                    color="grey",
                    style = list(textOutline = FALSE)
                )
            )
        )%>%
        hc_tooltip(pointFormat = "CAB: {point.y:.2f} <br> UR: {point.x:.2f}")%>%
        hc_title(text="Current Account Balance Vs Unemployment Rate")%>%
        hc_add_series(
            fit1,
            type = "spline",
            hcaes(x=UR,y=.fitted),
            name = "Fit",
            id = "fit", # this is for link the arearange series to this one and have one legend
        )%>% 
        hc_add_series(
            fit2,
            type = "spline",
            hcaes(x=UR,y=.fitted),
            name = "Fit",
            id = "fit", # this is for link the arearange series to this one and have one legend
        )%>% 
        hc_add_series(
            fit3,
            type = "spline",
            hcaes(x=UR,y=.fitted),
            name = "Fit",
            id = "fit", # this is for link the arearange series to this one and have one legend
        )%>% 
        hc_add_theme(hc_theme_gridlight())
}

varpibur<-function(datos){
    auxi<-na.omit(left_join(filtra(datos,tparoID),filtra(datos,pibkID),by="date"))
    auxi<-auxi[,c(1,13,3)]
    colnames(auxi)<-c("Date","GDPg","UR")
    auxi<-auxi[auxi$Date>1999,]
    auxi1<-auxi[auxi$Date<2008,]
    auxi2<-auxi[auxi$Date>2007&auxi$Date<2014,]
    auxi3<-auxi[auxi$Date>2013,]
    modlss1 <- lm(GDPg ~ UR, data = auxi1)
    fit1 <- arrange(augment(modlss1), UR) %>% 
        mutate(.se = predict(modlss1, se = TRUE)$se.fit)
    modlss2 <- lm(GDPg ~ UR, data = auxi2)
    fit2 <- arrange(augment(modlss2), UR) %>% 
        mutate(.se = predict(modlss2, se = TRUE)$se.fit)
    modlss3 <- lm(GDPg ~ UR, data = auxi3)
    fit3 <- arrange(augment(modlss3), UR) %>% 
        mutate(.se = predict(modlss3, se = TRUE)$se.fit)
    hc <- hchart(
        auxi,
        type = "scatter",
        hcaes(x = UR, y = GDPg),
        name = "UR vs GDPg",
        showInLegend = FALSE
    ) %>%
        hc_plotOptions(
            series = list(
                dataLabels = list(
                    enabled = TRUE,
                    format="{point.Date}",
                    color="grey",
                    style = list(textOutline = FALSE)
                )
            )
        )%>%
        hc_tooltip(pointFormat = "GDPg: {point.y:.2f} <br> UR: {point.x:.2f}")%>%
        hc_title(text="GDP growth Vs Unemployment Rate")%>%
        hc_add_series(
            fit1,
            type = "spline",
            hcaes(x=UR,y=.fitted),
            name = "Fit",
            id = "fit", # this is for link the arearange series to this one and have one legend
        )%>% 
        hc_add_series(
            fit2,
            type = "spline",
            hcaes(x=UR,y=.fitted),
            name = "Fit",
            id = "fit", # this is for link the arearange series to this one and have one legend
        )%>% 
        hc_add_series(
            fit3,
            type = "spline",
            hcaes(x=UR,y=.fitted),
            name = "Fit",
            id = "fit", # this is for link the arearange series to this one and have one legend
        )%>% 
        hc_add_theme(hc_theme_gridlight())
}

deficit<-function(datos){
    govbal<-filtra(datos,defID)
    govbal
}

valor<-function(datos){
    datos[nrow(datos)-2,]
}

descpibkg<-function(datos){
    privcons<-filtra(datos,privconsID)
    pubcons<-filtra(datos,pubconsID)
    gcf<-filtra(datos,gcfID)
    exp<-filtra(datos,expID)
    imp<-filtra(datos,impID)
    aportaciones<-cbind(privcons[,c(1,3)],pubcons[,3],gcf[,3],exp[,3],imp[,3])
    colnames(aportaciones)<-c("Date","PrivFCE","PubFCE","GCF","EXP","IMP")
    aportaciones$growth<-aportaciones[,2]+aportaciones[,3]+aportaciones[,4]+aportaciones[,5]+aportaciones[,6]
    aportaciones<-aportaciones[aportaciones$Date>1999,]
    categories_column <- "Date"
    measure_columns <- c("PrivFCE","PubFCE","GCF","EXP","IMP","growth")
    base_highchart <- highchart() %>%
        hc_xAxis(categories = aportaciones[,categories_column],
                 title = categories_column) %>%
        hc_yAxis(title = list(text = "GDP growth"),plotLines = list(list(value=0,width=2,color = "black"))) %>%
        hc_add_series(name = measure_columns[1],
                      data = aportaciones[,measure_columns[1]],type="column") %>%
        hc_add_series(name = measure_columns[2],
                      data = aportaciones[,measure_columns[2]],type="column") %>%
        hc_add_series(name = measure_columns[3],
                      data = aportaciones[,measure_columns[3]],type="column") %>%
        hc_add_series(name = measure_columns[4],
                      data = aportaciones[,measure_columns[4]],type="column") %>%
        hc_add_series(name = measure_columns[5],
                      data = aportaciones[,measure_columns[5]],type="column")%>%
        hc_add_series(name = measure_columns[6],
                      data = aportaciones[,measure_columns[6]],type="spline")
    
    base_highchart %>% hc_title(text="GDP growth Decomposition")%>%hc_plotOptions(series = list(stacking = "normal"))%>%hc_add_theme(hc_theme_gridlight())
}

varpibplot<-function(datos){
    pibk<-filtra(datos,pibkID)
    pibk<-pibk[pibk[,1]>1999,]
    highchart()%>%
        hc_xAxis(categories = pibk[,1]) %>%
        hc_add_series(name = "GDP",data = pibk[,4]) %>%
        hc_title(text="GDP constant prices") %>%
        hc_subtitle(text="2015 = 100")%>%
        hc_add_theme(hc_theme_gridlight()) %>%
        hc_plotOptions(series = list(showInLegend = FALSE))
}

tsog<-function(datos){
    lserie<-log(filtra(datos,pibkID)[,4])
    inicio<-filtra(datos,pibkID)[1,1]
    lin.mod <- lm(lserie ~ time(lserie))
    lin.pot <- lin.mod$fitted.values
    og<-cbind(filtra(datos,pibkID)[,c(1,4)],exp(lin.pot))
    colnames(og)<-c("date","GDP","Potential")
    og$OG<-(1-og$Potential/og$GDP)*100
    return(og)
}

hpog<-function(datos,frecu){
    lserie<-log(filtra(datos,pibkID)[,4])
    inicio<-filtra(datos,pibkID)[1,1]
    hp.decom <- hpfilter(lserie, freq = frecu, type = "lambda")
    lin.pot <- hp.decom$trend
    og<-cbind(filtra(datos,pibkID)[,c(1,4)],exp(lin.pot))
    colnames(og)<-c("date","GDP","Potential")
    og$OG<-(1-og$Potential/og$GDP)*100
    return(og)
}

bkog<-function(datos){
    lserie<-log(filtra(datos,pibkID)[,4])
    inicio<-filtra(datos,pibkID)[1,1]
    bk.decom <- bkfilter(lserie,pl=24, pu=32)
    lin.pot <- bk.decom$trend
    og<-cbind(filtra(datos,pibkID)[,c(1,4)],exp(lin.pot))
    colnames(og)<-c("date","GDP","Potential")
    og$OG<-(1-og$Potential/og$GDP)*100
    return(og)
}

cfog<-function(datos){
    lserie<-log(filtra(datos,pibkID)[,4])
    inicio<-filtra(datos,pibkID)[1,1]
    cf.decom <- cffilter(lserie,pl=24, pu=32, root=TRUE, drift=FALSE)
    lin.pot <- cf.decom$trend
    og<-cbind(filtra(datos,pibkID)[,c(1,4)],exp(lin.pot))
    colnames(og)<-c("date","GDP","Potential")
    og$OG<-(1-og$Potential/og$GDP)*100
    return(og)
}

kalog<-function(datos){
    Lgdp<-log(filtra(datos,pibkID)[,4])

    mod1<-dlmModPoly(2)
    hp.dec<-hpfilter(Lgdp,freq=100,type="lambda",drif=FALSE)
    ciclo<-hp.dec$cycle
    tend<-hp.dec$trend
    estruc<-StructTS(tend)
    armodcicl<-arima(ciclo,order=c(2,0,0),include.mean = FALSE)
    arcoef<-armodcicl$coef[1:2]
    mod2<-dlmModARMA(ar=arcoef)
    mod<-mod1+mod2
    mod$V=0.00000001
    mod$W[2,2]<-estruc$coef[2]
    mod$W[3,3]<-armodcicl$sigma2
    serieFilt <- dlmFilter(Lgdp, mod)
    serieSmooth <- dlmSmooth(serieFilt)
    
    lin.pot <- serieSmooth$s[-1,1]
    og<-cbind(filtra(datos,pibkID)[,c(1,4)],exp(lin.pot))
    colnames(og)<-c("date","GDP","Potential")
    og$OG<-(1-og$Potential/og$GDP)*100
    return(og)
}

kaluni2<-function(serie){
    Lgdp<-serie
    mod1<-dlmModPoly(2)
    hp.dec<-hpfilter(Lgdp,freq=100,type="lambda",drif=FALSE)
    ciclo<-hp.dec$cycle
    tend<-hp.dec$trend
    estruc<-StructTS(tend)
    armodcicl<-arima(ciclo,order=c(2,0,0),include.mean = FALSE)
    arcoef<-armodcicl$coef[1:2]
    mod2<-dlmModARMA(ar=arcoef)
    mod<-mod1+mod2
    mod$V=0.00000001
    mod$W[2,2]<-estruc$coef[2]
    mod$W[3,3]<-armodcicl$sigma2
    serieFilt <- dlmFilter(Lgdp, mod)
    serieSmooth <- dlmSmooth(serieFilt)
    
    lin.pot <- serieSmooth$s[-1,1]
    og<-as.data.frame(cbind(serie,lin.pot))
    colnames(og)<-c("SerieOr","trend")
    og$cycle<-(1-og$trend/og$SerieOr)*100
    return(og)
}

kalurog<-function(datos){
    gdp<-filtra(datos,pibkID)
    paro<-filtra(datos,tparoID)
    seriedatos<-na.omit(left_join(gdp,paro,by="date"))
    Lgdp<-log(seriedatos[,4])
    Lparo<-log(seriedatos[,9])
    ldatos<-cbind(Lgdp,Lparo)
    hp1.dec<-hpfilter(Lgdp,freq=100,type="lambda",drift=FALSE)
    hp2.dec<-hpfilter(Lparo,freq=100,type="lambda",drift=FALSE)
    ciclo.gdp<-hp1.dec$cycle
    tend.gdp<-hp1.dec$trend
    ciclo.paro<-hp2.dec$cycle
    tend.paro<-hp2.dec$trend
    modreg<-lm(ciclo.paro~ciclo.gdp-1)
    regcof<-modreg$coefficients
    armodcicl<-arima(ciclo.gdp,order=c(2,0,0),include.mean = FALSE)
    arcoef<-armodcicl$coef[1:2]
    estruc.gdp<-StructTS(tend.gdp)
    estruc.paro<-StructTS(tend.paro)
    mymodel<-dlm(
        FF=as.matrix(rbind(c(1,0,1,0,0,0),c(0,0,regcof,0,1,0))),
        V=diag(c(0,var(modreg$residuals))),
        GG=as.matrix(rbind(c(1,1,0,0,0,0),c(0,1,0,0,0,0),c(0,0,arcoef[1],arcoef[2],0,0),c(0,0,1,0,0,0),c(0,0,0,0,1,1),c(0,0,0,0,0,1))),
        W=diag(c(0,estruc.gdp$coef[2],armodcicl$sigma2,0,0,estruc.paro$coef[2])),
        m0=rep(0,6),
        C0=100*diag(6))
    seriefilt<-dlmFilter(ldatos,mymodel)
    seriesuav<-dlmSmooth(seriefilt)
    
    lin.pot <- seriesuav$s[-1,1]
    og<-cbind(seriedatos[,c(1,4)],exp(lin.pot))
    colnames(og)<-c("date","GDP","Potential")
    og$OG<-(1-og$Potential/og$GDP)*100
    return(og)
}

kalurcabog<-function(datos){
    gdp<-filtra(datos,pibkID)
    paro<-filtra(datos,tparoID)
    cabo<-filtra(datos,cabID)
    seriedatos<-na.omit(left_join(gdp,paro,by="date"))
    seriedatos<-na.omit(left_join(seriedatos,cabo,by="date"))
    Lgdp<-log(seriedatos[,4])
    Lparo<-log(seriedatos[,9])
    Lcabo<-seriedatos[,15]
    ldatos<-cbind(Lgdp,Lparo,Lcabo)
    hp1.dec<-hpfilter(Lgdp,freq=100,type="lambda",drift=FALSE)
    hp2.dec<-hpfilter(Lparo,freq=100,type="lambda",drift=FALSE)
    hp3.dec<-hpfilter(Lcabo,freq=100,type="lambda",drift=FALSE)
    ciclo.gdp<-hp1.dec$cycle
    tend.gdp<-hp1.dec$trend
    ciclo.paro<-hp2.dec$cycle
    tend.paro<-hp2.dec$trend
    ciclo.cab<-hp3.dec$cycle
    tend.cab<-hp3.dec$trend
    modreg<-lm(ciclo.paro~ciclo.gdp-1)
    modreg2<-lm(ciclo.cab~ciclo.gdp-1)
    regcof<-modreg$coefficients
    regcof2<-modreg2$coefficients
    armodcicl<-arima(ciclo.gdp,order=c(2,0,0),include.mean = FALSE)
    arcoef<-armodcicl$coef[1:2]
    estruc.gdp<-StructTS(tend.gdp)
    estruc.paro<-StructTS(tend.paro)
    estruc.cab<-StructTS(tend.cab)
    mymodel<-dlm(
        FF=as.matrix(rbind(c(1,0,1,0,0,0,0,0),c(0,0,regcof,0,1,0,0,0),c(0,0,regcof2,0,0,0,1,0))),
        V=diag(c(0,var(modreg$residuals),var(modreg2$residuals))),
        GG=as.matrix(rbind(c(1,1,0,0,0,0,0,0),c(0,1,0,0,0,0,0,0),c(0,0,arcoef[1],arcoef[2],0,0,0,0),c(0,0,1,0,0,0,0,0),c(0,0,0,0,1,1,0,0),c(0,0,0,0,0,1,0,0),c(0,0,0,0,0,0,1,1),c(0,0,0,0,0,0,0,1))),
        W=diag(c(0,estruc.gdp$coef[2],armodcicl$sigma2,0,0,estruc.paro$coef[2],0,estruc.cab$coef[2])),
        m0=rep(0,8),
        C0=100*diag(8))
    seriefilt<-dlmFilter(ldatos,mymodel)
    seriesuav<-dlmSmooth(seriefilt)
    
    lin.pot <- seriesuav$s[-1,1]
    og<-cbind(seriedatos[,c(1,4)],exp(lin.pot))
    colnames(og)<-c("date","GDP","Potential")
    og$OG<-(1-og$Potential/og$GDP)*100
    return(og)
}

prodfunog<-function(datos,frecu){
    activos<-filtra(datos,labforID)
    pobl<-filtra(datos,popID)
    act<-na.omit(left_join(activos,pobl,by="date"))
    act$tact<-act[,3]/act[,9]*100
    tactfilt<-hpfilter(act$tact,freq=frecu,type="lambda",drift=FALSE)
    act$fullact<-act[,9]*tactfilt$trend/100
    tparo<-filtra(datos,tparoID)
    act<-na.omit(left_join(act,tparo,by="date"))
    tparofilt<-hpfilter(act[,17],freq=frecu,type="lambda",drift=FALSE)
    act$tparopot<-tparofilt$trend
    act$empot<-(100-act$tparopot)*act$fullact/100
    pibk<-filtra(datos,pibkID)
    act<-na.omit(left_join(act,pibk,by="date"))
    invk<-filtra(datos,gcfID)
    act<-na.omit(left_join(act,invk,by="date"))
    kk<-filtra(datos,ktID)
    act<-na.omit(left_join(act,kk,by="date"))
    delta=0.025
    act$kt<-act[,37]
    alfa<-0.35
    act$ocup<-(100-act[,17])*act[,3]/100
    TFP<-log(act[,26])-alfa*log(act$kt)-(1-alfa)*log(act$ocup)
    TFPfilt<-hpfilter(TFP,freq=frecu,type="lambda",drift=FALSE)
    act$TFPtend<-TFPfilt$trend
    ypot<-exp(act$TFPtend)*(act$kt^alfa)*(act$empot^(1-alfa))
    
    lin.pot <- log(ypot)
    og<-cbind(act[,c(1,26)],exp(lin.pot))
    colnames(og)<-c("date","GDP","Potential")
    og$OG<-(1-og$Potential/og$GDP)*100
    return(og)
}

prodfunkalog<-function(datos){
    activos<-filtra(datos,labforID)
    pobl<-filtra(datos,popID)
    act<-na.omit(left_join(activos,pobl,by="date"))
    act$tact<-act[,3]/act[,9]*100
    tactfilt<-kaluni2(act$tact)
    act$fullact<-act[,9]*tactfilt$trend/100
    tparo<-filtra(datos,tparoID)
    act<-na.omit(left_join(act,tparo,by="date"))
    tparofilt<-kaluni2(act[,17])
    act$tparopot<-tparofilt$trend
    act$empot<-(100-act$tparopot)*act$fullact/100
    pibk<-filtra(datos,pibkID)
    act<-na.omit(left_join(act,pibk,by="date"))
    invk<-filtra(datos,gcfID)
    act<-na.omit(left_join(act,invk,by="date"))
    kk<-filtra(datos,ktID)
    act<-na.omit(left_join(act,kk,by="date"))
    delta=0.025
    act$kt<-act[,37]
    alfa<-0.35
    act$ocup<-(100-act[,17])*act[,3]/100
    TFP<-log(act[,26])-alfa*log(act$kt)-(1-alfa)*log(act$ocup)
    TFPfilt<-kaluni2(TFP)
    act$TFPtend<-TFPfilt$trend
    ypot<-exp(act$TFPtend)*(act$kt^alfa)*(act$empot^(1-alfa))
    
    lin.pot <- log(ypot)
    og<-cbind(act[,c(1,26)],exp(lin.pot))
    colnames(og)<-c("date","GDP","Potential")
    og$OG<-(1-og$Potential/og$GDP)*100
    return(og)
}

cbounds<-function(datos){
    pais<-codigos[codigos$Country==datos$Country[1],"iso2c"]
    bounds<-codigos[codigos$iso2c==pais,13:18]
}

comisionog<-function(datos,frecu,anchor,custanchor,nawruadj,vuplbf,vupubf,vuslopelbf,vuslopeubf,vuglbf,vugubf){
    pais<-tolower(codigos[codigos$Country==datos$Country[1],"iso2c"])
    file="AMECO_2020_I.xlsx"
    tact<-read_excel(file,sheet="Part_rate",col_names = TRUE, col_types = NULL, skip = 1)
    tact<-tact[,colnames(tact)==pais]
    tact<-ts(tact,frequency=1,start=1965)
    
    tparo<-read_excel(file,sheet="UnEmpl_Rate",col_names = TRUE, col_types = NULL, skip = 1)
    tparo<-tparo[,colnames(tparo)==pais]
    tparo<-ts(tparo,frequency=1,start=1965)
    
    pop<-read_excel(file,sheet="Total Pop",col_names = TRUE, col_types = NULL, skip = 1)
    pop<-pop[,colnames(pop)==pais]
    pop<-ts(pop,frequency=1,start=1965)
    
    horas<-read_excel(file,sheet="TotHrs",col_names = TRUE, col_types = NULL, skip = 1)
    horas<-horas[,colnames(horas)==pais]
    horas<-ts(horas,frequency=1,start=1965)
    
    horasmed<-read_excel(file,sheet="HperE",col_names = TRUE, col_types = NULL, skip = 1)
    horasmed<-horasmed[,colnames(horasmed)==pais]
    horasmed<-ts(horasmed,frequency=1,start=1965)
    
    nawrucom<-read_excel(file,sheet="NAWRU",col_names = TRUE, col_types = NULL, skip = 1)
    nawrucom<-nawrucom[,colnames(nawrucom)==pais]
    nawrucom<-ts(nawrucom,frequency=1,start=1965)
    
    pop1574<-read_excel(file,sheet="POPA15-74",col_names = TRUE, col_types = NULL, skip = 1)
    pop1574<-pop1574[,colnames(pop1574)==pais]
    pop1574<-ts(pop1574,frequency=1,start=1965)
    
    lforce<-read_excel(file,sheet="LF",col_names = TRUE, col_types = NULL, skip = 1)
    lforce<-lforce[,colnames(lforce)==pais]
    lforce<-ts(lforce,frequency=1,start=1965)
    
    ocup<-read_excel(file,sheet="Empl",col_names = TRUE, col_types = NULL, skip = 1)
    ocup<-ocup[,colnames(ocup)==pais]
    ocup<-ts(ocup,frequency=1,start=1965)
    
    inv<-read_excel(file,sheet="Investment",col_names = TRUE, col_types = NULL, skip = 1)
    inv<-inv[,colnames(inv)==pais]
    inv<-ts(inv,frequency=1,start=1965)
    
    k<-read_excel(file,sheet="Capital",col_names = TRUE, col_types = NULL, skip = 1)
    k<-k[,colnames(k)==pais]
    k<-ts(k,frequency=1,start=1965)
    
    gdpdefl<-read_excel(file,sheet="GDPdefl",col_names = TRUE, col_types = NULL, skip = 1)
    gdpdefl<-gdpdefl[,colnames(gdpdefl)==pais]
    gdpdefl<-ts(gdpdefl,frequency=1,start=1965)
    
    gdp<-read_excel(file,sheet="Real GDP",col_names = TRUE, col_types = NULL, skip = 1)
    gdp<-gdp[,colnames(gdp)==pais]
    gdp<-ts(gdp,frequency=1,start=1965)
    y<-gdp
    
    nawrudat<-filtra(datos,tparoID)
    nawrudat<-na.omit(left_join(nawrudat,filtra(datos,compempID),by="date"))
    nawrudat<-na.omit(left_join(nawrudat,filtra(datos,prodwork),by="date"))
    nawrudat<-na.omit(left_join(nawrudat,filtra(datos,inflID),by="date"))
    nawrudat$dws<-nawrudat[,13]/100-nawrudat[,18]-(nawrudat[,23]-1)
    nawrudat<-ts(nawrudat,frequency=1,start=nawrudat[1,1])
    nawrudat<-nawrudat[,c(3,26)]
    colnames(nawrudat)<-c("LUR","DWS")
    
    outgcom<-read_excel(file,sheet="OutputGapPf",col_names = TRUE, col_types = NULL, skip = 1)
    outgcom<-outgcom[,colnames(outgcom)==pais]
    outgcom<-ts(outgcom,frequency=1,start=1965)
    
    tact.des<-hpfilter(tact, freq = frecu, type = "lambda")
    tactpot<-tact.des$trend
    
    #BUSCANDO VALORES INICIALES
    hp.decom <- hpfilter(nawrudat[,1], freq = frecu, type = "lambda")
    hpy <- hpfilter(nawrudat[,2], freq = frecu, type = "lambda")
    ols.fit<-lm(hpy$cycle~hp.decom$cycle)
    ar.fit<-arima(hp.decom$cycle,c(2,0,0))
    ar.resid<-arima(ols.fit$residuals,c(1,0,0))
    buildLocalLevel <- function(psi) dlmModPoly(order = 2, dV = exp(psi[1]), dW = c(exp(psi[2]),exp(psi[3])))
    mleOut <- dlmMLE(hp.decom$trend, parm = c(0,0,0), build = buildLocalLevel, lower = c(-18,-18,-18))
    init<-c(ols.fit$coeff[2],ar.fit$coef[1],ar.fit$coef[2],ar.resid$coef[1],log(var(ols.fit$residuals)),mleOut$par[2],mleOut$par[3],log(ar.fit$sigma2))
    #CALCULAMOS NAWRU
    par<-init
    Z=matrix(c(0,1,1,0,0,0,1,0,0,0,0,par[1],0,par[1]*(par[4]-0.99)/(par[4]*0.99-1)*par[3],0,1),2,8)
    T=matrix(c(1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,par[2],1,par[2],0,0,0,0,0,par[3],0,par[3],0,0,0,0,0,0,0,par[4],1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,par[4]),8,8)
    H=diag(c(0,exp(par[5])))
    Q=diag(c(exp(par[5]),exp(par[6]),exp(par[7]),exp(par[8])))
    a0<-rep(0,8)
    P0<-100*diag(8)
    P1inf=diag(8)
    R<-matrix(c(0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0),8,4)
    mod_com_kfs<-SSModel(nawrudat~-1+SSMcustom(Z=Z,T=T,Q=Q,R=R,P1inf=P1inf,a1=a0,P1=P0),H=H)
    update_model<-function(par,model){
        Z=matrix(c(0,1,1,0,0,0,1,0,0,0,0,par[1],0,par[1]*(par[4]-0.99)/(par[4]*0.99-1)*par[3],0,1),2,8)
        T=matrix(c(1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,par[2],1,par[2],0,0,0,0,0,par[3],0,par[3],0,0,0,0,0,0,0,par[4],1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,par[4]),8,8)
        H=diag(c(0,exp(par[5])))
        Q=diag(c(exp(par[5]),exp(par[6]),exp(par[7]),exp(par[8])))
        a0<-rep(0,8)
        P0<-100*diag(8)
        R<-matrix(c(0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0),8,4)
        tmp<-SSModel(nawrudat~-1+SSMcustom(Z=Z,T=T,Q=Q,R=R,a1=a0,P1=P0),H=H)
        model["Z"]<-tmp$Z
        model["T"]<-tmp$T
        model["Q"]<-tmp$Q
        model["a1"]<tmp$a1
        model["P1"]<-tmp$P1
        model["H"]<-tmp$H
        model
    }
    #fit_mod_com_kfs<-fitSSM(mod_com_kfs, init, updatefn = update_model, method = "L-BFGS-B", lower = c(-0.005,-1.96,-0.97,0,log(0.00000001),log(0.00000001),log(0.14),log(0.00000001)), upper = c(-0.003,1.96,0.97,0.97,log(0.00045),log(0.2),log(0.4),log(1.05)))
    #init<-c(beta0,phiG1,phiG2,phiw1,Vw,VuP,Vuslope,Vug)
    fit_mod_com_kfs<-fitSSM(mod_com_kfs, init, updatefn = update_model, method = "L-BFGS-B", lower = c(-2,-1.96,-0.97,0,log(0.00000001),log(vuplbf),log(vuslopelbf),log(vuglbf)), upper = c(0,1.96,0.97,0.97,log(4.18561966638578E-04),log(vupubf),log(vuslopeubf),log(vugubf)))
    out_mod_com_kfs<-KFS(fit_mod_com_kfs$model)
    NAWRU<-out_mod_com_kfs$alphahat[,2]
    #ANCLA
    if(anchor==1){ancla<-codigos[codigos$Country==datos$Country[1],"Anchor"]}else if(anchor==0){ancla<-custanchor}
    if(anchor==2){NAWRU_anch<-NAWRU} else{
    modelo<-fit_mod_com_kfs$model
    filtro<-out_mod_com_kfs
    h=10
    s<-matrix(c(0,1,0,0,0,0,0,0),1,8)
    W<-modelo$R[,,1]%*%modelo$Q[,,1]%*%t(modelo$R[,,1])
    den<-function(modelo,h){
        auxi1<-0
        for(i in 1:h){
            auxi1<-auxi1+modelo$T[,,1]^(i-1)%*%W%*%t(modelo$T[,,1]^(i-1))
        }
        auxi2<-auxi1+modelo$T[,,1]^(10)%*%filtro$V[,,dim(filtro$V)[3]]%*%t(modelo$T[,,1]^(10))
        valor<-s%*%auxi2%*%t(s)
        return(valor)
    }
    den(modelo,h)
    num<-0
    for(i in 1:dim(nawrudat)[1]){
        num[i]<-s%*%filtro$V[,,i]%*%t(modelo$T[,,1]^(10))%*%t(s)
    }
    NAWRU_anch<-NAWRU+num/den(modelo,h)*(ancla-s%*%modelo$T[,,1]^10%*%t(s)*NAWRU[length(NAWRU)])
    }
    #AJUSTE
    ajuste<-codigos[codigos$Country==datos$Country[1],"AdjFactor"]
    if(nawruadj==1){NAWRU_fin<-NAWRU_anch+ajuste}else{NAWRU_fin<-NAWRU_anch}
    #HORAS
    horasmed.des<-hpfilter(horasmed, freq = frecu, type = "lambda")
    horasmedpot<-horasmed.des$trend
    horaspot=(pop1574*tactpot*(1-NAWRU_fin/100))*horasmedpot/100
    #TFP
    alfa<-0.35
    TFP<-log(y)-(alfa)*log(k)-(1-alfa)*log(horas)
    load(file="cubs.Rda")
    colnames(cubs)[29]<-"GB"
    colcubs<-codigos[codigos$Country==datos$Country[1],"iso2c"]
    cubsdat<-na.omit(cubs[,c("year",colcubs)])
    cu<-ts(cubsdat[,2],frequency=1,start=cubsdat[1,1])
    datosTFP<-cbind(TFP,cu)
    datosTFP2<-na.omit(datosTFP)
    hp.decom <- hpfilter(datosTFP2[,1], freq = frecu, type = "lambda")
    ols.fit<-lm(datosTFP2[,2]~hp.decom$cycle)
    ar.fit<-arima(hp.decom$cycle,c(2,0,0))
    ar.resid<-arima(ols.fit$residuals,c(1,0,0))
    mleOut <- StructTS(hp.decom$trend)
    init<-c(ols.fit$coeff[2],ar.resid$coef[1],ar.fit$coef[1],ar.fit$coef[2],log(var(ols.fit$residuals)),log(mleOut$coef[2]),log(ar.fit$sigma2))
    par<-init
    Z=matrix(c(0,1,1,0,0,0,1,0,0,0,0,par[1]),2,6)
    T=matrix(c(1,0,0,0,0,0,0,1,0,0,0,0,0,1,par[2],0,0,0,0,0,0,par[3],1,par[3],0,0,0,par[4],0,par[4],0,0,0,0,0,0),6,6)
    H=diag(c(0,exp(par[5])))
    Q=diag(c(exp(par[6]),exp(par[7]),exp(par[7])))
    a0<-rep(0,6)
    P0<-100*diag(6)
    P1inf=diag(6)
    R=matrix(c(0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1),6,3)
    mod_com_kfs<-SSModel(datosTFP~-1+SSMcustom(Z=Z,T=T,Q=Q,R=R, P1inf=P1inf,a1=a0,P1=P0),H=H)
    update_model<-function(par,model){
        Z=matrix(c(0,1,1,0,0,0,1,0,0,0,0,par[1]),2,6)
        T=matrix(c(1,0,0,0,0,0,0,1,0,0,0,0,0,1,par[2],0,0,0,0,0,0,par[3],1,par[3],0,0,0,par[4],0,par[4],0,0,0,0,0,0),6,6)
        H=diag(c(0,exp(par[5])))
        Q=diag(c(exp(par[6]),exp(par[7]),exp(par[7])))
        a0<-rep(0,6)
        P0<-100*diag(6)
        P1inf=diag(6)
        R=matrix(c(0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1),6,3)
        tmp<-SSModel(datosTFP~-1+SSMcustom(Z=Z,T=T,Q=Q,R=R,a1=a0,P1=P0),H=H)
        model["Z"]<-tmp$Z
        model["T"]<-tmp$T
        model["Q"]<-tmp$Q
        model["a1"]<tmp$a1
        model["P1"]<-tmp$P1
        model["H"]<-tmp$H
        model
    }
    #init<-c(beta0,phiG1,phiG2,phiw1,Vw,VuP,Vuslope,Vug)
    fit_mod_com_kfs<-fitSSM(mod_com_kfs, init, updatefn = update_model, method = "L-BFGS-B", lower = c(0,-1.96,-1.96,-0.97,log(0.00000001),log(0.00000001),log(0.00000001)), upper = c(4,1.96,1.96,0.97,log(0.000418),log(0.000005),log(0.00005)))
    out_mod_com_kfs<-KFS(fit_mod_com_kfs$model)
    TFPpot<-out_mod_com_kfs$alphahat[,2]
    
    ypot=(horaspot^(1-alfa))*(k^(alfa))*exp(TFPpot)
    ygap=(y/ypot-1)*100

    lin.pot <- ypot
    og<-as.data.frame(na.omit(cbind(y,lin.pot,NAWRU_fin,TFPpot,tparo,TFP,tact,tactpot,horasmed,horasmedpot)))
    og$date<-seq(start(ypot)[1],end(ypot)[1],1)
    og<-og[,c(ncol(og),1:(ncol(og)-1))]
    colnames(og)<-c("date","GDP","Potential","NAWRU","TFPpot","UR","TFP","PartRate","PotPartRate","AvHour","PotAvHour")
    og$OG<-(og$GDP/og$Potential-1)*100
    og<-og[og$date>1999,]
    og$date<-as.factor(og$date)
    return(og)
}

alternative<-function(datos){
    frecu=100
    bounds<-cbounds(datos)[1,]
    vuplbf=as.numeric(bounds[1])
    vupubf=as.numeric(bounds[2])
    vuslopelbf=as.numeric(bounds[3])
    vuslopeubf=as.numeric(bounds[4])
    vuglbf=as.numeric(bounds[5])
    vugubf=as.numeric(bounds[6])
    pais<-tolower(codigos[codigos$Country==datos$Country[1],"iso2c"])
    file="AMECO_2020_I.xlsx"
    tact<-read_excel(file,sheet="Part_rate",col_names = TRUE, col_types = NULL, skip = 1)
    tact<-tact[,colnames(tact)==pais]
    tact<-ts(tact,frequency=1,start=1965)
    
    tparo<-read_excel(file,sheet="UnEmpl_Rate",col_names = TRUE, col_types = NULL, skip = 1)
    tparo<-tparo[,colnames(tparo)==pais]
    tparo<-ts(tparo,frequency=1,start=1965)
    
    pop<-read_excel(file,sheet="Total Pop",col_names = TRUE, col_types = NULL, skip = 1)
    pop<-pop[,colnames(pop)==pais]
    pop<-ts(pop,frequency=1,start=1965)
    
    horas<-read_excel(file,sheet="TotHrs",col_names = TRUE, col_types = NULL, skip = 1)
    horas<-horas[,colnames(horas)==pais]
    horas<-ts(horas,frequency=1,start=1965)
    
    horasmed<-read_excel(file,sheet="HperE",col_names = TRUE, col_types = NULL, skip = 1)
    horasmed<-horasmed[,colnames(horasmed)==pais]
    horasmed<-ts(horasmed,frequency=1,start=1965)
    
    nawrucom<-read_excel(file,sheet="NAWRU",col_names = TRUE, col_types = NULL, skip = 1)
    nawrucom<-nawrucom[,colnames(nawrucom)==pais]
    nawrucom<-ts(nawrucom,frequency=1,start=1965)
    
    pop1574<-read_excel(file,sheet="POPA15-74",col_names = TRUE, col_types = NULL, skip = 1)
    pop1574<-pop1574[,colnames(pop1574)==pais]
    pop1574<-ts(pop1574,frequency=1,start=1965)
    
    lforce<-read_excel(file,sheet="LF",col_names = TRUE, col_types = NULL, skip = 1)
    lforce<-lforce[,colnames(lforce)==pais]
    lforce<-ts(lforce,frequency=1,start=1965)
    
    ocup<-read_excel(file,sheet="Empl",col_names = TRUE, col_types = NULL, skip = 1)
    ocup<-ocup[,colnames(ocup)==pais]
    ocup<-ts(ocup,frequency=1,start=1965)
    
    inv<-read_excel(file,sheet="Investment",col_names = TRUE, col_types = NULL, skip = 1)
    inv<-inv[,colnames(inv)==pais]
    inv<-ts(inv,frequency=1,start=1965)
    
    k<-read_excel(file,sheet="Capital",col_names = TRUE, col_types = NULL, skip = 1)
    k<-k[,colnames(k)==pais]
    k<-ts(k,frequency=1,start=1965)
    
    gdpdefl<-read_excel(file,sheet="GDPdefl",col_names = TRUE, col_types = NULL, skip = 1)
    gdpdefl<-gdpdefl[,colnames(gdpdefl)==pais]
    gdpdefl<-ts(gdpdefl,frequency=1,start=1965)
    
    gdp<-read_excel(file,sheet="Real GDP",col_names = TRUE, col_types = NULL, skip = 1)
    gdp<-gdp[,colnames(gdp)==pais]
    gdp<-ts(gdp,frequency=1,start=1965)
    y<-gdp
    
    nawrudat<-filtra(datos,tparoID)
    nawrudat<-na.omit(left_join(nawrudat,filtra(datos,compempID),by="date"))
    nawrudat<-na.omit(left_join(nawrudat,filtra(datos,prodwork),by="date"))
    nawrudat<-na.omit(left_join(nawrudat,filtra(datos,inflID),by="date"))
    nawrudat$dws<-nawrudat[,13]/100-nawrudat[,18]-(nawrudat[,23]-1)
    nawrudat<-ts(nawrudat,frequency=1,start=nawrudat[1,1])
    nawrudat<-nawrudat[,c(3,26)]
    colnames(nawrudat)<-c("LUR","DWS")
    
    outgcom<-read_excel(file,sheet="OutputGapPf",col_names = TRUE, col_types = NULL, skip = 1)
    outgcom<-outgcom[,colnames(outgcom)==pais]
    outgcom<-ts(outgcom,frequency=1,start=1965)
    
    tact.des<-hpfilter(tact, freq = frecu, type = "lambda")
    tactpot<-tact.des$trend
    
    #BUSCANDO VALORES INICIALES
    hp.decom <- hpfilter(nawrudat[,1], freq = frecu, type = "lambda")
    hpy <- hpfilter(nawrudat[,2], freq = frecu, type = "lambda")
    ols.fit<-lm(hpy$cycle~hp.decom$cycle)
    ar.fit<-arima(hp.decom$cycle,c(2,0,0))
    ar.resid<-arima(ols.fit$residuals,c(1,0,0))
    buildLocalLevel <- function(psi) dlmModPoly(order = 2, dV = exp(psi[1]), dW = c(exp(psi[2]),exp(psi[3])))
    mleOut <- dlmMLE(hp.decom$trend, parm = c(0,0,0), build = buildLocalLevel, lower = c(-18,-18,-18))
    init<-c(ols.fit$coeff[2],ar.fit$coef[1],ar.fit$coef[2],ar.resid$coef[1],log(var(ols.fit$residuals)),mleOut$par[2],mleOut$par[3],log(ar.fit$sigma2))
    #CALCULAMOS NAWRU
    par<-init
    Z=matrix(c(0,1,1,0,0,0,1,0,0,0,0,par[1],0,par[1]*(par[4]-0.99)/(par[4]*0.99-1)*par[3],0,1),2,8)
    T=matrix(c(1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,par[2],1,par[2],0,0,0,0,0,par[3],0,par[3],0,0,0,0,0,0,0,par[4],1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,par[4]),8,8)
    H=diag(c(0,exp(par[5])))
    Q=diag(c(exp(par[5]),exp(par[6]),exp(par[7]),exp(par[8])))
    a0<-rep(0,8)
    P0<-100*diag(8)
    P1inf=diag(8)
    R<-matrix(c(0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0),8,4)
    mod_com_kfs<-SSModel(nawrudat~-1+SSMcustom(Z=Z,T=T,Q=Q,R=R,P1inf=P1inf,a1=a0,P1=P0),H=H)
    update_model<-function(par,model){
        Z=matrix(c(0,1,1,0,0,0,1,0,0,0,0,par[1],0,par[1]*(par[4]-0.99)/(par[4]*0.99-1)*par[3],0,1),2,8)
        T=matrix(c(1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,par[2],1,par[2],0,0,0,0,0,par[3],0,par[3],0,0,0,0,0,0,0,par[4],1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,par[4]),8,8)
        H=diag(c(0,exp(par[5])))
        Q=diag(c(exp(par[5]),exp(par[6]),exp(par[7]),exp(par[8])))
        a0<-rep(0,8)
        P0<-100*diag(8)
        R<-matrix(c(0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0),8,4)
        tmp<-SSModel(nawrudat~-1+SSMcustom(Z=Z,T=T,Q=Q,R=R,a1=a0,P1=P0),H=H)
        model["Z"]<-tmp$Z
        model["T"]<-tmp$T
        model["Q"]<-tmp$Q
        model["a1"]<tmp$a1
        model["P1"]<-tmp$P1
        model["H"]<-tmp$H
        model
    }
    #fit_mod_com_kfs<-fitSSM(mod_com_kfs, init, updatefn = update_model, method = "L-BFGS-B", lower = c(-0.005,-1.96,-0.97,0,log(0.00000001),log(0.00000001),log(0.14),log(0.00000001)), upper = c(-0.003,1.96,0.97,0.97,log(0.00045),log(0.2),log(0.4),log(1.05)))
    #init<-c(beta0,phiG1,phiG2,phiw1,Vw,VuP,Vuslope,Vug)
    fit_mod_com_kfs<-fitSSM(mod_com_kfs, init, updatefn = update_model, method = "L-BFGS-B", lower = c(-2,-1.96,-0.97,0,log(0.00000001),log(vuplbf),log(vuslopelbf),log(vuglbf)), upper = c(0,1.96,0.97,0.97,log(4.18561966638578E-04),log(vupubf),log(vuslopeubf),log(vugubf)))
    out_mod_com_kfs<-KFS(fit_mod_com_kfs$model)
    NAWRU<-out_mod_com_kfs$alphahat[,2]
    NAWRU_fin<-NAWRU
    #HORAS
    ocupal=(pop1574*tact*(1-tparo/100))/100
    ocupot=(pop1574*tactpot*(1-NAWRU_fin/100))/100
    #TFP
    alfa<-0.35
    TFP<-log(y)-(alfa)*log(k)-(1-alfa)*log(ocupal)
    load(file="cubs.Rda")
    colnames(cubs)[29]<-"GB"
    colcubs<-codigos[codigos$Country==datos$Country[1],"iso2c"]
    cubsdat<-na.omit(cubs[,c("year",colcubs)])
    cu<-ts(cubsdat[,2],frequency=1,start=cubsdat[1,1])
    datosTFP<-cbind(TFP,cu)
    datosTFP2<-na.omit(datosTFP)
    hp.decom <- hpfilter(datosTFP2[,1], freq = frecu, type = "lambda")
    ols.fit<-lm(datosTFP2[,2]~hp.decom$cycle)
    ar.fit<-arima(hp.decom$cycle,c(2,0,0))
    ar.resid<-arima(ols.fit$residuals,c(1,0,0))
    mleOut <- StructTS(hp.decom$trend)
    init<-c(ols.fit$coeff[2],ar.resid$coef[1],ar.fit$coef[1],ar.fit$coef[2],log(var(ols.fit$residuals)),log(mleOut$coef[2]),log(ar.fit$sigma2))
    par<-init
    Z=matrix(c(0,1,1,0,0,0,1,0,0,0,0,par[1]),2,6)
    T=matrix(c(1,0,0,0,0,0,0,1,0,0,0,0,0,1,par[2],0,0,0,0,0,0,par[3],1,par[3],0,0,0,par[4],0,par[4],0,0,0,0,0,0),6,6)
    H=diag(c(0,exp(par[5])))
    Q=diag(c(exp(par[6]),exp(par[7]),exp(par[7])))
    a0<-rep(0,6)
    P0<-100*diag(6)
    P1inf=diag(6)
    R=matrix(c(0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1),6,3)
    mod_com_kfs<-SSModel(datosTFP~-1+SSMcustom(Z=Z,T=T,Q=Q,R=R, P1inf=P1inf,a1=a0,P1=P0),H=H)
    update_model<-function(par,model){
        Z=matrix(c(0,1,1,0,0,0,1,0,0,0,0,par[1]),2,6)
        T=matrix(c(1,0,0,0,0,0,0,1,0,0,0,0,0,1,par[2],0,0,0,0,0,0,par[3],1,par[3],0,0,0,par[4],0,par[4],0,0,0,0,0,0),6,6)
        H=diag(c(0,exp(par[5])))
        Q=diag(c(exp(par[6]),exp(par[7]),exp(par[7])))
        a0<-rep(0,6)
        P0<-100*diag(6)
        P1inf=diag(6)
        R=matrix(c(0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1),6,3)
        tmp<-SSModel(datosTFP~-1+SSMcustom(Z=Z,T=T,Q=Q,R=R,a1=a0,P1=P0),H=H)
        model["Z"]<-tmp$Z
        model["T"]<-tmp$T
        model["Q"]<-tmp$Q
        model["a1"]<tmp$a1
        model["P1"]<-tmp$P1
        model["H"]<-tmp$H
        model
    }
    #init<-c(beta0,phiG1,phiG2,phiw1,Vw,VuP,Vuslope,Vug)
    fit_mod_com_kfs<-fitSSM(mod_com_kfs, init, updatefn = update_model, method = "L-BFGS-B", lower = c(0,-1.96,-1.96,-0.97,log(0.00000001),log(0.00000001),log(0.00000001)), upper = c(4,1.96,1.96,0.97,log(0.000418),log(0.000005),log(0.00005)))
    out_mod_com_kfs<-KFS(fit_mod_com_kfs$model)
    TFPpot<-out_mod_com_kfs$alphahat[,2]
    
    ypot=(ocupot^(1-alfa))*(k^(alfa))*exp(TFPpot)
    ygap=(y/ypot-1)*100
    
    lin.pot <- ypot
    og<-as.data.frame(na.omit(cbind(y,lin.pot,NAWRU_fin,TFPpot,tparo,TFP,tact,tactpot,ocupal,ocupot)))
    og$date<-seq(start(ypot)[1],end(ypot)[1],1)
    og<-og[,c(ncol(og),1:(ncol(og)-1))]
    colnames(og)<-c("date","GDP","Potential","NAWRU","TFPpot","UR","TFP","PartRate","PotPartRate","Occup","PotOccup")
    og$OG<-(og$GDP/og$Potential-1)*100
    og<-og[og$date>1999,]
    og$date<-as.factor(og$date)
    return(og)  
}

nawruplot<-function(ogserie,datos){
    ogcom<-na.omit(left_join(ogserie,filtra(datos,nawID),by="date"))
    pais<-tolower(codigos[codigos$Country==datos$Country[1],"iso2c"])
    file="AMECO_2019_I.xlsx"
    outgcom<-read_excel(file,sheet="NAWRU",col_names = TRUE, col_types = NULL, skip = 1)
    outgcom<-outgcom[,colnames(outgcom)==pais]
    outgcom$date<-as.factor(seq(1965,1965+nrow(outgcom)-1,1))
    ogcom<-na.omit(left_join(ogcom,outgcom,by="date"))
    ogcom<-ogcom[ogcom[,1]>1999,]
    highchart()%>%
        hc_xAxis(categories = ogcom[,1]) %>%
        hc_add_series(name = "NAWRU",data = ogcom[,"NAWRU"],type="line",marker = list(enabled=FALSE)) %>%
        hc_add_series(name = "NAWRUCOM",data = ogcom[,"value"],type="line",marker = list(enabled=FALSE),color="red", dashStyle = "DashDot") %>%
        hc_add_series(name = "NAWRU Last Year",data = ogcom[,ncol(ogcom)],type="line",marker = list(enabled=FALSE),color="blue", dashStyle = "DashDot") %>%
        hc_title(text="NAWRU") %>%
        hc_subtitle(text="Potential Unemployment Rate")%>%
        hc_yAxis(plotLines=list(
            list(
                color="black",
                width=2,
                value=0
            )
        ))%>%
        hc_add_theme(hc_theme_gridlight()) %>% hc_exporting(enabled = TRUE)%>%
        hc_plotOptions(series = list(showInLegend = TRUE))
}

descog<-function(ogserie){
    alfa<-0.35
    ogserie$TFPog<-(exp(ogserie$TFP)/exp(ogserie$TFPpot)-1)*100
    ogserie$tactog<-(ogserie$PartRate/ogserie$PotPartRate-1)*100
    ogserie$tocupog<-((1-ogserie$UR/100)/(1-ogserie$NAWRU/100)-1)*100
    ogserie$hormedog<-(ogserie$AvHour/ogserie$PotAvHour-1)*100
    ogserie$yogsum<-ogserie$TFPog+(1-alfa)*(ogserie$tactog+ogserie$tocupog+ogserie$hormedog)
    aportaciones<-ogserie[,c("date","TFPog","tactog","tocupog","hormedog")]
    colnames(aportaciones)<-c("Date","TFP","PartRate","OccupRate","AvHours")
    aportaciones$growth<-aportaciones[,2]+(1-alfa)*(aportaciones[,3]+aportaciones[,4]+aportaciones[,5])
    categories_column <- "Date"
    measure_columns <- c("TFP","PartRate","OccupRate","AvHours","growth")
    aportaciones[,3:5]<-aportaciones[,3:5]*(1-alfa)
    base_highchart <- highchart() %>%
        hc_xAxis(categories = aportaciones[,categories_column],
                 title = categories_column) %>%
        hc_yAxis(title = list(text = "OG"),plotLines = list(list(value=0,width=2,color = "black"))) %>%
        hc_add_series(name = measure_columns[1],
                      data = aportaciones[,measure_columns[1]],type="column") %>%
        hc_add_series(name = measure_columns[2],
                      data = aportaciones[,measure_columns[2]],type="column") %>%
        hc_add_series(name = measure_columns[3],
                      data = aportaciones[,measure_columns[3]],type="column") %>%
        hc_add_series(name = measure_columns[4],
                      data = aportaciones[,measure_columns[4]],type="column") %>%
        hc_add_series(name = measure_columns[5],
                      data = aportaciones[,measure_columns[5]],type="spline")
    
    base_highchart %>% hc_title(text="OUPUT GAP Decomposition")%>%hc_plotOptions(series = list(stacking = "normal"))%>%hc_add_theme(hc_theme_gridlight())
}

descogal<-function(ogserie){
    alfa<-0.35
    ogserie$TFPog<-(exp(ogserie$TFP)/exp(ogserie$TFPpot)-1)*100
    ogserie$tactog<-(ogserie$PartRate/ogserie$PotPartRate-1)*100
    ogserie$tocupog<-((1-ogserie$UR/100)/(1-ogserie$NAWRU/100)-1)*100
    ogserie$yogsum<-ogserie$TFPog+(1-alfa)*(ogserie$tactog+ogserie$tocupog)
    aportaciones<-ogserie[,c("date","TFPog","tactog","tocupog")]
    colnames(aportaciones)<-c("Date","TFP","PartRate","OccupRate")
    aportaciones$growth<-aportaciones[,2]+(1-alfa)*(aportaciones[,3]+aportaciones[,4])
    categories_column <- "Date"
    measure_columns <- c("TFP","PartRate","OccupRate","growth")
    aportaciones[,3:4]<-aportaciones[,3:4]*(1-alfa)
    base_highchart <- highchart() %>%
        hc_xAxis(categories = aportaciones[,categories_column],
                 title = categories_column) %>%
        hc_yAxis(title = list(text = "OG"),plotLines = list(list(value=0,width=2,color = "black"))) %>%
        hc_add_series(name = measure_columns[1],
                      data = aportaciones[,measure_columns[1]],type="column") %>%
        hc_add_series(name = measure_columns[2],
                      data = aportaciones[,measure_columns[2]],type="column") %>%
        hc_add_series(name = measure_columns[3],
                      data = aportaciones[,measure_columns[3]],type="column") %>%
        hc_add_series(name = measure_columns[4],
                      data = aportaciones[,measure_columns[4]],type="spline")
    
    base_highchart %>% hc_title(text="OUPUT GAP Decomposition")%>%hc_plotOptions(series = list(stacking = "normal"))%>%hc_add_theme(hc_theme_gridlight())
}

potplot<-function(ogserie){
    if(class(ogserie[,1])!="factor"){ogserie<-ogserie[ogserie[,1]>1999,]}
    highchart()%>%
        hc_xAxis(categories = ogserie[,1]) %>%
        hc_add_series(name = "GDP",data = ogserie[,"GDP"],type="line",marker = list(enabled=FALSE)) %>%
        hc_add_series(name = "PotGDP",data = ogserie[,"Potential"],type="line",marker = list(enabled=FALSE)) %>%
        hc_title(text="GDP and Potential GDP") %>%
        hc_subtitle(text="2010 = 100")%>%
        hc_add_theme(hc_theme_gridlight()) %>%
        hc_plotOptions(series = list(showInLegend = TRUE))
}

ogplot<-function(ogserie,datos){
    ogcom<-na.omit(left_join(ogserie,filtra(datos,ogcID),by="date"))
    pais<-tolower(codigos[codigos$Country==datos$Country[1],"iso2c"])
    file="AMECO_2019_I.xlsx"
    outgcom<-read_excel(file,sheet="OutputGapPf",col_names = TRUE, col_types = NULL, skip = 1)
    outgcom<-outgcom[,colnames(outgcom)==pais]
    outgcom$date<-as.factor(seq(1965,1965+nrow(outgcom)-1,1))
    ogcom<-na.omit(left_join(ogcom,outgcom,by="date"))
    ogcom<-ogcom[ogcom[,1]>1999,]
    highchart()%>%
        hc_xAxis(categories = ogcom[,1]) %>%
        hc_add_series(name = "OG",data = ogcom[,"OG"],type="line",marker = list(enabled=FALSE)) %>%
        hc_add_series(name = "OGCOM",data = ogcom[,"value"],type="line",marker = list(enabled=FALSE),color="red", dashStyle = "DashDot") %>%
        hc_add_series(name = "OG Last Year",data = ogcom[,ncol(ogcom)],type="line",marker = list(enabled=FALSE),color="blue", dashStyle = "DashDot") %>%
        hc_title(text="OuputGap") %>%
        hc_subtitle(text="GDP %")%>%
        hc_yAxis(plotLines=list(
            list(
                color="black",
                width=2,
                value=0
            )
        ))%>%
        hc_add_theme(hc_theme_gridlight()) %>% hc_exporting(enabled = TRUE)%>%
        hc_plotOptions(series = list(showInLegend = TRUE))
}

remixplot<-function(ogseriecom,ogseriekfprod,ogseriehp,ogseriebik,ogserieal,datos){
    ogcom<-ogseriecom[,c("date","OG")]
    ogcom<-na.omit(left_join(ogcom,filtra(datos,ogcID),by="date"))
    ogcom<-ogcom[,c(1,2,4)]
    ogcom<-na.omit(left_join(ogcom,ogseriekfprod,by="date"))
    ogcom<-ogcom[,c(1,2,3,6)]
    ogcom<-na.omit(left_join(ogcom,ogseriehp,by="date"))
    ogcom<-ogcom[,c(1,2,3,4,7)]
    ogcom<-na.omit(left_join(ogcom,ogseriebik,by="date"))
    ogcom<-ogcom[,c(1,2,3,4,5,8)]
    ogcom<-na.omit(left_join(ogcom,ogserieal,by="date"))
    ogcom<-ogcom[,c(1,2,3,4,5,6,17)]
    ogcom<-ogcom[ogcom[,1]>1999,]
    highchart()%>%
        hc_xAxis(categories = ogcom[,1]) %>%
        hc_add_series(name = "OG Com Meth",data = ogcom[,2],type="line",marker = list(enabled=FALSE)) %>%
        hc_add_series(name = "OG COM",data = ogcom[,3],type="line",marker = list(enabled=FALSE),color="darkblue", dashStyle = "DashDot") %>%
        hc_add_series(name = "OG Prod Func",data = ogcom[,4],type="line",marker = list(enabled=FALSE),color="red") %>%
        hc_add_series(name = "OG HP Filter",data = ogcom[,5],type="line",marker = list(enabled=FALSE),color="green") %>%
        hc_add_series(name = "OG Kalman Bivariate",data = ogcom[,6],type="line",marker = list(enabled=FALSE),color="orange") %>%
        hc_add_series(name = "OG Proposal",data = ogcom[,7],type="line",marker = list(enabled=FALSE),color="black") %>%
        hc_title(text="OuputGap") %>%
        hc_subtitle(text="GDP %")%>%
        hc_yAxis(plotLines=list(
            list(
                color="black",
                width=2,
                value=0
            )
        ))%>%
        hc_add_theme(hc_theme_gridlight()) %>% hc_exporting(enabled = TRUE)%>%
        hc_plotOptions(series = list(showInLegend = TRUE))
}

ogurcorr<-function(ogserie,datos){
    auxi<-na.omit(left_join(ogserie,filtra(datos,tparoID),by="date"))
    auxi<-auxi[,c("date","OG","value")]
    colnames(auxi)<-c("Date","OG","UR")
    auxi<-auxi[auxi$Date>1999,]
    modlss <- lm(UR ~ OG, data = auxi)
    fit <- arrange(augment(modlss), OG) %>% 
        mutate(.se = predict(modlss, se = TRUE)$se.fit)
    hc <- hchart(
        auxi,
        type = "scatter",
        hcaes(x = OG, y = UR),
        name = "OG vs UR",
        showInLegend = FALSE
    ) %>%
        hc_plotOptions(
            series = list(
                dataLabels = list(
                    enabled = TRUE,
                    format="{point.Date}",
                    color="grey",
                    style = list(textOutline = FALSE)
                )
            )
        )%>%
        hc_tooltip(pointFormat = "OG: {point.x:.2f} <br> UR: {point.y:.2f}")%>%
        hc_title(text="OutputGap Vs Unemployment Rate")%>%
        hc_add_series(
            fit,
            type = "spline",
            hcaes(x=OG,y=.fitted),
            name = "Fit",
            id = "fit", # this is for link the arearange series to this one and have one legend
        )%>% 
        hc_add_theme(hc_theme_gridlight())
    hc
}

nawrucal<-function(ogserie,datos){
    auxi<-na.omit(left_join(ogserie,filtra(datos,tparoID),by="date"))
    auxi<-auxi[,c("date","OG","value")]
    colnames(auxi)<-c("Date","OG","UR")
    modlss <- lm(UR ~ OG, data = auxi)
    newvalue<-as.data.frame(matrix(0,1,1))
    colnames(newvalue)="OG"
    return(predict(modlss,newdata=newvalue))
}

nawruofical<-function(datos){
    auxi<-na.omit(left_join(filtra(datos,ogcID),filtra(datos,tparoID),by="date"))
    auxi<-auxi[,c("date","value.x","value.y")]
    colnames(auxi)<-c("Date","OG","UR")
    modlss <- lm(UR ~ OG, data = auxi)
    newvalue<-as.data.frame(matrix(0,1,1))
    colnames(newvalue)="OG"
    return(predict(modlss,newdata=newvalue))
}

ogincorr<-function(ogserie,datos){
    auxi<-na.omit(left_join(ogserie,filtra(datos,inflID),by="date"))
    auxi<-auxi[,c("date","OG","value")]
    colnames(auxi)<-c("Date","OG","Inflation")
    auxi<-auxi[auxi$Date>1999,]
    modlss <- lm(Inflation ~ OG, data = auxi)
    fit <- arrange(augment(modlss), OG) %>% 
        mutate(.se = predict(modlss, se = TRUE)$se.fit)
    hc <- hchart(
        auxi,
        type = "scatter",
        hcaes(x = OG, y = Inflation),
        name = "OG vs Inflation",
        showInLegend = FALSE
    ) %>%
        hc_plotOptions(
            series = list(
                dataLabels = list(
                    enabled = TRUE,
                    format="{point.Date}",
                    color="grey",
                    style = list(textOutline = FALSE)
                )
            )
        )%>%
        hc_tooltip(pointFormat = "OG: {point.x:.2f} <br> Inflation: {point.y:.2f}")%>%
        hc_title(text="OutputGap Vs Inflation")%>%
        hc_add_series(
            fit,
            type = "spline",
            hcaes(x=OG,y=.fitted),
            name = "Fit",
            id = "fit", # this is for link the arearange series to this one and have one legend
        )%>% 
        hc_add_theme(hc_theme_gridlight())
    hc
}

ogcabcorr<-function(ogserie,datos){
    auxi<-na.omit(left_join(ogserie,filtra(datos,cabID),by="date"))
    auxi<-auxi[,c("date","OG","value")]
    colnames(auxi)<-c("Date","OG","CAB")
    auxi<-auxi[auxi$Date>1999,]
    modlss <- lm(CAB ~ OG, data = auxi)
    fit <- arrange(augment(modlss), OG) %>% 
        mutate(.se = predict(modlss, se = TRUE)$se.fit)
    hc <- hchart(
        auxi,
        type = "scatter",
        hcaes(x = OG, y = CAB),
        name = "OG vs CAB",
        showInLegend = FALSE
    ) %>%
        hc_plotOptions(
            series = list(
                dataLabels = list(
                    enabled = TRUE,
                    format="{point.Date}",
                    color="grey",
                    style = list(textOutline = FALSE)
                )
            )
        )%>%
        hc_tooltip(pointFormat = "OG: {point.x:.2f} <br> CAB: {point.y:.2f}")%>%
        hc_title(text="OutputGap Vs Current Account Balance")%>%
        hc_add_series(
            fit,
            type = "spline",
            hcaes(x=OG,y=.fitted),
            name = "Fit",
            id = "fit", # this is for link the arearange series to this one and have one legend
        )%>% 
        hc_add_theme(hc_theme_gridlight())
    hc
}

colorea<-function(v){
    if(v>0){relleno="green"}else{relleno="red"}
}

flecha<-function(valor){
    if(valor>0){cupido="arrow-alt-circle-up"}else{cupido="arrow-alt-circle-down"}
    return(cupido)
}

ui <- dashboardPage(
    dashboardHeader(
        title = tags$b(tags$img(src="https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/eu.svg", width = 50),tags$b('European Commission Methodology to Estimate Output Gap')),
        titleWidth = 800),
    
    dashboardSidebar(id="",
                     tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),
                     selectInput("bins", "Select a Country", codigos[c(7:34,nrow(codigos)),"Name"], selected = "Spain", multiple = FALSE,
                                 selectize = TRUE, width = NULL, size = NULL),
                     htmlOutput("bandera"),
                     valueBoxOutput("pibkbox",width=12),
                     valueBoxOutput("inflbox",width=12),
                     valueBoxOutput("cabbox",width=12),
                     valueBoxOutput("defbox",width=12),
                     valueBoxOutput("tparobox",width=12),
                     valueBoxOutput("pibpcbox",width=12)
    ),
    
    dashboardBody(customTheme,
        tabBox(title="",id="tabset1",width=12,
               tabPanel("Basics",
                    fluidRow(width=12),
                    fluidRow(width=12,
                        box(highchartOutput("pibdesc"),width=6),
                        box(highchartOutput("pibcrec"),width=6),
                        box(highchartOutput("phillipsbox"),width=4),
                        box(highchartOutput("caburbox"),width=4),
                        box(highchartOutput("varpiburbox"),width=4)
                    )
               ),
               tabPanel("Time Series",
                    fluidRow(width=12),
                    fluidRow(width=12,
                        box(highchartOutput("tspotbox"),width=6),
                        box(highchartOutput("tsogbox"),width=6),
                        box(highchartOutput("tsogincorrbox"),width=4),
                        box(highchartOutput("tsogurcorrbox"),width=4),
                        box(highchartOutput("tsogcabcorrbox"),width=4)
                    )
               ),
               tabPanel("Hodrick-Prescott",
                    fluidRow(width=12,
                             column(width=2, style="margin-left:10px;",
                                    numericInput("freq1","HP filter frequency (lambda):",min=5,max=200,value=10,step=5,width="250px"),
                             ),
                    ),
                    fluidRow(width=12,
                        box(highchartOutput("hppotbox"),width=6),
                        box(highchartOutput("hpogbox"),width=6),
                        box(highchartOutput("hpogincorrbox"),width=4),
                        box(highchartOutput("hpogurcorrbox"),width=4),
                        box(highchartOutput("hpogcabcorrbox"),width=4)
                    )
               ),
               #            tabPanel("Baxter-King",
               #                     box(highchartOutput("bkpotbox"),width=6),
               #                     box(highchartOutput("bkogbox"),width=6),
               #                     box(highchartOutput("bkogincorrbox"),width=4),
               #                     box(highchartOutput("bkogurcorrbox"),width=4),
               #                     box(highchartOutput("bkogcabcorrbox"),width=4)
               #            ),
               #            tabPanel("Christiano-Fitzgerald",
               #                     box(highchartOutput("cfpotbox"),width=6),
               #                     box(highchartOutput("cfogbox"),width=6),
               #                     box(highchartOutput("cfogincorrbox"),width=4),
               #                     box(highchartOutput("cfogurcorrbox"),width=4),
               #                     box(highchartOutput("cfogcabcorrbox"),width=4)
               #            ),
               tabPanel("Kalman Univariate",
                    fluidRow(width=12),
                    fluidRow(width=12,
                        box(highchartOutput("kalpotbox"),width=6),
                        box(highchartOutput("kalogbox"),width=6),
                        box(highchartOutput("kalogincorrbox"),width=4),
                        box(highchartOutput("kalogurcorrbox"),width=4),
                        box(highchartOutput("kalogcabcorrbox"),width=4)
                    )
               ),
               tabPanel("Kalman (GDP+UR)",
                    fluidRow(width=12),
                    fluidRow(width=12,
                        box(highchartOutput("kalurpotbox"),width=6),
                        box(highchartOutput("kalurogbox"),width=6),
                        box(highchartOutput("kalurogincorrbox"),width=4),
                        box(highchartOutput("kalurogurcorrbox"),width=4),
                        box(highchartOutput("kalurogcabcorrbox"),width=4)
                    )
               ),
               tabPanel("Kalman (GDP+UR+CAB)",
                    fluidRow(width=12),
                    fluidRow(width=12,
                        box(highchartOutput("kalurcabpotbox"),width=6),
                        box(highchartOutput("kalurcabogbox"),width=6),
                        box(highchartOutput("kalurcabogincorrbox"),width=4),
                        box(highchartOutput("kalurcabogurcorrbox"),width=4),
                        box(highchartOutput("kalurcabogcabcorrbox"),width=4)
                    )
               ),
               tabPanel("Cobb-Douglas (HP)",
                    fluidRow(width=12,
                             column(width=2, style="margin-left:10px;",
                                    numericInput("freq2","HP filter frequency (lambda):",min=5,max=200,value=10,step=5,width="250px"),
                             ),
                    ),
                    fluidRow(width=12,
                        box(highchartOutput("prodfunpotbox"),width=6),
                        box(highchartOutput("prodfunogbox"),width=6),
                        box(highchartOutput("prodfunogincorrbox"),width=4),
                        box(highchartOutput("prodfunogurcorrbox"),width=4),
                        box(highchartOutput("prodfunogcabcorrbox"),width=4)
                    )
               ),
               tabPanel("Cobb-Douglas (K)",
                    fluidRow(width=12),
                    fluidRow(width=12,
                        box(highchartOutput("prodfunkalpotbox"),width=6),
                        box(highchartOutput("prodfunkalogbox"),width=6),
                        box(highchartOutput("prodfunkalogincorrbox"),width=4),
                        box(highchartOutput("prodfunkalogurcorrbox"),width=4),
                        box(highchartOutput("prodfunkalogcabcorrbox"),width=4)
                    )
               ),
               tabPanel("EC Meth",
                        fluidRow(width=12,
                                 column(width=1,
                                        fluidRow(width=12,infoBoxOutput("blanco",width=12)),
                                        fluidRow(width=12,numericInput("freq","HP filter frequency:",min=5,max=200,value=10,step=5,width="150px"))
                                 ),
                                 column(width=3,
                                        fluidRow(width=12,infoBoxOutput("comanchor",width=12)),
                                        fluidRow(width=12,
                                                 column(width=6,radioButtons("anchor", "Anchor:",
                                                                       c("Commission" = 1,
                                                                         "Custom" = 0,
                                                                         "No Anchor"=2))),
                                                 column(width=6,numericInput("custanch","Custom Anchor:",min=0,max=20,value=10,step=1,width="150px"))
                                        )
                                 ),
                                 column(width=3,
                                        fluidRow(width=12,infoBoxOutput("comadj",width=12)),
                                        fluidRow(width=12,radioButtons("nawruadj", "NAWRU Adjustment:",
                                                                       c("Commission" = 1,
                                                                         "No Adjustment"=2)))
                                 ),
                                 column(width=3,
                                        fluidRow(width=12,radioButtons("cutbounds", "NAWRU Estimates Variance Bounds:",
                                                                       c("Commission" = 1,
                                                                         "Custom"=2))),
                                        fluidRow(width=12,
                                                 column(width=6,
                                                        numericInput("vuplb","LB Trend Level Var:",min=0.00000001,max=0.05,value=0.00000001,step=0.00000001,width="150px")
                                                 ),
                                                 column(width=6,
                                                        numericInput("vupub","UB Trend Level Var:",min=0.0001,max=1,value=0.01,step=0.0001,width="150px")
                                                 )
                                        ),
                                        fluidRow(width=12,
                                                 column(width=6,
                                                        numericInput("vuslopelb","LB Trend Slope Var:",min=0,max=0.3,value=0.00000001,step=0.00000001,width="150px")
                                                 ),
                                                 column(width=6,
                                                        numericInput("vuslopeub","UB Trend Slope Var:",min=0,max=1,value=0.2,step=0.001,width="150px")
                                                 )
                                        ),
                                        fluidRow(width=12,
                                                 column(width=6,
                                                        numericInput("vuglb","LB Cycle Var:",min=0.00000001,max=0.2,value=0.00000001,step=0.00000001,width="150px")
                                                 ),
                                                 column(width=6,
                                                        numericInput("vugub","UB Cycle Var:",min=0.01,max=2,value=1,step=0.001,width="150px")
                                                 )
                                        )
                                 ),
                                 column(width=2,DTOutput("cboundsbox"))
                            ),
                        fluidRow(width=12,
                            box(highchartOutput("comisionpotbox"),width=6),
                            box(highchartOutput("comisionogbox"),width=6),
                            box(highchartOutput("nawrubox"),width=6),
                            box(highchartOutput("descogbox"),width=6),
                            box(highchartOutput("comisionogincorrbox"),width=4),
                            box(highchartOutput("comisionogurcorrbox"),width=4),
                            box(highchartOutput("comisionogcabcorrbox"),width=4)
                        )
               ),
               tabPanel("Proposal",
                        fluidRow(width=12,
                                 box(highchartOutput("proposalpotbox"),width=6),
                                 box(highchartOutput("proposalogbox"),width=6),
                                 box(highchartOutput("proposalnawrubox"),width=6),
                                 box(highchartOutput("descogalbox"),width=6),
                                 box(highchartOutput("proposalogincorrbox"),width=4),
                                 box(highchartOutput("proposalogurcorrbox"),width=4),
                                 box(highchartOutput("proposalogcabcorrbox"),width=4)
                        )
               ),
               tabPanel("Remix",
                        fluidRow(width=12,
                                 h2("Unemployment Rate which makes OG=0",style="margin-left:10px;"),
                                 textOutput("ofanchor"),
                                 tags$head(tags$style("#ofanchor{
                                 color: red;
                                 margin-left:10px;
                                 margin-top:-5px;
                                 margin-bottom:10px;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                                 ))
                            ),
                        fluidRow(width=12,
                                 valueBoxOutput("ofnawru",width=2),
                                 valueBoxOutput("comnawru",width=2),
                                 valueBoxOutput("propnawru",width=2),
                                 valueBoxOutput("prodfnawru",width=2),
                                 valueBoxOutput("hpnawru",width=2),
                                 valueBoxOutput("biknawru",width=2)
                        ),
                        fluidRow(width=12,
                                 box(highchartOutput("remixogbox",height=500),width=12)
                        )
               ),
               tabPanel("Paper",
                        tags$iframe(style="height:1000px; width:100%; scrolling=yes", 
                                    src="Paper.pdf")
                        ),
               tabPanel(a("<Code>",href="https://github.com/frsabido/OutputGap",target="_blank")
               )
        )
    )
)


server <- function(input, output) {
    
    output$report = downloadHandler(
        filename = 'mycode.pdf',
        
        content = function(file) {
            out = knit2pdf('app.R', clean = TRUE)
            file.rename(out, file) # move pdf to file for downloading
        },
        
        contentType = 'application/pdf'
    )

    valores<-reactive({tablapais(codigos$CODE[codigos$Name==input$bins])})
    
    output$bandera<-renderUI({
        tags$img(src=paste0("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/",tolower(codigos[codigos$Name==input$bins,"iso2c"]),".svg"),width=200,height=100,style="margin:15px;margin-top:-10px;")
    })
    output$phillipsbox<-renderHighchart({phillips(valores())})
    output$caburbox<-renderHighchart({cabur(valores())})
    output$varpiburbox<-renderHighchart({varpibur(valores())})
    output$pibdesc<-renderHighchart({descpibkg(valores())})
    output$pibcrec<-renderHighchart({varpibplot(valores())})
    
    output$tspotbox<-renderHighchart({potplot(tsog(valores()))})
    output$tsogbox<-renderHighchart({ogplot(tsog(valores()),valores())})
    output$tsogincorrbox<-renderHighchart({ogincorr(tsog(valores()),valores())})
    output$tsogurcorrbox<-renderHighchart({ogurcorr(tsog(valores()),valores())})
    output$tsogcabcorrbox<-renderHighchart({ogcabcorr(tsog(valores()),valores())})
    
    output$hppotbox<-renderHighchart({potplot(hpog(valores(),input$freq1))})
    output$hpogbox<-renderHighchart({ogplot(hpog(valores(),input$freq1),valores())})
    output$hpogincorrbox<-renderHighchart({ogincorr(hpog(valores(),input$freq1),valores())})
    output$hpogurcorrbox<-renderHighchart({ogurcorr(hpog(valores(),input$freq1),valores())})
    output$hpogcabcorrbox<-renderHighchart({ogcabcorr(hpog(valores(),input$freq1),valores())})
    
    output$bkpotbox<-renderHighchart({potplot(bkog(valores()))})
    output$bkogbox<-renderHighchart({ogplot(bkog(valores()),valores())})
    output$bkogincorrbox<-renderHighchart({ogincorr(bkog(valores()),valores())})
    output$bkogurcorrbox<-renderHighchart({ogurcorr(bkog(valores()),valores())})
    output$bkogcabcorrbox<-renderHighchart({ogcabcorr(bkog(valores()),valores())})
    
    output$cfpotbox<-renderHighchart({potplot(cfog(valores()))})
    output$cfogbox<-renderHighchart({ogplot(cfog(valores()),valores())})
    output$cfogincorrbox<-renderHighchart({ogincorr(cfog(valores()),valores())})
    output$cfogurcorrbox<-renderHighchart({ogurcorr(cfog(valores()),valores())})
    output$cfogcabcorrbox<-renderHighchart({ogcabcorr(cfog(valores()),valores())})
    
    output$kalpotbox<-renderHighchart({potplot(kalog(valores()))})
    output$kalogbox<-renderHighchart({ogplot(kalog(valores()),valores())})
    output$kalogincorrbox<-renderHighchart({ogincorr(kalog(valores()),valores())})
    output$kalogurcorrbox<-renderHighchart({ogurcorr(kalog(valores()),valores())})
    output$kalogcabcorrbox<-renderHighchart({ogcabcorr(kalog(valores()),valores())})
    
    output$kalurpotbox<-renderHighchart({potplot(kalurog(valores()))})
    output$kalurogbox<-renderHighchart({ogplot(kalurog(valores()),valores())})
    output$kalurogincorrbox<-renderHighchart({ogincorr(kalurog(valores()),valores())})
    output$kalurogurcorrbox<-renderHighchart({ogurcorr(kalurog(valores()),valores())})
    output$kalurogcabcorrbox<-renderHighchart({ogcabcorr(kalurog(valores()),valores())})
    
    output$kalurcabpotbox<-renderHighchart({potplot(kalurcabog(valores()))})
    output$kalurcabogbox<-renderHighchart({ogplot(kalurcabog(valores()),valores())})
    output$kalurcabogincorrbox<-renderHighchart({ogincorr(kalurcabog(valores()),valores())})
    output$kalurcabogurcorrbox<-renderHighchart({ogurcorr(kalurcabog(valores()),valores())})
    output$kalurcabogcabcorrbox<-renderHighchart({ogcabcorr(kalurcabog(valores()),valores())})
    
    output$prodfunpotbox<-renderHighchart({potplot(prodfunog(valores(),input$freq2))})
    output$prodfunogbox<-renderHighchart({ogplot(prodfunog(valores(),input$freq2),valores())})
    output$prodfunogincorrbox<-renderHighchart({ogincorr(prodfunog(valores(),input$freq2),valores())})
    output$prodfunogurcorrbox<-renderHighchart({ogurcorr(prodfunog(valores(),input$freq2),valores())})
    output$prodfunogcabcorrbox<-renderHighchart({ogcabcorr(prodfunog(valores(),input$freq2),valores())})
    
    output$prodfunkalpotbox<-renderHighchart({potplot(prodfunkalog(valores()))})
    output$prodfunkalogbox<-renderHighchart({ogplot(prodfunkalog(valores()),valores())})
    output$prodfunkalogincorrbox<-renderHighchart({ogincorr(prodfunkalog(valores()),valores())})
    output$prodfunkalogurcorrbox<-renderHighchart({ogurcorr(prodfunkalog(valores()),valores())})
    output$prodfunkalogcabcorrbox<-renderHighchart({ogcabcorr(prodfunkalog(valores()),valores())})
    
    datoscom<-reactive({
        if (input$cutbounds==1){
            bounds<-cbounds(valores())[1,]
            }else{bounds<-c(input$vuplb,input$vupup,input$vuslopelb,input$vuslopeub,input$vuglb,input$vugub)}
        comisionog(valores(),input$freq,input$anchor,input$custanch,input$nawruadj,as.numeric(bounds[1]),as.numeric(bounds[2]),as.numeric(bounds[3]),as.numeric(bounds[4]),as.numeric(bounds[5]),as.numeric(bounds[6]))
        })
    output$comisionpotbox<-renderHighchart({potplot(datoscom())})
    output$comisionogbox<-renderHighchart({ogplot(datoscom(),valores())})
    output$nawrubox<-renderHighchart({nawruplot(datoscom(),valores())})
    output$descogbox<-renderHighchart({descog(datoscom())})
    output$comisionogincorrbox<-renderHighchart({ogincorr(datoscom(),valores())})
    output$comisionogurcorrbox<-renderHighchart({ogurcorr(datoscom(),valores())})
    output$comisionogcabcorrbox<-renderHighchart({ogcabcorr(datoscom(),valores())})
    
    output$proposalpotbox<-renderHighchart({potplot(alternative(valores()))})
    output$proposalogbox<-renderHighchart({ogplot(alternative(valores()),valores())})
    output$proposalnawrubox<-renderHighchart({nawruplot(alternative(valores()),valores())})
    output$descogalbox<-renderHighchart({descogal(alternative(valores()))})
    output$proposalogincorrbox<-renderHighchart({ogincorr(alternative(valores()),valores())})
    output$proposalogurcorrbox<-renderHighchart({ogurcorr(alternative(valores()),valores())})
    output$proposalogcabcorrbox<-renderHighchart({ogcabcorr(alternative(valores()),valores())})

    output$remixogbox<-renderHighchart({remixplot(datoscom(),prodfunkalog(valores()),hpog(valores(),input$freq1),kalurog(valores()),alternative(valores()),valores())})
        
    output$pibkbox<-renderValueBox({
        valueBox(paste(round(valor(filtra(valores(),pibkID))[,7],2),"%"),paste("GDP growth in",valor(filtra(valores(),pibkID))[,1]),color=colorea(valor(filtra(valores(),pibkID))[,7]),icon=icon(flecha(valor(filtra(valores(),pibkID))[,7])))
    })
    output$inflbox<-renderValueBox({
        valueBox(paste(round(valor(filtra(valores(),inflID))[,7],2),"%"),paste("Inflation (CPI) in",valor(filtra(valores(),inflID))[,1]),color=colorea(valor(filtra(valores(),inflID))[,7]),icon=icon(flecha(valor(filtra(valores(),inflID))[,7])))
    })
    output$cabbox<-renderValueBox({
        valueBox(paste(round(valor(filtra(valores(),cabID))[,3],2),"%"),paste("CAB (% GDP) in",valor(filtra(valores(),cabID))[,1]),color=colorea(valor(filtra(valores(),cabID))[,3]),icon=icon(flecha(valor(filtra(valores(),cabID))[,3])))
    })
    output$defbox<-renderValueBox({
        valueBox(paste(round(valor(deficit(valores()))[,3],2),"%"),paste("Gov Bal (%GDP) in",valor(deficit(valores()))[,1]),color=colorea(valor(deficit(valores()))[,3]),icon=icon(flecha(valor(deficit(valores()))[,3])))
    })
    output$tparobox<-renderValueBox({
        valueBox(paste(round(valor(filtra(valores(),tparoID))[,3],2),"%"),paste("UR in",valor(filtra(valores(),tparoID))[,1]),icon=icon("industry"))
    })
    output$pibpcbox<-renderValueBox({
        valueBox(formatC(round(valor(filtra(valores(),pibpcID))[,3]*1000,0),big.mark=",",format="d"),paste("GDPpc (€) in",valor(filtra(valores(),pibpcID))[,1]),icon=icon("euro-sign"))
    })
    
    output$comanchor<-renderInfoBox({
        infoBox(
            "Commission NAWRU anchor", codigos[codigos$Country==input$bins,"Anchor"], icon = icon("anchor"),
            color = "blue"
        )
    })
    
    output$ofanchor<-renderText({paste0("Commission Anchor was ",codigos[codigos$Country==input$bins,"Anchor"])})
        
    output$ofnawru<-renderValueBox({
        valueBox(round(nawruofical(valores()),2),"COM data",color="aqua")
    })
    
    output$comnawru<-renderValueBox({
        valueBox(round(nawrucal(datoscom(),valores()),2),"COM Meth value",color="aqua")
    })
    
    output$propnawru<-renderValueBox({
        valueBox(round(nawrucal(alternative(valores()),valores()),2),"Proposal Meth value",color="aqua")
    })
    
    output$prodfnawru<-renderValueBox({
        valueBox(round(nawrucal(prodfunkalog(valores()),valores()),2),"PF(KF) Meth value",color="aqua")
    })
    
    output$hpnawru<-renderValueBox({
        valueBox(round(nawrucal(hpog(valores(),input$freq1),valores()),2),"HP Meth value",color="aqua")
    })
    
    output$biknawru<-renderValueBox({
        valueBox(round(nawrucal(kalurog(valores()),valores()),2),"Biv KF Meth value",color="aqua")
    })
    
    output$comadj<-renderInfoBox({
        infoBox(
            "Commission NAWRU adjustment", codigos[codigos$Country==input$bins,"AdjFactor"], icon = icon("adjust"),
            color = "blue"
        )
    })
    
    output$cboundsbox<-renderDT({
        bounds<-cbounds(valores())
        matriz<-as.data.frame(matrix(3,2,0))
        matriz[1,1]<-bounds[1,1]
        matriz[1,2]<-bounds[1,2]
        matriz[2,1]<-bounds[1,3]
        matriz[2,2]<-bounds[1,4]
        matriz[3,1]<-bounds[1,5]
        matriz[3,2]<-bounds[1,6]
        colnames(matriz)<-c("LB","UB")
        rownames(matriz)<-c("Trend Level","Trend Slope","Cycle")
        datatable(matriz, ,caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            htmltools::em('Commission Bounds.')
        ),extensions = 'FixedColumns', rownames=TRUE,class='cell-border stripe',options = list(columnDefs = list(list(className = 'dt-right', targets = 1)),pageLength = 3, dom = 't',fixedColumns = TRUE))%>%formatRound(names(matriz),8)%>%formatStyle(names(matriz),backgroundColor = 'darkblue',color="white",fontWeight = "bold")
    })
}


shinyApp(ui = ui, server = server)
