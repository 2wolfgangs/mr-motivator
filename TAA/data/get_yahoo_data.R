# =====================================================
# Gets Yahoo data using the quantmod getSymbols package
# =====================================================
require(quantmod)
require(SIT)

# Get data for dataset 1 (global 11 asset classes)
# ------------------------------------------------
#
# Asset Class   Yahoo Ticker  Yahoo data from
# ------------- ------------- ---------------
# U.S. Stocks	    SPY         29/01/1993
# All world	      VGTSX	      31/12/1998
# Japan	          EWJ	        1/04/1996
# EM	            VEIEX	      12/09/1995
# US Real Estate	VGSIX	      28/06/1996
# Intmed Treasury	VFIUX	      23/02/2001
# Long Treasury	  VUSTX	      14/12/1989
# Short Treasury	VFSIX	      16/10/1997

tickers <- c('SPY','VGTSX','EWJ','VEIEX','VGSIX','VFIUX','VUSTX','VFSIX')

# Put all the yahoo data in its own environment,
# so we can munge it separately and more efficiently.
raw.yahoo.data <- new.env()

# Read data into yahoodata environment using quantmod
getSymbols(tickers, src = 'yahoo', from = '1990-01-01', to = '2013-08-30', env = raw.yahoo.data, 
           auto.assign = T)


# Get data for dataset 2 (8 ETFs from 2002)
# -----------------------------------------
raw.yahoo.data.8ETF <- new.env()
tickers <- c('SPY','QQQ','EEM','IWM','EFA','TLT','IYR','GLD')
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', to = '2013-08-30', env = raw.yahoo.data.8ETF, 
           auto.assign = T)

# Get data for dataset 3 (Dow Stock - Engle)
# -----------------------------------------
raw.yahoo.data.dow <- new.env()
tickers <- c('AA','AXP','BA','CAT','DD','DIS','GE','IBM','IP','JNJ','JPM',
             'KO','MCD','MMM','MO','MRK','MSFT')
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', to = '2013-08-30', env = raw.yahoo.data.dow, 
           auto.assign = T) 

# Get data for dataset 4 (Major asset classes)
# --------------------------------------------
raw.yahoo.data.major <- new.env()
tickers <- c('VTI','IEV','EEM','EWJ','AGG','GSG','GLD','ICF')
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', to = '2013-08-30', env = raw.yahoo.data.major, 
           auto.assign = T)

# Get data for dataset 5 (Nasdaq 100 stocks)
# ------------------------------------------
# Uses SIT package function spl
raw.yahoo.data.nasdaq <- new.env()
tickers <-
  spl('ATVI,ADBE,ALTR,AMZN,AMGN,APOL,AAPL,AMAT,ADSK,ADP,BBBY,BIIB,BMC,BRCM,CHRW,CA,CELG,CEP
H,CERN,CHKP,CTAS,CSCO,CTXS,CTSH,CMCSA,COST,DELL,XRAY,DISH,EBAY,ERTS,EXPD,ESRX,FAST,FISV,F
LEX,FLIR,FWLT,GILD,HSIC,HOLX,INFY,INTC,INTU,JBHT,KLAC,LRCX,LIFE,LLTC,LOGI,MAT,MXIM,MCHP,M
SFT,MYL,NTAP,NWSA,NVDA,ORLY,ORCL,PCAR,PDCO,PAYX,PCLN,QGEN,QCOM,RIMM,ROST,SNDK,SIAL,SPLS,S
BUX,SRCL,SYMC,TEVA,URBN,VRSN,VRTX,VOD,XLNX,YHOO')
for(i in tickers) {
  try(getSymbols(i, src = 'yahoo', from = '1980-01-01', env = raw.yahoo.data.nasdaq, 
                 auto.assign = T), TRUE) 
}
