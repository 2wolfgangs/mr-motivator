# =====================================================
# Gets Yahoo data using the quantmod getSymbols package
# =====================================================

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