# Web-Scaping PGA Tour Statistics using R
Short web-scraping code to collect PGA Tour statistics from <a href="https://www.pgatour.com/stats.html">https://www.pgatour.com/stats.html</a>

The code requires the `year` and an optional `stat id` as list inputs. <br>
It consists of 2 parts:
1. **HTML Links Builder:** <br>
   This part consists of building the html strings used in the next step. <br>
   It also retrieves a list of `stat id` which can be used for selection.
2. **Web Scaping Loop:** <br>
   This is where the web-scraping magic happens! <br>
   It loops through all the pages and spits out the data in a long format.
