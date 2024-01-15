cd "/Users/jurrell/Desktop/文章/Environmental Research Letters投稿/缩减版本/最终缩减版本/Data&Code" 
clear all

***Table S1:Data Sources and Summary Statistics
use updata, clear
global c "lnpgdp spgdp lnssg lngasc lngr lngi lngswur" 
sum2docx Eff treat $c using Summary.docx, replace stats(N mean sd)

***Figure S1:Cumulative distribution of city-county mergers over time
use cxsq,clear
sort chgtime
bys chgtime: gen number=_N
duplicates drop chgtime, force
merge 1:1 chgtime using year
sort chgtime
drop _merge
replace number=0 if number==.
label var number "the number of city-county merger events that occur annually"
gen total=sum(number)
label var total "Cumulative number of city-county mergers over time"
gen year=chgtime
sort year
graph set window fontface "Times New Roman"
twoway bar total year, xscale(range(1986 2021)) xlabel(1986(5)2021) xtitle("Year") ytitle("Cumulative Amount of City-County Merger",) ylabel(,nogrid tposition(inside) format(%12.0f) angle(horizontal)) xlabel(,tposition(inside)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) legend(off) plotr(lcolor(edkblue) lpattern(1) lwidth(*1.5))
graph export "figureS1.svg", replace

***Figure S2:Event Study Estimates of the Effect of City-County Merger on Urbanization Rate
use fdata, clear
replace distance = -8 if distance < -8
replace distance = 8 if distance > 8
reghdfe ur d_8 d_7 d_6 d_5 d_4 d_3 d_2 d_1 d1 d2 d3 d4 d5 d6 d7 d8 $c, absorb(year citycode) vce(r)
graph set window fontface "Times New Roman"
coefplot, keep(d_8 d_7 d_6 d_5 d_4 d_3 d_2 d_1 d1 d2 d3 d4 d5 d6 d7 d8) coeflabels(d_8="-8+" d_7="-7" d_6="-6" d_5="-5" d_4="-4" d_3="-3" d_2="-2" d_1="-1" d1="1" d2="2" d3="3" d4="4" d5="5" d6="6" d7="7" d8="8+") levels(95) vertical ///
 yline(0,lcolor(teal) lwidth(vthin) lpattern(dash)) ///
 ylabel(,nogrid tposition(inside) tlcolor(gs10) angle(horizontal)) ///
 xline(8.5, lwidth(vthin) lpattern(dash) lcolor(teal)) ///
 xlabel(,nogrid tposition(inside) tlcolor(gs10)) ///
 ytitle(Coefficients, margin(medium)) ///
 xtitle(Years Prior to and After City-County Merger, margin(medium)) ///
 recast(connect)lcolor(red*0.45) ///
 ciopts(lpattern(dash) recast(rcap) msize(medium)) ///
 msymbol(circle_hollow) ///
 graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
 plotr(lcolor(edkblue) lpattern(1) lwidth(*1.5)) ///
 scheme(s2color)
graph export "figureS2.svg", replace

***Figure S3:Scatter Plot of Urbanization Rate and Carbon Emissions
use mdata, clear
graph set window fontface "Times New Roman"
set scheme s2color 
graph twoway (scatter co2 ur,msize(small)) (lfit co2 ur, lwidth(thick) lcolor(black) lpattern(solid)), graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) plotregion(lcolor(edkblue) lpattern(1) lwidth(*1.5)) legend(off) ylabel(,nogrid tposition(inside) tlcolor(gs10) angle(horizontal)) xlabel(,tposition(inside) tlcolor(gs10)) xtitle(Urbanization Rate, margin(medium)) ytitle(Carbon Emissions,margin(medium))
graph export "figureS3.svg", replace

***Table S2:The Estimates of City-County Merger on Jointly Specified Time-varying Observable City Characteristics
use data, clear
reg treat $c, vce(cl citycode)
test $c
sca ftest=r(F)
sca pval=r(p)
outreg2 using Table2.doc, replace nocons addstat("F-test of joint significance",ftest,"P-value",pval) bdec(3) sdec(3) adec(3) nonotes adjr2 addtext(year,No, city,No)
reghdfe treat $c, absorb(year) vce(cl citycode)
test $c
sca ftest=r(F)
sca pval=r(p)
outreg2 using Table2.doc, append nocons addstat("F-test of joint significance",ftest,"P-value",pval) bdec(3) sdec(3) adec(3) nonotes adjr2 addtext(year,Yes, city,No)
reghdfe treat $c, absorb(year citycode) vce(cl citycode)
test $c
sca ftest=r(F)
sca pval=r(p)
outreg2 using Table2.doc, append nocons addstat("F-test of joint significance",ftest,"P-value",pval) bdec(3) sdec(3) adec(3) nonotes adjr2 addtext(year,Yes, city,Yes)

***Figure S4:Kernel Density of the Treat and Control Groups
use data, clear
set seed 10101
gen ranorder=runiform()
sort ranorder
psmatch2 treat $c, outcome(co2) n(1) ate ties logit common
graph set window fontface "Times New Roman"
set scheme s2color 
twoway(kdensity _ps if _treat==1,legend(label(1 "Treat")))(kdensity _ps if _treat==0, legend(label(2 "Control"))),xtitle(Propensity score,margin(medium)) title("Before Matching",color(black)) yscale(range(0 5)) ylabel(0(2)5, format(%4.0f) nogrid tposition(inside) angle(horizontal)) ytitle(Density,margin(medium)) xlabel(,format(%4.2f) tposition(inside)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) legend(size(small)) legend(ring(0) position(3) row(2) region(lcolor(white))) name(before,replace)
graph set window fontface "Times New Roman"
set scheme s2color 
twoway(kdensity _ps if _treat==1,legend(label(1 "Treat")))(kdensity _ps if (_weight!=1&_weight!=.), legend(label(2 "Control"))),xtitle(Propensity score,margin(medium)) title("After Matching",color(black)) yscale(range(0 5)) ylabel(0(2)5,format(%4.0f)nogrid tposition(inside) angle(horizontal)) ytitle(Density,margin(medium)) xlabel(,format(%4.2f) tposition(inside)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) legend(size(small)) legend(ring(0) position(3) row(2) region(lcolor(white))) name(after,replace)
graph combine before after,row(2) xsize(2) ysize(3) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))
graph export "figureS4.svg", replace

***Figure S5:Estimates of dynamics effect of City-County Merger on Carbon Efficiency
use updata, clear
reghdfe Eff d_12 d_11 d_10 d_9 d_8 d_7 d_6 d_5 d_4 d_3 d_2 d_1 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 $c, absorb(year citycode)
graph set window fontface "Times New Roman"
coefplot, keep(d_11 d_9 d_6 d_3 d3 d6 d9 d11) coeflabels(d_13="-13" d_12="-12" d_11="-11+" d_10="-10" d_9="-9" d_8="-8" d_7="-7" d_6="-6" d_5="-5" d_4="-4" d_3="-3" d_2="-2" d_1="-1" d1="1" d2="2" d3="3" d4="4" d5="5" d6="6" d7="7" d8="8" d9="9" d10="10" d11="11+" d12="12" d13="13" d14="14" d15="15+" d16="16" d17="17" d18="18")  levels(90) vertical /// 转置图形
 yline(0,lcolor(teal) lwidth(vthin) lpattern(dash)) ///
 ylabel(,nogrid tposition(inside) tlcolor(gs10) angle(horizontal) format(%4.2f)) /// 
 xline(4.5, lwidth(vthin) lpattern(dash) lcolor(teal)) ///
 xlabel(,nogrid tposition(inside) tlcolor(gs10)) ///
 ytitle(Coefficients, margin(medium)) ///
 xtitle(Years Prior to and After City-County Merger, margin(medium)) ///
 recast(connect)lcolor(red*0.45) ///
 ciopts(lpattern(dash) recast(rcap) msize(medium)) ///
 msymbol(circle_hollow) ///
 graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
 plotr(lcolor(edkblue) lpattern(1) lwidth(*1.5)) ///
 scheme(s2color)
graph export "figureS5.svg", replace

***Table 2:The Impact of City-County Merger on Carbon Efficiency
use updata,replace
reg Eff treat, vce(cl citycode)
estadd local Year "No"
estadd local City "No"
est store a1
reghdfe Eff treat , absorb(year citycode) vce(cl citycode)
estadd local Year "Yes"
estadd local City "Yes"
est store a2
reghdfe Eff treat $c, absorb(year citycode) vce(cl citycode)
estadd local Year "Yes"
estadd local City "Yes"
est store a3
esttab a1 a2 a3 using baseresults.rtf, replace ///
star(* 0.1 ** 0.05 *** 0.01) ///
b(%6.3f) se(%6.3f) ar2 compress nogap ///  
stats(Year City N r2_a,fmt(%12.0f %12.0f %12.0f %9.3f) label("Year FE" "City FE" "Observations" "Adjusted R2") ) varwidth(20) title(Table 2 The Impact of City-County Merger on Carbon Efficiency) mtitles("Eff" "Eff" "Eff")

***Figure S6:Distribution of estimated coefficients of falsification test
use updata, clear
permute treat _b[treat], reps(500) seed(10101) saving("simulations.dta",replace): reghdfe Eff treat $c, absorb(year citycode) vce(cl citycode)
use simulations, clear
graph set window fontface "Times New Roman"
dpplot _pm_1, xline(-0.052, lc(black*0.5) lp(dash)) xline(0, lc(black*0.5) lp(solid)) xlabel(-0.06(0.02)0.06, format(%4.2f)) xtitle("Estimator", margin(medium)) ytitle("Density",margin(medium)) ylabel(, nogrid format(%4.0f)) note("") caption("") graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))
graph export "figureS6.svg",replace

***Table S3:Robustness checks
**Expected factors***
use updata, clear
reghdfe Eff treat d_1 $c, absorb(year citycode) vce(cl citycode)
estadd local Year "Yes"
estadd local City "Yes"
est store a1
**Other relevant events
use updata, clear
merge 1:1 city year using lcity
drop _merge
drop if year==.
bys city: replace lccity=lccity[_n-1] if lccity==.
replace lccity=0 if lccity==.
reghdfe Eff treat lccity $c, absorb(year citycode) vce(cl citycode)
estadd local Year "Yes"
estadd local City "Yes" 
est store a2
**Match method
use data, clear
set seed 10101
gen ranorder=runiform()
sort ranorder
psmatch2 treat $c, outcome(co2) kernel ate ties logit common quietly
drop if _support==0
merge 1:1 citycode year using sbm
keep if _merge==3
drop _merge
label var TE "carbon efficiency"
rename TE Eff
reghdfe Eff treat $c, absorb(year citycode) vce(cl citycode)
estadd local Year "Yes"
estadd local City "Yes" 
est store a3
**Sample selection
use updata, clear
drop if city=="北京市" | city=="天津市" | city=="重庆市" | city=="上海市"
reghdfe Eff treat $c, absorb(year citycode) vce(cl citycode)
estadd local Year "Yes"
estadd local City "Yes" 
est store a4
**Model selection
use updata, clear
replace ur=ur/100
reghdfe Eff ur $c, absorb(year citycode) vce(cl citycode)
estadd local Year "Yes"
estadd local City "Yes"
est store a5
use updata, clear
winsor2 ur,replace cuts(1 99)
ivregress2 2sls Eff $c (ur=treat) i.year i.citycode
estadd local Year "Yes"
estadd local City "Yes"
est store a6

esttab a1 a2 a3 a4 a5 a6 using Table4.rtf, replace ///
star(* 0.1 ** 0.05 *** 0.01) ///
b(%6.3f) se(%6.3f) ar2 compress nogap ///  
stats(Year City N r2_a,fmt(%12.0f %12.0f %12.0f %9.3f) label("Year FE" "City FE" "Observations" "Adjusted R2") ) varwidth(20) title(Robustness checks) drop(*.year *.citycode)
 
**Estimation method selection
use updata, clear
replace chgtime=0 if chgtime==.
csdid Eff $c, ivar(citycode) time(year) gvar(chgtime) notyet method(stdipw) agg(simple)

**************Heterogeneity*******************
***Figure 2:Regional heterogeneity
use updata, clear
sort citycode year
gen east=(province=="北京市"|province=="天津市"|province=="河北省"|province=="辽宁省"|province=="上海市"|province=="江苏省"|province=="浙江省"|province=="福建省"|province=="山东省"|province=="广东省"|province=="广西壮族自治区"|province=="海南省") 
gen central=(province=="山西省"|province=="内蒙古自治区"|province=="吉林省"|province=="黑龙江省"|province=="安徽省"|province=="江西省"|province=="河南省"|province=="湖北省"|province=="湖南省") 
gen west=(province=="重庆市"|province=="四川省"|province=="贵州省"|province=="云南省"|province=="西藏自治区"|province=="陕西省"|province=="甘肃省"|province=="青海省"|province=="宁夏回族自治区"|province=="新疆维吾尔自治区")
gen treat×central=treat*central
gen treat×west=treat*west
reghdfe Eff treat treat×central treat×west $c, absorb(year citycode) vce(cl citycode)
est store a1
graph set window fontface "Times New Roman"
coefplot,keep(treat treat×central treat×west) coeflabels(treat="east region" treat×central="central region" treat×west="west region") levels(95) vertical lcolor(black) mcolor(black) msymbol(circle_hollow) ytitle(Coefficients, margin(medium)) xlabel(,format(%4.2f) tposition(inside))  ylabel(,format(%4.2f) tposition(inside) angle(horizontal) nogrid) yline(0, lwidth(vthin)lpattern(solid) lcolor(black)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ciopts(recast(rcap)) plotr(lcolor(edkblue) lpattern(1) lwidth(*1.5))
graph export "Figure 2.svg", replace

***Figure 3:Heterogeneity of city size
use updata, clear
gen smcity=(up<50)
gen Ismcity=(up>=20 & up<50)
gen IIsmcity=(up<20)
gen mecity=(up>=50 & up<100) 
gen lacity=(up>=100 & up<500)
gen Ilacity=(up>=300 & up<500)
gen IIlacity=(up>=100 & up<300)
gen xlcity=(up>=500 & up<1000)
gen xxlcity=(up>=1000)
gen treat×smcity=treat*smcity
gen treat×mecity=treat*mecity
gen treat×lacity=treat*lacity
gen treat×xlcity=treat*xlcity
reghdfe Eff treat treat×mecity treat×lacity treat×xlcity $c, absorb(year citycode) vce(cl citycode)
est store a1
coefplot,keep(treat treat×mecity treat×lacity treat×xlcity) coeflabels(treat="small city" treat×mecity="medium city" treat×lacity="large city" treat×xlcity="megalopolis") levels(95) vertical lcolor(black) mcolor(black) msymbol(circle_hollow) ytitle(Coefficients,margin(medium)) ylabel(,format(%4.2f) tposition(inside) angle(horizontal) nogrid) yline(0, lwidth(vthin) lpattern(solid) lcolor(black)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ciopts(recast(rcap)) xlabel(,tposition(inside)) plotr(lcolor(edkblue) lpattern(1) lwidth(*1.5))
graph export "Figure 3.svg", replace

***Table 3:Heterogeneity analysis results: digital technology
use updata, clear
merge 1:1 city year using dti
keep if _merge==3
drop _merge
sort citycode year
sum dti
bys year: egen dti_m=median(dti)
gen h_dti=(dti>dti_m)
reghdfe Eff treat $c if h_dti==1, absorb(year citycode) vce(cl citycode)
estadd local Year "Yes"
estadd local City "Yes"
est store a1
reghdfe Eff treat $c if h_dti==0, absorb(year citycode) vce(cl citycode)
estadd local Year "Yes"
estadd local City "Yes"
est store a2
esttab a1 a2 using Table5.rtf, replace ///
star(* 0.1 ** 0.05 *** 0.01) ///
b(%6.3f) se(%6.3f) ar2 compress nogap ///  
stats(Year City N r2_a,fmt(%12.0f %12.0f %12.0f %9.3f) label("Year FE" "City FE" "Observations" "Adjusted R2") ) varwidth(20) title(Heterogeneity analysis results: digital technology)

**************Mechanisms*******************
***Figure 4:Mechanism testing diagram for the impact of urbanization on carbon efficiency
***The energy utilization mechanism
use updata, clear
winsor2 cngst lpgst, cuts(1 99) replace
gen lncngst=ln(cngst)
gen lnlpgst=ln(lpgst)
reghdfe lncngst treat $c, absorb(year citycode) vce(r)
est store a1
reghdfe lnlpgst treat $c, absorb(year citycode) vce(r)
est store a2

***The transportation infrastructure mechanism
use updata, clear
replace lroad=lroad/100
reghdfe lroad treat $c, absorb(year citycode) vce(r)
est store a3

***The labor market mechanism
use updata, clear
reghdfe rsse treat $c, absorb(year citycode) vce(r)
est store a4

coefplot (a1,label("natural gas supply")) (a2,label("liquefied petroleum gas supply")) (a3,label("road length")) (a4,label(" industrial labor share")), keep(treat) xline(0,lp(dash) lc(black*0.5))  graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) legend(cols(2)) xtitle(Coefficients) msize(0.8) levels(95) ciopts(recast(rcap) msize(medium))

**************Discussions**************
***Table 4:The impact of City-County Merger on three carbon emission dimensions
use updata, clear
gen pco2=co2*100/tp
label var pco2 "carbon emissions per capita"
reghdfe co2 treat $c, absorb(year citycode) vce(r)
estadd local Year "Yes"
estadd local City "Yes"
est store a1
reghdfe pco2 treat $c, absorb(year citycode) vce(r)
estadd local Year "Yes"
estadd local City "Yes"
est store a2
reghdfe ico2 treat $c, absorb(year citycode) vce(r)
estadd local Year "Yes"
estadd local City "Yes"
est store a3

esttab a1 a2 a3 using Discussion.rtf, replace ///
star(* 0.1 ** 0.05 *** 0.01) ///
b(%6.3f) se(%6.3f) ar2 compress nogap ///  
stats(Year City N r2_a,fmt(%12.0f %12.0f %12.0f %9.3f) label("Year FE" "City FE" "Observations" "Adjusted R2") ) varwidth(20) title(Table 7 The impact of City-County Merger on three carbon emission dimensions) mtitles("co2" "pco2" "ico2")

