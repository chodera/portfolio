---
layout: post
title: "Unequal political participation in Europe"
output:
  html_document:
    df_print: paged
image: assets/images/vote_opt.jpg
normal: true
show: true
logo: r
---

This post uses some insights from my master's thesis where I investigated contextual determinants of inequalities in political participation across European regions, Here I will give a short overview of some descriptive key findings.

Data for the analysis came from several sources but the main source on individual-level data was the latest [European Social Survey (ESS)](https://www.europeansocialsurvey.org/){:target="_blank"} from 2016. The various data sets were prepared and merged together with the statistical software package Stata. In the following, we start by using the prepared data set and do not repeat the steps in R.

The interactive maps below indicate that the extent of political participation varies substantively across European regions and across different forms of political participation (illustrated are voting, attending demonstrations, working for a political party or action group and boycotting products). When comparing the different forms of participation, it is obvious that voting is the most widely performed form of participation. While there is no clear pattern, Eastern European countries tend to have lower rates of participation.

{:refdef: style="text-align: center;"}
<p><iframe style="width:80%;"  height="1100" src="https://chodera.shinyapps.io/unequal-app/" frameborder="0" allowfullscreen></iframe></p>
{:refdef}

In my thesis, I further investigate these patterns of inequalities in political participation by considering differing levels of income inequality as potential explanation and conducting multilevel logistic regression analyses.

Get the full code [here](https://github.com/chodera/chodera.github.io/blob/master/assets/projects/8_unequal_political_participation_europe/unequal_participation.R){:target="_blank"}.
