---
title: "Removing_non_normaly_covariates_simulation"
author: "Xuelong Wang"
date: "2018-07-12"
output: 
  html_document: 
    fig_caption: yes
    fig_width: 20
    keep_md: yes
    number_sections: yes
    theme: united
    toc: yes
---





# Tranformation results 

The followings are the empirical density graphs of the covariates, which are selected based on their graphs shape. Those density graphs are not closed to a normal distribution



## Orignal scale

![](Subsetting_simulation_files/figure-html/original scale-1.png)<!-- -->

## After the rank tranformation

![](Subsetting_simulation_files/figure-html/rank-1.png)<!-- -->

## After the normal quantile transformation
![](Subsetting_simulation_files/figure-html/qunatile-1.png)<!-- -->


# Simulation result on the subset of PCB data

## rank tranformation

### On PCB (removing 7 variables)

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Table 1: MSE of estimated variance under rank transformation</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> GCTA_main </th>
   <th style="text-align:right;"> GCTA_interaction </th>
   <th style="text-align:right;"> pro_main </th>
   <th style="text-align:right;"> pro_interaction </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> MSE </td>
   <td style="text-align:right;"> 2.374025 </td>
   <td style="text-align:right;"> 1.781549 </td>
   <td style="text-align:right;"> 0.1450366 </td>
   <td style="text-align:right;"> 10.26997 </td>
  </tr>
</tbody>
</table>


![Figure 1: Box-plot of Main and Interaction variance with rank transformed data](Subsetting_simulation_files/figure-html/unnamed-chunk-2-1.png)

![Figure 2: Histogram of Main and Interaction variance with rank transformed data](Subsetting_simulation_files/figure-html/unnamed-chunk-3-1.png)

### On PCB (removing 11 variables)

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Table 2: MSE of estimated variance under rank transformation</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> GCTA_main </th>
   <th style="text-align:right;"> GCTA_interaction </th>
   <th style="text-align:right;"> pro_main </th>
   <th style="text-align:right;"> pro_interaction </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> MSE </td>
   <td style="text-align:right;"> 1.087262 </td>
   <td style="text-align:right;"> 1.894595 </td>
   <td style="text-align:right;"> 0.091419 </td>
   <td style="text-align:right;"> 6.977685 </td>
  </tr>
</tbody>
</table>


![Figure 3: Box-plot of Main and Interaction variance with rank transformed data](Subsetting_simulation_files/figure-html/unnamed-chunk-5-1.png)

![Figure 4: Histogram of Main and Interaction variance with rank transformed data](Subsetting_simulation_files/figure-html/unnamed-chunk-6-1.png)

## Normal quantile tranformation with subset

### On PCB (removing 7 variables)

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Table 3: MSE of estimated variance under quantile transformation</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> GCTA_main </th>
   <th style="text-align:right;"> GCTA_interaction </th>
   <th style="text-align:right;"> pro_main </th>
   <th style="text-align:right;"> pro_interaction </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> MSE </td>
   <td style="text-align:right;"> 4.129354 </td>
   <td style="text-align:right;"> 3.128731 </td>
   <td style="text-align:right;"> 1.318495 </td>
   <td style="text-align:right;"> 17.81728 </td>
  </tr>
</tbody>
</table>

![Figure 5: Box-plot of Main and Interaction variance with quantile transformed data](Subsetting_simulation_files/figure-html/unnamed-chunk-8-1.png)

![Figure 6: Histogram of Main and Interaction variance with quantile transformed data](Subsetting_simulation_files/figure-html/unnamed-chunk-9-1.png)

### On PCB (removing 11 variables)

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Table 4: MSE of estimated variance under quantile transformation</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> GCTA_main </th>
   <th style="text-align:right;"> GCTA_interaction </th>
   <th style="text-align:right;"> pro_main </th>
   <th style="text-align:right;"> pro_interaction </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> MSE </td>
   <td style="text-align:right;"> 1.341972 </td>
   <td style="text-align:right;"> 1.582012 </td>
   <td style="text-align:right;"> 0.5135139 </td>
   <td style="text-align:right;"> 13.8205 </td>
  </tr>
</tbody>
</table>

![Figure 7: Box-plot of Main and Interaction variance with quantile transformed data](Subsetting_simulation_files/figure-html/unnamed-chunk-11-1.png)

![Figure 8: Histogram of Main and Interaction variance with quantile transformed data](Subsetting_simulation_files/figure-html/unnamed-chunk-12-1.png)
