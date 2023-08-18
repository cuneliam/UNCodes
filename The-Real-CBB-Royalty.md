The Real CBB Royalty
================

``` r
summary(data$ENROLL)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    3341   12938   17718   18573   24148   42017

``` r
summary(data$WPCTBB)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.372   0.518   0.561   0.561   0.593   0.764

``` r
summary(data$REVENUEBB)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##  1349628  2976173  7465979  9316492 12102078 43134625

``` r
data2 <- subset(data, select = c("ENROLL", "WPCTBB", "REVENUEBB"))
cormat <- rcorr(as.matrix(data2), type = "pearson")
View(cormat$r)
View(cormat$P)
```

``` r
std_data <- scale(data2)
max(std_data[,1])
```

    ## [1] 2.805913

``` r
max(std_data[,2])
```

    ## [1] 2.933747

``` r
max(std_data[,3])
```

    ## [1] 4.262746

``` r
data3 <- data.frame(data$SCHOOL, std_data)
```

``` r
data4 <- data3[,-1]
rownames(data4) <- data3$data.SCHOOL
k <- kmeans(data4, centers = 3, nstart = 25)
fviz_cluster(k, data = data4)
```

![](The-Real-CBB-Royalty_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Finding Eucidian Distances

``` r
# Add the cluster assignments to the original dataframe
data4$Cluster <- k$cluster

# Calculate the cluster centroids
cluster_centroids <- k$centers

# Calculate the Euclidean distance from each data point to its corresponding cluster centroid
distances_to_centroids <- sqrt(rowSums((data4 - cluster_centroids[data4$Cluster, ])^2))

# Add the distances to the original dataframe
data4$DistanceToCentroid <- as.vector(distances_to_centroids)

# Print the updated dataframe with the distances to centroids
print(data4)
```

    ##                           ENROLL        WPCTBB   REVENUEBB Cluster
    ## Boston College      -1.104416151 -0.0865887730 -0.28771277       1
    ## Clemson             -0.244603297 -0.8814139923  0.24750358       1
    ## Duke                -1.448389166  2.1100191059  3.41539352       2
    ## Florida State        1.239962215  0.2024385795  1.00692177       3
    ## Georgia Tech        -0.649016943 -0.4912270664 -0.20425891       1
    ## Louisville          -0.743088895  1.5030616657  4.26274648       2
    ## Miami (FL)          -0.990596270  0.0868276385 -0.08991571       1
    ## NC State             0.324376423  0.8816528578  0.85513542       3
    ## North Carolina      -0.125277933  2.5291087669  1.93406850       2
    ## Pittsburgh          -0.113189807  0.3614036234  0.35112076       3
    ## Syracuse            -0.519877457  1.9076999591  2.83057797       2
    ## Virginia            -0.434901520  0.1301817414  0.52329531       1
    ## Virginia Tech        0.608028492 -0.3033592873  0.51603261       3
    ## Wake Forest         -1.648022774  0.0001194328 -0.08315847       1
    ## Notre Dame          -1.216321081  1.2718397837 -0.72334690       1
    ## Illinois             1.524691444  1.2573884161  1.99339629       3
    ## Indiana              1.506619097  1.1128747398  1.98783866       3
    ## Iowa                 0.096018555  0.3903063586  0.19057563       3
    ## Maryland             0.758232435  0.4192090939  0.61019888       3
    ## Michigan             1.044158508  0.2891467853  1.35012268       3
    ## Michigan State       1.970635582  0.6070768730  1.58388723       3
    ## Minnesota            1.121474642  0.1157303738  1.02581552       3
    ## Nebraska             0.010444196 -0.4912270664  1.26350373       3
    ## Northwestern        -1.222664355 -2.1675857108  1.29146925       1
    ## Ohio State           2.618607018  0.6937850787  1.85794906       3
    ## Penn State           2.454040944 -0.0721374053  0.20585846       3
    ## Purdue               1.174016101  0.9683610636  0.07244427       3
    ## Rutgers              1.639588484 -0.7513516836 -0.12388444       3
    ## Wisconsin            1.112378626  0.1157303738  1.65405500       3
    ## Baylor              -0.524784518 -1.0548304037 -0.01650973       1
    ## Iowa State           1.032429435 -0.8091571541  0.58484926       3
    ## Kansas              -0.188830358  2.3267896202  1.19566195       2
    ## Kansas State        -0.073813633  0.3758549910 -0.02934040       1
    ## Oklahoma            -0.103136316  0.6937850787  0.52399564       3
    ## Oklahoma State      -0.054065705  0.4336604615  0.39354751       3
    ## TCU                 -1.226374572 -1.4594686972  0.87268713       1
    ## Texas                2.094389270  0.9683610636  1.15699473       3
    ## Texas Tech           0.832795827 -0.1443942434  0.07219886       3
    ## West Virginia        0.274108968  0.7804932845 -0.25644540       3
    ## Arizona              1.288913142  1.3296452542  1.70343677       3
    ## Arizona State        2.058244576 -0.4767756988 -0.08745851       3
    ## California           0.927226833  0.1735358443 -0.15520046       3
    ## Colorado             0.667272278 -0.6357407427 -0.01406602       3
    ## Oregon               0.009726090 -0.2889079197  0.26063135       1
    ## Oregon State        -0.068308150  0.1012790062 -0.29823699       1
    ## Stanford            -1.382921790  0.1735358443 -0.25991314       1
    ## UCLA                 1.251212550  1.8932485915  0.25328117       3
    ## USC                 -0.080755330  0.1012790062 -0.42289063       1
    ## Utah                -0.198764165  1.1995829456  0.05525571       3
    ## Washington           1.096340914  0.6215282406  0.11469305       3
    ## Washington State     0.263098001 -0.6212893750 -0.44872529       1
    ## Alabama              1.048227779  0.8382987550  0.75406582       3
    ## Arkansas             0.054607746  1.1417774751  1.00328765       3
    ## Auburn               0.020737056 -0.5056784340  0.23930081       1
    ## Florida              1.317038980 -0.0432346701  0.27025192       3
    ## Georgia              0.800241666 -0.6501921103  0.11797272       3
    ## Kentucky             0.232219422  2.9337470604  2.70083144       2
    ## Louisiana State      0.540526480  0.1012790062 -0.05101115       3
    ## Mississippi         -0.257888267 -0.9247680951  0.15828664       1
    ## Mississippi State   -0.480142230 -0.2889079197 -0.28365689       1
    ## Missouri             0.859365768  0.4625631967  0.56173765       3
    ## South Carolina       0.524129715 -0.4912270664  0.22641405       3
    ## Tennessee            0.192125142  0.7082364464  0.83976990       3
    ## Texas A&M            2.805913131 -0.5201298017 -0.01145680       3
    ## Vanderbilt          -1.412005103  0.3614036234  0.05331657       1
    ## Cincinnati           0.239998909  1.0550692693 -0.23325551       3
    ## Houston              0.601086796  0.4192090939 -0.50439363       3
    ## Memphis             -0.747995956  1.0261665341  0.32054402       1
    ## SMU                 -1.482140171 -0.6501921103 -0.22927715       1
    ## South Florida        0.581817604 -1.3727604915 -0.57774565       1
    ## Temple               0.743750621  1.1851315779 -0.35085481       3
    ## Tulane              -1.479148061 -0.9536708304 -0.62392653       1
    ## Tulsa               -1.823001392 -0.0721374053 -0.42314072       1
    ## UCF                  2.130414280 -0.2311024492 -0.74979150       3
    ## Idaho               -1.312427668 -1.2860522857 -0.95802480       1
    ## East Carolina        0.026721277 -1.0114763009 -0.76634339       1
    ## FIU                  0.809936104 -2.3988075927 -0.94117355       1
    ## Florida Atlantic    -0.406296945 -2.7311890481 -0.87698605       1
    ## Louisiana Tech      -1.438215990  0.0868276385 -0.86505739       1
    ## Marshall            -1.259168102  0.1735358443 -0.79863781       1
    ## Middle Tennessee    -0.239576551 -0.4478729635 -0.76302225       1
    ## North Texas          0.621074093 -1.3005036533 -0.76136660       1
    ## Rice                -1.762919814 -1.8063015202 -0.76369573       1
    ## Southern Miss       -0.957084633 -0.1877483463 -0.94983136       1
    ## Texas at El Paso    -0.666730237  0.1590844767 -0.55805552       1
    ## UAB                 -1.215602974  0.8672014902 -0.72561818       1
    ## UTSA                 0.181114175 -0.7079975808 -0.89930317       1
    ## New Mexico State    -0.957802739  0.1879872119 -0.86110121       1
    ## Akron               -0.418265386  0.6937850787 -0.80447679       1
    ## Ball State          -0.438013315 -0.6646434779 -0.92295529       1
    ## Bowling Green       -0.678818363 -0.2889079197 -0.89303462       1
    ## Buffalo             -0.069624679 -0.7513516836 -0.83991134       1
    ## Central Michigan    -0.085303337 -0.7513516836 -0.90975933       1
    ## Eastern Michigan    -0.674390039 -0.9825735656 -0.92399683       1
    ## Kent State          -0.004037618 -0.9536708304 -0.89349849       1
    ## Miami               -0.424129923 -0.2889079197 -0.87182799       1
    ## Northern Illinois   -0.611076983 -0.9970249332 -0.93433878       1
    ## Ohio                -0.189907518  0.1446331090 -0.80558211       1
    ## Toledo              -0.702994615  0.1590844767 -0.85366141       1
    ## Western Michigan    -0.358064124 -0.4478729635 -0.87884716       1
    ## Boise State         -0.771813155 -0.1010401406 -0.64692785       1
    ## Colorado State       0.186619659 -0.8814139923 -0.37592233       1
    ## Fresno State        -0.102298525 -0.3611647578 -0.75175346       1
    ## Hawaii              -0.849009604 -1.4305659620 -0.83172773       1
    ## Nevada              -0.564519744 -0.5923866398 -0.57609983       1
    ## New Mexico          -0.293434539  0.2313413148 -0.61677601       1
    ## San Diego State      0.753923796  0.0290221680 -0.19403456       3
    ## San Jose State       0.323778001 -1.4883714325 -0.84522784       1
    ## UNLV                -0.147539234  2.1244704735 -0.44791328       3
    ## Utah State          -0.340470515  0.4336604615 -0.73924762       1
    ## Wyoming             -1.232837530  0.1157303738 -0.76183651       1
    ## Appalachian State   -0.142632173 -0.4623243312 -0.95834459       1
    ## Arkansas State      -0.537112013 -0.8236085218 -0.97398035       1
    ## Georgia Southern    -0.332691028 -0.0287833025 -0.96358231       1
    ## Georgia State        0.031867707 -2.5866753719 -0.85221059       1
    ## Louisiana-Lafayette -0.728367711 -0.0287833025 -0.83178092       1
    ## Louisiana-Monroe    -1.643355081 -0.4478729635 -1.00421626       1
    ## South Alabama       -1.136252206  0.0434735357 -0.90278137       1
    ## Texas State          0.916933973 -0.4767756988 -0.92964131       1
    ## Troy                -1.144031693 -0.1443942434 -0.90004523       1
    ## Western Kentucky    -0.661942860  1.4886102980 -0.79919167       1
    ##                     DistanceToCentroid
    ## Boston College                1.717083
    ## Clemson                       1.842048
    ## Duke                          2.745131
    ## Florida State                 2.144091
    ## Georgia Tech                  1.609980
    ## Louisville                    3.006294
    ## Miami (FL)                    1.771033
    ## NC State                      2.227833
    ## North Carolina                2.629440
    ## Pittsburgh                    2.313958
    ## Syracuse                      2.487953
    ## Virginia                      1.990865
    ## Virginia Tech                 2.194470
    ## Wake Forest                   2.010598
    ## Notre Dame                    2.422288
    ## Illinois                      2.745566
    ## Indiana                       2.696988
    ## Iowa                          2.241003
    ## Maryland                      2.063234
    ## Michigan                      2.229084
    ## Michigan State                2.547867
    ## Minnesota                     2.145659
    ## Nebraska                      2.545583
    ## Northwestern                  3.038850
    ## Ohio State                    2.992359
    ## Penn State                    2.602116
    ## Purdue                        2.180097
    ## Rutgers                       2.525806
    ## Wisconsin                     2.381338
    ## Baylor                        1.766335
    ## Iowa State                    2.383475
    ## Kansas                        2.915582
    ## Kansas State                  1.912480
    ## Oklahoma                      2.324020
    ## Oklahoma State                2.285213
    ## TCU                           2.436976
    ## Texas                         2.507492
    ## Texas Tech                    2.164993
    ## West Virginia                 2.315474
    ## Arizona                       2.581274
    ## Arizona State                 2.556037
    ## California                    2.161170
    ## Colorado                      2.368082
    ## Oregon                        1.865590
    ## Oregon State                  1.755137
    ## Stanford                      1.895675
    ## UCLA                          2.566007
    ## USC                           1.738272
    ## Utah                          2.520295
    ## Washington                    2.101411
    ## Washington State              1.789158
    ## Alabama                       2.116253
    ## Arkansas                      2.413009
    ## Auburn                        1.854162
    ## Florida                       2.140906
    ## Georgia                       2.337807
    ## Kentucky                      2.660447
    ## Louisiana State               2.179875
    ## Mississippi                   1.813937
    ## Mississippi State             1.602719
    ## Missouri                      2.054916
    ## South Carolina                2.290645
    ## Tennessee                     2.235547
    ## Texas A&M                     2.957459
    ## Vanderbilt                    2.046509
    ## Cincinnati                    2.378980
    ## Houston                       2.304873
    ## Memphis                       2.328112
    ## SMU                           1.854734
    ## South Florida                 2.156934
    ## Temple                        2.360036
    ## Tulane                        1.885449
    ## Tulsa                         2.047155
    ## UCF                           2.744106
    ## Idaho                         1.972311
    ## East Carolina                 1.785968
    ## FIU                           2.887475
    ## Florida Atlantic              2.796588
    ## Louisiana Tech                1.898620
    ## Marshall                      1.840732
    ## Middle Tennessee              1.619286
    ## North Texas                   2.158848
    ## Rice                          2.404942
    ## Southern Miss                 1.686613
    ## Texas at El Paso              1.685950
    ## UAB                           2.154151
    ## UTSA                          1.795291
    ## New Mexico State              1.765614
    ## Akron                         1.963655
    ## Ball State                    1.634703
    ## Bowling Green                 1.619338
    ## Buffalo                       1.701570
    ## Central Michigan              1.710447
    ## Eastern Michigan              1.704708
    ## Kent State                    1.778704
    ## Miami                         1.617925
    ## Northern Illinois             1.708937
    ## Ohio                          1.739879
    ## Toledo                        1.715934
    ## Western Michigan              1.618981
    ## Boise State                   1.623593
    ## Colorado State                1.804893
    ## Fresno State                  1.653581
    ## Hawaii                        1.895454
    ## Nevada                        1.577416
    ## New Mexico                    1.734161
    ## San Diego State               2.201340
    ## San Jose State                2.107040
    ## UNLV                          3.043401
    ## Utah State                    1.825031
    ## Wyoming                       1.807545
    ## Appalachian State             1.678768
    ## Arkansas State                1.670733
    ## Georgia Southern              1.694247
    ## Georgia State                 2.738898
    ## Louisiana-Lafayette           1.657281
    ## Louisiana-Monroe              1.956039
    ## South Alabama                 1.775628
    ## Texas State                   2.196662
    ## Troy                          1.735202
    ## Western Kentucky              2.506612

``` r
data4 %>% group_by(Cluster) %>% summarise(max(DistanceToCentroid), min(DistanceToCentroid))
```

    ## # A tibble: 3 × 3
    ##   Cluster `max(DistanceToCentroid)` `min(DistanceToCentroid)`
    ##     <int>                     <dbl>                     <dbl>
    ## 1       1                      3.04                      1.58
    ## 2       2                      3.01                      2.49
    ## 3       3                      3.04                      2.05

Anova for clusters

``` r
data4 %>% group_by(Cluster) %>% summarise(mean(DistanceToCentroid^2))
```

    ## # A tibble: 3 × 2
    ##   Cluster `mean(DistanceToCentroid^2)`
    ##     <int>                        <dbl>
    ## 1       1                         3.75
    ## 2       2                         7.54
    ## 3       3                         5.75

``` r
191.4884 / 69.63
```

    ## [1] 2.750085

``` r
191.4884 / 90.10
```

    ## [1] 2.125287

``` r
191.4884 / 8.78
```

    ## [1] 21.80961

Mean z-scores by cluster

``` r
data4 %>% group_by(Cluster) %>% summarise(mean(ENROLL), mean(WPCTBB), mean(REVENUEBB))
```

    ## # A tibble: 3 × 4
    ##   Cluster `mean(ENROLL)` `mean(WPCTBB)` `mean(REVENUEBB)`
    ##     <int>          <dbl>          <dbl>             <dbl>
    ## 1       1         -0.570         -0.447            -0.547
    ## 2       2         -0.466          2.22              2.72 
    ## 3       3          0.949          0.399             0.488
