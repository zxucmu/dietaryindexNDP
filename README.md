# <span style="font-family: 'Arial'; font-size: 30pt;">Supplementary Material</span> 

### **Improving the dietaryindex R package: A proposal to include additional components for more accurate DII computation in NHANES.**

#### **Zhe Xu. Department of Thoracic Surgery, The First Hospital of China Medical University, Shenyang, Liaoning 110001, PR China. E-mail address: zxu@cmu.edu.cn**  

### **1.Supplementary Methods**

#### **1.1 Study population**

This cross-sectional analysis utilized data from the National Health and Nutrition Examination Survey (NHANES) cycles spanning 1999-2020. Methodological details have been published previously(1). The selection of specific survey cycles was determined by data availability requirements for distinct Dietary Inflammatory Index (DII) computation methods, as detailed in Section 1.2. Participants who were aged 18 years and older and not pregnant at baseline were included. Those with missing information on demographic information and socioeconomic factors were excluded from the analysis (Supplementary Figure 1). All participants provided written informed consent, and the NCHS Research Ethics Review Board approved the NHANES protocol.

#### **1.2 Calculation of dietary inflammatory index**

In the present study, the DII was calculated using three different approaches: (1) The first approach included 28 dietary components: alcohol, vitamins B6, B12, B1, A, B2, C, D, E, beta carotene, caffeine, carbohydrate, cholesterol, energy, total fat, dietary fiber, folic acid, iron, magnesium, monounsaturated fatty acids (MUFAs), niacin, n-3 fatty acids, n-6 fatty acids, protein, polyunsaturated fatty acids (PUFA), saturated fatty acids (SFAs), selenium, and zinc (the Sp-Flav DII). (2) The second approach incorporated the same 28 components as the first approach, with the addition of five spices and herbs: garlic, ginger, onion, pepper, and thyme/oregano (33 components total, the Sp+Flav- DII). (3) The third approach included all components from the second approach, plus six flavonoid subclasses: flavan-3-ols, flavones, flavonols, flavonones, anthocyanidins, and isoflavones (39 components total, the Sp+Flav+ DII).

#### **1.3 Outcome ascertainment**

The primary outcomes included five chronic conditions: hypertension, diabetes, cancer history, cardiovascular disease (CVD), and selected chronic respiratory conditions. Each condition was defined as follows: Hypertension: Meeting ≥1 criterion: (1) measured systolic/diastolic blood pressure (BP) ≥130/80 mmHg; (2) self-reported physician diagnosis; (3) current antihypertensive medication use; (4) self-reported history of hypertension; Diabetes: Defined by ≥1 criterion: (1) glycated hemoglobin A1c (HbA1c) ≥6.5%; (2) plasma fasting plasma glucose (PFG) ≥7.0 mmol/L; (3) oral glucose tolerance test (OGTT) 2-hour glucose ≥11.1 mmol/L; (4) self-reported diagnosis; (5) now taking prescribed medicine for diabetes; Cancer history: Self-reported physician-diagnosed malignancy; CVD: Self-reported history of congestive heart failure, coronary heart disease, angina, heart attack or stroke; Selected chronic respiratory conditions: Self-reported physician diagnosis of asthma, emphysema, or chronic bronchitis. The second outcome included C-reactive protein (CRP).

#### **1.4 Assessment of covariates**

The covariates in this study encompassed a range of factors: demographic characteristics including age, gender, and race/ethnicity; socioeconomic indicators comprising education level, marital status, and the poverty-income ratio (PIR), for which missing values of these variables were minimal and excluded from analysis without substantially affecting sample size; lifestyle factors such as alcohol use, smoking status, and leisure-time physical activity (LTPA); anthropometric measures specifically body mass index (BMI, kg/m²); and medical history/comorbidities including hypertension, diabetes, cancer, cardiovascular disease (CVD), and selected chronic respiratory conditions (emphysema, chronic bronchitis, or asthma). All covariates were treated as categorical variables, with missing values (for alcohol use, smoking status, LTPA, BMI, hypertension, diabetes, cancer, CVD, and selected chronic respiratory diseases) assigned to a separate category, for which missing values of these variables were more prevalent and thus assigned to a separate category to preserve sample size. Detailed categorization criteria are presented in Supplementary Table 1.

#### **1.5 Statistical analysis**

Continuous variables are presented as means with 95% confidence intervals (95% CI), while categorical variables are summarized as frequencies and percentages. Baseline characteristics were compared between participants included in analyses and those excluded due to missing data. Intergroup differences were assessed using Student's t-tests for continuous variables and c² tests for categorical variables. Pearson correlation analyses assessed the associations between the Sp-Flav- DII and both the Sp+Flav- DII and Sp+Flav+ DII, while paired t-tests compared mean scores of the Sp-Flav- DII against the Sp+Flav- DII and Sp+Flav+ DII. Sankey diagrams visualized dietary component flows during transitions from the Sp-Flav- DII to both the Sp+Flav- and Sp+Flav+ DII. To explore modified associations of DII with CRP and chronic diseases, we employed linear regression for CRP-DII relationships. For dichotomous disease outcomes, relative risks (RRs) were estimated using modified Poisson regression with robust variance estimation when outcome incidence exceeded 10%; otherwise, logistic regression was used to calculate odds ratios (ORs). These analyses were supplemented by interaction-term regression models that directly tested regression coefficient differences between the Sp-Flav- DII and both the Sp+Flav- DII and the Sp+Flav+ DII, using multiplicative interaction terms (DII ´ method term). For logistic or modified Poisson regression analyses, all outcomes were dichotomized (present/absent), with missing responses classified as absence of disease. Statistical significance was defined as ***P*** \< 0.05 using two-tailed tests. The statistical analyses were performed using R (version 4.3.1), with key packages detailed in Supplementary Table 2 (2-7).

### **2.Supplementary Results**

Supplementary Table 3 demonstrates significant differences in baseline characteristics between included and excluded participants across both NHANES periods (1999-2020 and 2007-2010/2017-2018), with excluded participants generally being older (higher proportion ≥80 years: 9.9% vs 6.3% in 1999-2020; 8.7% vs 6.5% in 2007-2010/2017-2018), more frequently from racial/ethnic minority groups (e.g., 14% Other Race/multiracial vs 9.4% in 1999-2020), having lower educational attainment (e.g., 31% less than high school vs 24% in 1999-2020), lower income (higher proportion with PIR\<1.3: 35% vs 30% in 1999-2020; 38% vs 31% in 2007-2010/2017-2018), and higher rates of missing data for health behaviors like alcohol use (76% vs 47% in 1999-2020) and smoking (7.0% vs 3.3% in 1999-2020). While excluded participants showed lower prevalence of several comorbidities such as hypertension (39% vs 50% in 1999-2020), diabetes (15% vs 17%), and cancer (8.9% vs 10%), they had significantly higher missing BMI data (30% vs 1.4% in 1999-2020), with all differences being statistically significant (***P***\<0.001) except for gender in 2007-2010/2017-2018 (***P*** =0.863) and asthma in both periods (***P*** =0.560 and ***P*** =0.143).

Supplementary Figure 2 illustrates significant correlations and distribution differences among three DII indices. Strong correlations were observed between the Sp-Flav- DII and the Sp+Flav- DII (r = 0.996, ***P*** \< 0.001), and between the Sp-Flav- DII and the Sp+Flav+ DII (r = 0.937, ***P*** \< 0.001). Both the Sp+Flav- DII and the Sp+Flav+ DII scores were significantly higher than the Sp-Flav- DII (***P*** \< 0.001). Transition analyses revealed substantial categorical shifts, particularly from the Sp-Flav- DII (0-3) to the Sp+Flav- DII (3-6) and the Sp+Flav+ DII (3-6). Density plots demonstrated altered distributions in both the comparison of the Sp-Flav- DII versus the Sp+Flav- DII, and the Sp-Flav- DII versus the Sp+Flav+ DII. Notably, neither the Sp+Flav- DII nor the Sp+Flav+ DII showed significantly modified associations with CRP levels compared to the Sp-Flav- DII. As shown in Supplementary Figure 3, neither the Sp+Flav- DII nor the Sp+Flav+ DII demonstrated significantly modified associations with ORs or RRs of chronic conditions compared to the Sp-Flav- DII.

### **3.Supplementary Discussion**

This comparative analysis of three DII frameworks revealed critical insights into the impact of expanding dietary components on inflammatory potential estimation. The exceptionally high correlation between the Sp-Flav- DII and the Sp+Flav- DII indicates that incorporating spices/herbs minimally alters relative inflammatory ranking, while the strong but attenuated correlation with the Sp+Flav+ DII suggests flavonoid inclusion introduces greater variability in scoring. Despite these correlations, the systematically higher absolute scores observed for both the Sp+Flav- and the Sp+Flav+ DII versus the Sp-Flav- DII (***P*** \< 0.001) demonstrate that expanding component lists consistently elevates pro-inflammatory estimates. This was further quantified through categorical transition analyses, where substantial shifts occurred from lower (0-3) to moderate (3-6) inflammatory categories when spices/herbs or flavonoids were incorporated, reflecting how additional components reclassify individuals' inflammatory status.

Notably, the modified indices did not significantly alter associations with clinical outcomes. Neither the Sp+Flav- nor the Sp+Flav+ DII demonstrated meaningfully different relationships with CRP levels or chronic disease risk (ORs/RRs) compared to the original Sp-Flav- DII. This suggests that while component expansions change absolute DII scoring, they do not materially enhance the index's capacity to predict inflammation-related health outcomes in this population. These null findings may reflect the dominance of core dietary components in driving inflammatory pathways, where spices/flavonoids—despite their putative anti-inflammatory properties—exert insufficient influence to modify population-level risk stratification.

Methodologically, the exclusion of participants with missing demographic/socioeconomic data introduced selection biases, as excluded individuals were disproportionately older, socioeconomically disadvantaged, and exhibited higher missingness in health behaviors. While our analytical approach (categorical "missing" groups for covariates) preserved statistical power, the lower comorbidity prevalence among excluded participants implies potential underestimation of true disease burden in high-risk subgroups. This limitation warrants caution in generalizing findings to vulnerable populations.

The restricted availability of flavonoid data (2007-2010/2017-2018 cycles) constrained the Sp+Flav+ DII analyses to smaller subsets, potentially limiting power to detect subtle association modifications. Furthermore, self-reported outcomes (e.g., cancer history) may be susceptible to misclassification bias. Nevertheless, this study provides the first comprehensive comparison of DII frameworks in a nationally representative sample, employing rigorous methods for missing data handling and outcome ascertainment. The robustness of the Sp-Flav- DII across expanded indices supports its continued utility in nutritional epidemiology, though future research should validate these findings in cohorts with biomarker-based dietary assessment and explore context-dependent effects of specific components (e.g., flavonoid subclasses) in diverse populations.

### **4.Supplementary References:**

1\.   CDC. About NHANES [Internet]. National Health and Nutrition Examination Survey. 2024 [cited 2025 Aug 10]. Available from: <https://www.cdc.gov/nchs/nhanes/about/index.html>

2\.   Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, et al. Welcome to the Tidyverse. Journal of Open Source Software 2019;4:1686.

3\.   Neal R, Cook I, Dunnington D, François R, Keane J, Moldovan-Grünfeld D, Ooms J, Wujciak-Jens J, Luraschi J, Werner KD, et al. arrow: Integration to "Apache" "Arrow" [Internet]. [cited 2025 Aug 12]. Available from: <https://github.com/apache/arrow/>

4\.   Patil I. Visualizations with statistical details: The "ggstatsplot" approach. Journal of Open Source Software 2021;6:3167.

5\.   Wickham H. ggplot2: Elegant Graphics for Data Analysis. Second Edition. Springer-Verlag New York; 2016.

6\.   Zeileis A, Köll S, Graham N. Various Versatile Variances: An Object-Oriented Implementation of Clustered Covariances in R. Journal of Statistical Software 2020;95:1-36.

7\.   Zeileis A. Object-oriented Computation of Sandwich Estimators. Journal of Statistical Software 2006;16:1-16.

### **Supplementary Table 1. Detailed categorization criteria for covariates**

| **Covariates**  | **Categorization criteria**                                                                                                                                         |
|:--------------------------|:----------------------------------------------|
| Age             | \<60 years; 60-79 years; ≥80 years                                                                                                                                  |
| Race/ethnicity  | Mexican American; Other Hispanic; Non-Hispanic White; Non-Hispanic Black; Other Race/multiracial                                                                    |
| Education level | Less than high School; High School Grad/GED or equivalent; More than high School                                                                                    |
| Marital status  | Married or living with partner; Widowed, divorced, or separated; Never married                                                                                      |
| PIR             | Low income: \<1.3; Middle income: 1.3-3.5; High income: ≥3.5                                                                                                        |
| Alcohol use     | Non-drinkers: \<12 drinks in lifetime or past year; Former drinkers: ≥12 lifetime drinks with no past-year consumption; Current drinkers: ≥12 drinks in past year   |
| Smoking status  | Non-smokers: \<100 lifetime cigarettes; Former smokers: ≥100 lifetime cigarettes, no current smoking; Current smokers: ≥100 lifetime cigarettes and current smoking |
| LTPA            | Inactive: 0 min/week; Insufficiently active: \<150 min/week; Sufficiently active: 150-300 min/week; Highly active: \>300 min/week                                   |
| BMI             | Normal range: \<18.5 kg/m²; Underweight: ≥18.5 and \<25.0 kg/m²; Overweight: ≥25.0 and \<30 kg/m²; Obesity: ≥30 kg/m²                                               |

Abbreviations: PIR = the poverty-income ratio; LTPA = leisure-time physical activity; BMI = body mass index.

### **Supplementary Table 2. R Packages Utilized for Statistical Analyses**

| **Package** | **Version** |
|:-------------|:-------------|
| tidyverse   | 2.0.0       |
| arrow       | 20.0.0      |
| labelled    | 2.12.0      |
| ggstatsplot | 0.13.0      |
| ggplot2     | 3.5.1       |
| ggpubr      | 0.6.0       |
| ggside      | 0.3.1       |
| ggsankey    | 0.0.99999   |
| ggridges    | 0.5.6       |
| broom       | 1.0.8       |
| sandwich    | 3.1-1       |
| lmtest      | 0.9-40      |

### **Supplementary Table 3. Baseline characteristics of included versus excluded participants**<sup>1</sup>

| Covariates                         | Included participants (n = 55,553) | Excluded participants (n = 19,705) | *P*-value | Included participants (n = 14,179) | Excluded participants (n = 4,252) | *P*-value |
|:----------------|:--------------------|:--------------------|:----------------|:--------------------|:--------------------|:----------------|
| **Age (yrs)**                      |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;< 60                              | 36,297 (65%)                       | 12,948 (66%)                       |                   | 9,130 (64%)                        | 2,924 (69%)                       |                   |
| &nbsp;&nbsp;60-79                              | 15,741 (28%)                       | 4,808 (24%)                        |                   | 4,129 (29%)                        | 957 (23%)                         |                   |
| &nbsp;&nbsp;≥ 80                               | 3,515 (6.3%)                       | 1,949 (9.9%)                       |                   | 920 (6.5%)                         | 371 (8.7%)                        |                   |
| **Gender**                         |                                    |                                    | 0.021             |                                    |                                   | 0.863             |
| &nbsp;&nbsp;Male                               | 27,396 (49%)                       | 9,528 (48%)                        |                   | 6,993 (49%)                        | 2,090 (49%)                       |                   |
| &nbsp;&nbsp;Female                             | 28,157 (51%)                       | 10,177 (52%)                       |                   | 7,186 (51%)                        | 2,162 (51%)                       |                   |
| **Race**                           |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;Non-Hispanic White                 | 25,429 (46%)                       | 7,333 (37%)                        |                   | 6,535 (46%)                        | 1,387 (33%)                       |                   |
| &nbsp;&nbsp;Non-Hispanic Black                 | 11,678 (21%)                       | 4,257 (22%)                        |                   | 2,882 (20%)                        | 915 (22%)                         |                   |
| &nbsp;&nbsp;Mexican American                   | 8,787 (16%)                        | 3,300 (17%)                        |                   | 2,220 (16%)                        | 844 (20%)                         |                   |
| &nbsp;&nbsp;Other Hispanic                     | 4,436 (8.0%)                       | 2,052 (10%)                        |                   | 1,339 (9.4%)                       | 558 (13%)                         |                   |
| &nbsp;&nbsp;Other Race/multiracial             | 5,223 (9.4%)                       | 2,763 (14%)                        |                   | 1,203 (8.5%)                       | 548 (13%)                         |                   |
| **Education level**                |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;Less Than high school              | 13,465 (24%)                       | 5,720 (31%)                        |                   | 3,618 (26%)                        | 1,400 (33%)                       |                   |
| &nbsp;&nbsp;High School Grad/GED or Equivalent | 13,038 (23%)                       | 4,703 (25%)                        |                   | 3,394 (24%)                        | 1,114 (26%)                       |                   |
| &nbsp;&nbsp;More than high school              | 29,050 (52%)                       | 8,314 (44%)                        |                   | 7,167 (51%)                        | 1,702 (40%)                       |                   |
| **Marital status**                 |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;Married or living with partner     | 32,070 (58%)                       | 8,710 (53%)                        |                   | 8,452 (60%)                        | 1,851 (55%)                       |                   |
| &nbsp;&nbsp;Widowed, divorced, or separated    | 12,355 (22%)                       | 4,203 (25%)                        |                   | 3,317 (23%)                        | 848 (25%)                         |                   |
| &nbsp;&nbsp;Never married                      | 11,128 (20%)                       | 3,578 (22%)                        |                   | 2,410 (17%)                        | 651 (19%)                         |                   |
| **PIR**                            |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;< 1.3                             | 16,680 (30%)                       | 3,965 (35%)                        |                   | 4,357 (31%)                        | 838 (38%)                         |                   |
| &nbsp;&nbsp;1.3 - 3.5                          | 21,214 (38%)                       | 4,177 (37%)                        |                   | 5,554 (39%)                        | 846 (39%)                         |                   |
| &nbsp;&nbsp;≥ 3.5                              | 17,659 (32%)                       | 3,090 (28%)                        |                   | 4,268 (30%)                        | 509 (23%)                         |                   |
| **Alcohol use**                    |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;None smokers                       | 5,138 (9.2%)                       | 1,323 (6.7%)                       |                   | 1,243 (8.8%)                       | 200 (4.7%)                        |                   |
| &nbsp;&nbsp;Former smokers                     | 6,926 (12%)                        | 1,094 (5.6%)                       |                   | 1,757 (12%)                        | 212 (5.0%)                        |                   |
| &nbsp;&nbsp;Current smokers                    | 17,433 (31%)                       | 2,254 (11%)                        |                   | 4,516 (32%)                        | 421 (9.9%)                        |                   |
| &nbsp;&nbsp;Missing                            | 26,056 (47%)                       | 15,034 (76%)                       |                   | 6,663 (47%)                        | 3,419 (80%)                       |                   |
| **Smoking status**                 |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;Current smokers                    | 10,912 (20%)                       | 3,316 (17%)                        |                   | 2,972 (21%)                        | 693 (16%)                         |                   |
| &nbsp;&nbsp;Former smokers                     | 13,654 (25%)                       | 3,896 (20%)                        |                   | 3,585 (25%)                        | 730 (17%)                         |                   |
| &nbsp;&nbsp;Non-smokers                        | 29,163 (52%)                       | 11,113 (56%)                       |                   | 7,620 (54%)                        | 2,222 (52%)                       |                   |
| &nbsp;&nbsp;Missing                            | 1,824 (3.3%)                       | 1,380 (7.0%)                       |                   | 2 (\<0.1%)                         | 607 (14%)                         |                   |
| **LTPA**                           |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;Inactive                           | 17,943 (32%)                       | 6,138 (31%)                        |                   | 7,731 (55%)                        | 2,384 (56%)                       |                   |
| &nbsp;&nbsp;Insufficiently active              | 10,490 (19%)                       | 2,664 (14%)                        |                   | 2,029 (14%)                        | 507 (12%)                         |                   |
| &nbsp;&nbsp;Sufficiently active                | 6,788 (12%)                        | 1,981 (10%)                        |                   | 1,740 (12%)                        | 408 (9.6%)                        |                   |
| &nbsp;&nbsp;Highly active                      | 10,587 (19%)                       | 4,346 (22%)                        |                   | 2,662 (19%)                        | 942 (22%)                         |                   |
| &nbsp;&nbsp;Missing                            | 9,745 (18%)                        | 4,576 (23%)                        |                   | 17 (0.1%)                          | 11 (0.3%)                         |                   |
| **BMI**                            |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;Normal range                       | 15,327 (28%)                       | 4,494 (23%)                        |                   | 3,645 (26%)                        | 1,099 (26%)                       |                   |
| &nbsp;&nbsp;Obesity                            | 20,544 (37%)                       | 4,509 (23%)                        |                   | 5,488(39%)                         | 1,111 (26%)                       |                   |
| &nbsp;&nbsp;Overweight                         | 17,990 (32%)                       | 4,393 (22%)                        |                   | 4,663 (33%)                        | 1,102 (26%)                       |                   |
| &nbsp;&nbsp;Underweight                        | 887 (1.6%)                         | 400 (2.0%)                         |                   | 222 (1.6%)                         | 94 (2.2%)                         |                   |
| &nbsp;&nbsp;Missing                            | 805 (1.4%)                         | 5,909 (30%)                        |                   | 161 (1.1%)                         | 846 (20%)                         |                   |
| **Hypertension**                   |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;Yes                                | 28,005 (50%)                       | 7,763 (39%)                        |                   | 7,825 (55%)                        | 1,768 (42%)                       |                   |
| **Diabetes**                       |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;Yes                                | 9,620 (17%)                        | 2,926 (15%)                        |                   | 2,744 (19%)                        | 659 (15%)                         |                   |
| **Cancer**                         |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;Yes                                | 5,556 (10%)                        | 1,758 (8.9%)                       |                   | 1,475 (10%)                        | 306 (7.2%)                        |                   |
| **CHF**                            |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;Yes                                | 1,865 (3.4%)                       | 743 (3.8%)                         |                   | 463 (3.3%)                         | 128 (3.0%)                        |                   |
| **CHD**                            |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;Yes                                | 2,386 (4.3%)                       | 788 (4.0%)                         |                   | 619 (4.4%)                         | 153 (3.6%)                        |                   |
| **Angina**                         |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;Yes                                | 1,555 (2.8%)                       | 487 (2.5%)                         |                   | 377 (2.7%)                         | 102 (2.4%)                        |                   |
| **Heart attack**                   |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;Yes                                | 2,410 (4.3%)                       | 817 (4.1%)                         |                   | 651 (4.6%)                         | 161 (3.8%)                        |                   |
| **Stroke**                         |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;Yes                                | 2,226 (4.0%)                       | 823 (4.2%)                         |                   | 609 (4.3%)                         | 149 (3.5%)                        |                   |
| **Asthma**                         |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;Yes                                | 7,910 (14%)                        | 2,843 (14%)                        |                   | 1,996 (14%)                        | 598 (14%)                         |                   |
| **Emphysema**                      |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;Yes                                | 913 (1.6%)                         | 220 (1.1%)                         |                   | 331 (2.3%)                         | 76 (1.8%)                         |                   |
| **Chronic bronchitis**             |                                    |                                    | \<0.001           |                                    |                                   | \<0.001           |
| &nbsp;&nbsp;Yes                                | 2,576 (4.6%)                       | 579 (2.9%)                         |                   | 879 (6.2%)                         | 197 (4.6%)                        |                   |

Abbreviation: NHANES = the US National Health and Nutrition Examination Survey; PIR = the poverty-income ratio; LTPA = leisure-time physical activity; BMI = body mass index; CHF = chronic heart failure; CHD = coronary heart disease.
<sup>1</sup>Aged 18 years and older and not pregnant
Categorical variables were expressed as number (percentage). P values were calculated using analysis of Pearson's Chi-squared test for categorical variables.

### **Supplementary Figure 1. Flowchart of the study**

![Abbreviations: DII = dietary inflammatory index; Sp-Flav- = Base DII (28 components); Sp-Flav+ = Spice-enhanced DII (33 components); Sp+Flav+ = Spice and Flavonoid-enhanced DII (39 components).](Figure%201.jpg)
Abbreviations: DII = dietary inflammatory index; Sp-Flav- = Base DII (28 components); Sp-Flav+ = Spice-enhanced DII (33 components); Sp+Flav+ = Spice and Flavonoid-enhanced DII (39 components).

### **Supplementary Figure 2. Comparative analysis of three DII indices: correlations, distribution shifts, and inflammatory biomarker associations**

![Abbreviations: DII = dietary inflammatory index; Sp-Flav- = Base DII (28 components); Sp-Flav+ = Spice-enhanced DII (33 components); Sp+Flav+ = Spice and Flavonoid-enhanced DII (39 components); CRP = C-reactive protein. A. Pairwise correlations: the Sp-Flav- DII vs. the Sp+Flav- DII and the Sp-Flav- DII vs. the Sp+Flav+ DII; B. Distribution differences: the Sp-Flav- DII vs. the Sp+Flav- DII and the Sp-Flav- DII vs. the Sp+Flav+ DII; C. Transition analyses between the Sp-Flav- DII vs. the Sp+Flav- DII and the Sp-Flav- DII vs. the Sp+Flav+ DII; D. Altered distributions: Sp-Flav- DII vs. Sp+Flav- DII and Sp-Flav- DII vs. Sp+Flav+ DII; E. Modified associations of the Sp+Flav- DII and the Sp+Flav+ DII with CRP levels compared to the Sp-Flav- DII.](Figure%202.jpg)
Abbreviations: DII = dietary inflammatory index; Sp-Flav- = Base DII (28 components); Sp-Flav+ = Spice-enhanced DII (33 components); Sp+Flav+ = Spice and Flavonoid-enhanced DII (39 components); CRP = C-reactive protein. A. Pairwise correlations: the Sp-Flav- DII vs. the Sp+Flav- DII and the Sp-Flav- DII vs. the Sp+Flav+ DII; B. Distribution differences: the Sp-Flav- DII vs. the Sp+Flav- DII and the Sp-Flav- DII vs. the Sp+Flav+ DII; C. Transition analyses between the Sp-Flav- DII vs. the Sp+Flav- DII and the Sp-Flav- DII vs. the Sp+Flav+ DII; D. Altered distributions: Sp-Flav- DII vs. Sp+Flav- DII and Sp-Flav- DII vs. Sp+Flav+ DII; E. Modified associations of the Sp+Flav- DII and the Sp+Flav+ DII with CRP levels compared to the Sp-Flav- DII.

### **Supplementary Figure 3. Comparison of chronic disease risk associations across three DII indices**

![Abbreviations: DII = dietary inflammatory index; Sp-Flav- = Base DII (28 components); Sp-Flav+ = Spice-enhanced DII (33 components); Sp+Flav+ = Spice and Flavonoid-enhanced DII (39 components); CVD = cardiovascular disease; ECA = selected chronic respiratory conditions (emphysema, chronic bronchitis, or asthma).](Figure%203.jpg)
Abbreviations: DII = dietary inflammatory index; Sp-Flav- = Base DII (28 components); Sp-Flav+ = Spice-enhanced DII (33 components); Sp+Flav+ = Spice and Flavonoid-enhanced DII (39 components); CVD = cardiovascular disease; ECA = selected chronic respiratory conditions (emphysema, chronic bronchitis, or asthma
