# (A) Completed Foundational Coursework

<label for="tufte-mn-" class="margin-toggle">&#8853;</label><input type="checkbox" id="tufte-mn-" class="margin-toggle"><span class="marginnote"><emph>Note: '*' denotes courses applicable to both the major and minor areas.</emph></span>


```r
maj <-
    data.frame(
        CRN = c("`PSY 610`",
                "`PSY 614`",
                "`PSY 610`",
                "`PSY 597`",
                "`PSY 510`"),
        Title = c("_Top: Community \\& Social Interventions_",
                  "_Advanced Applied Social Psychology \\& Community Psychology_",
                  "_Top: Program Evaluation^`*`^_",
                  "_Applied Survey Research^`*`^_",
                  "_Top: Place-Based Research^`*`^_"),
        Grade = c("B+",
                  "A",
                  "A",
                  "A-",
                  "A"),
        Hrs = rep(4, 5))

knitr::kable(maj, caption = "**Major Area Coursework**: Applied
      Social \\& Community Psychology",
      col.names = c("CRN",
                    "Title",
                    "Grade",
                    "Credit Hours"),
      align = c('c', 'l', 'c', 'c'))
```



Table: (\#tab:maj)**Major Area Coursework**: Applied
      Social \& Community Psychology

    CRN      Title                                                           Grade    Credit Hours 
-----------  -------------------------------------------------------------  -------  --------------
 `PSY 610`   _Top: Community \& Social Interventions_                         B+           4       
 `PSY 614`   _Advanced Applied Social Psychology \& Community Psychology_      A           4       
 `PSY 610`   _Top: Program Evaluation^`*`^_                                    A           4       
 `PSY 597`   _Applied Survey Research^`*`^_                                   A-           4       
 `PSY 510`   _Top: Place-Based Research^`*`^_                                  A           4       


```r
minr <-
    data.frame(
        CRN = c("`SOC 610`",
                "`PSY 610`",
                "`SOC 610`",
                "`PSY 510`",
                "`SOC 610`",
                "`PSY 597`",
                "`PSY 510`"),
        Title = c("_Focus Groups_",
                  "_Program Evaluation^`*`^_",
                  "_Qualitative Data Analysis_",
                  "_HLM/Mixed Effects Models for Longitudinal Data Analysis_",
                  "_Mixed Methods Research_",
                  "_Applied Survey Research^`*`^_",
                  "_Place-Based Research^`*`^_"),
        Grade = c("A",
                  "A",
                  "A",
                  "A",
                  "A",
                  "A-",
                  "A"),
        Hrs = rep(4, 7))

knitr::kable(minr, caption = "**Minor Area Coursework**: Mixed Methods Research",
      col.names = c("CRN",
                    "Title",
                    "Grade",
                    "Credit Hours"),
      align = c('c', 'l', 'c', 'c'))
```



Table: (\#tab:minr)**Minor Area Coursework**: Mixed Methods Research

    CRN      Title                                                        Grade    Credit Hours 
-----------  ----------------------------------------------------------  -------  --------------
 `SOC 610`   _Focus Groups_                                                 A           4       
 `PSY 610`   _Program Evaluation^`*`^_                                      A           4       
 `SOC 610`   _Qualitative Data Analysis_                                    A           4       
 `PSY 510`   _HLM/Mixed Effects Models for Longitudinal Data Analysis_      A           4       
 `SOC 610`   _Mixed Methods Research_                                       A           4       
 `PSY 597`   _Applied Survey Research^`*`^_                                A-           4       
 `PSY 510`   _Place-Based Research^`*`^_                                    A           4       


```r
reqr <-
    data.frame(
        CRN = c("`PSY 621`",
                "`PSY 618`",
                "`PSY 622`",
                "`PSY 624`"),
        Title = c("_Univariate Quantitative Methods_",
                  "_Ethics \\& Professional Issues in Applied Research and Practice_",
                  "_Multiple Regression \\& Multivariate Quantitative Methods_",
                  "_Research Design in Applied Psychology_"),
        Grade = c("A-",
                  "A",
                  "B+",
                  "A"),
        Hrs = c(5,
                4,
                5,
                4))

knitr::kable(reqr, caption = "Research Methods \\& Statistics Coursework Required by Program",
      col.names = c("CRN",
                    "Title",
                    "Grade",
                    "Credit Hours"),
      align = c('c', 'l', 'c', 'c'))
```



Table: (\#tab:reqr)Research Methods \& Statistics Coursework Required by Program

    CRN      Title                                                               Grade    Credit Hours 
-----------  -----------------------------------------------------------------  -------  --------------
 `PSY 621`   _Univariate Quantitative Methods_                                    A-           5       
 `PSY 618`   _Ethics \& Professional Issues in Applied Research and Practice_      A           4       
 `PSY 622`   _Multiple Regression \& Multivariate Quantitative Methods_           B+           5       
 `PSY 624`   _Research Design in Applied Psychology_                               A           4       


```r
prac <-
    data.frame(
        CRN = c("`PSY 609`",
                "`PSY 609`",
                "`PSY 501`"),
        Title = c("_Prac: Violence Prevention I_",
                  "_Prac: Violence Prevention II_",
                  "_Res: BIP Staff Interviews_"),
        Grade = c("P",
                  "P",
                  "A"),
        Hrs = c(1,
                1,
                6))

knitr::kable(prac, caption = "Research \\& Field-Based Practica",
      col.names = c("CRN",
                    "Title",
                    "Grade",
                    "Credit Hours"),
      align = c('c', 'l', 'c', 'c'))
```



Table: (\#tab:prac)Research \& Field-Based Practica

    CRN      Title                             Grade    Credit Hours 
-----------  -------------------------------  -------  --------------
 `PSY 609`   _Prac: Violence Prevention I_       P           1       
 `PSY 609`   _Prac: Violence Prevention II_      P           1       
 `PSY 501`   _Res: BIP Staff Interviews_         A           6       
