--UPDATE `freecode` SET `name`="RCRD-PLC" WHERE `id`=77;
--UPDATE `freecode` SET `name`="INTV-PROP" WHERE `id`=19;

--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=11 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=19 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=22 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=32 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=36 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=51 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=58 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=63 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=64 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=65 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=72 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=79 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=80 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=82 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=84 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=48 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=95 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=96 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=100 AND `variable`="RMV2";
--UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=101 AND `variable`="RMV2";

-- [CHECK CHANGES]:
-- SELECT `variable`,`value`,`caseID` FROM `caseAttr` WHERE `value`="1"; --[x]
-- SELECT `variable`,`value`,`caseID` FROM `caseAttr` WHERE `value`="0"; --[x]

-- UPDATE `caseAttr` SET `name`="RMV" WHERE `name`="RMV2";
-- UPDATE `caseAttr` SET `variable`="RMV" WHERE `variable`="RMV2";

-- UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=39 AND `variable`="RMV";
-- UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=40 AND `variable`="RMV";

-- SELECT `name`,`id` FROM `cases` WHERE `id`=4 OR `id`=41;
-- SELECT `variable`,`value`,`caseID` FROM `caseAttr` WHERE `caseID`=4 OR `caseID`=41;
-- UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=41 AND `variable`="RMV";
-- UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=4 AND `variable`="RMV";
-- SELECT `variable`,`value`,`caseID` FROM `caseAttr` WHERE `caseID`=4 OR `caseID`=41; --[x]

-- INSERT INTO `coding` (`cid`,`fid`,`selfirst`,`selend`) VALUES (76,1,97502.0,99907.0);
-- UPDATE `coding` SET `seltext` = "@article{oswald2010lesbian,
--     author={Oswald,Ramona F. and Fonseca,Carol A. and Hardesty,Jennifer L.},
--     year={2010},
--     title={Lesbian mothers' counseling experiences in the context of intimate partner violence},
--     journal={Psychology of Women Quarterly},
--     volume={34},
--     number={3},
--     pages={286--296},
--     abstract={Intimate partner violence (IPV) is a significant concern for some lesbian households with children. Yet we know of only one study that has examined lesbian mothers' experiences with IPV. In the current study we analyzed the counseling experiences of participants in our prior study. Interviews with 24 lesbian mothers (12 Black, 9 White, and 3 Latina) 23 to 54 years of age ( M = 39.5) were coded using thematic analysis. Overall, lesbian mothers experiencing IPV did seek help from counselors ( n = 15, 63%), typically after reaching a breaking point. Counselors were most helpful when addressing the abuse and promoting self-empowerment, and least helpful when victim-blaming or ignoring the abuse and/or the same-sex relationship. Lesbian mothers' perceptions that mental health professionals were sometimes ineffective have implications for provider training. In order to work effectively with this population, providers should attempt to eliminate or correct personal biases or prejudices with self-exploration and education. By becoming more aware and knowledgeable of the nuances, struggles, and strengths of the lesbian community, providers can gain competency in providing therapeutic services to such clients. Mental health professionals can also adopt an advocacy stance to assist in spreading cultural awareness to others and support policy or institutional changes to include same-sex IPV. Competencies can be assessed through future studies that identify the knowledge and skills gap among mental health professionals who frequently work with the lesbian population. (PsycINFO Database Record (c) 2016 APA, all rights reserved) (Source: journal abstract)},
--     keywords={lesbian mothers; counseling experiences; intimate partner violence; Empirical Study; Interview; Quantitative Study; Human; Female; Adulthood (18 yrs & older); Young Adulthood (18-29 yrs); Thirties (30-39 yrs); Middle Age (40-64 yrs); Lesbianism; Partner Abuse; Mothers; Counseling; Violence; US; article; 3236:Criminal Behavior & Juvenile Delinquency; 2970:Sex Roles & Women's Issues},
-- }" WHERE `selfirst` = 97502.0 AND `seltext` = NULL;

-- UPDATE `coding` SET `cid` = 129 WHERE `selfirst` = 97502.0 AND `cid` = 76;

-- UPDATE `coding` SET  `owner` = "rachel97" WHERE `owner` IS NULL;
-- UPDATE `coding` SET  `date` = "Sat May 13 19:38:20 2017" WHERE `date` IS NULL;

-- INSERT INTO `coding` (`cid`,`fid`,`selfirst`,`selend`,`status`) VALUES (102,1,27051.0, 29855.0,1);
-- INSERT INTO `coding` (`cid`,`fid`,`selfirst`,`selend`,`status`) VALUES (105,1,27051.0, 29855.0,1);

-- INSERT INTO `coding` (`cid`,`fid`,`selfirst`,`selend`,`status`) VALUES (102,1,29857.0, 31667.0,1); -- potter2011bringing;
-- INSERT INTO `coding` (`cid`,`fid`,`selfirst`,`selend`,`status`) VALUES (112,1,29857.0, 31667.0,1); -- potter2011bringing;

-- UPDATE `caseAttr` SET `value`="1" WHERE `caseID`=77 AND `variable`="RMV"; -- mustanski2014syndemic
-- INSERT INTO `coding` (`cid`,`fid`,`selfirst`,`selend`,`status`) VALUES (180,1,147290.0,148986.0,1); -- mustanski2014syndemic;
-- UPDATE `coding` SET `seltext` = "@article{mustanski2014syndemic,
-- author={Mustanski, Brian and Andrews, Rebecca and Herrick, Amy and Stall, Ron and Schnarrs, Phillip W.},
-- title={A Syndemic of Psychosocial Health Disparities and Associations With Risk for Attempting Suicide Among Young Sexual Minority Men},
-- Journal={American Journal Of Public Health},
-- year={2014},
-- volume={104},
-- number={2},
-- pages={287--294},
-- abstract={Objectives. We examined a syndemic of psychosocial health issues among young men who have sex with men (MSM), with men and women (MSMW), and with women (MSW). We examined hypothesized drivers of syndemic production and effects on suicide attempts. Methods. Using a pooled data set of 2005 and 2007 Youth Risk Behavior Surveys from 11 jurisdictions, we used structural equation modeling to model a latent syndemic factor of depression symptoms, substance use, risky sex, and intimate partner violence. Multigroup models examined relations between victimization and bullying experiences, syndemic health issues, and serious suicide attempts. Results. We found experiences of victimization to increase syndemic burden among all male youths, especially MSMW and MSM compared with MSW (variance explained = 44\%, 38\%, and 10\%, respectively). The syndemic factor was shown to increase the odds of reporting a serious suicide attempt, particularly for MSM (odds ratio {[}OR] = 5.75; 95\% confidence interval {[}CI] = 1.36, 24.39; P < .001) and MSMW (OR = 5.08; 95\% CI = 2.14, 12.28; P < .001) compared with MSW (OR = 3.47; 95\% CI = 2.50, 4.83; P < .001). Conclusions. Interventions addressing multiple psychosocial health outcomes should be developed and tested to better meet the needs of young MSM and MSMW.},
-- }" WHERE `selfirst` = 147290.0 AND `seltext` IS NULL;
