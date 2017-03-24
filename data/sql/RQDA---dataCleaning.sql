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
