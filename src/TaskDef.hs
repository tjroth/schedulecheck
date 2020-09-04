

tasks = [("Wknd Gen2",
  [Saturday,Sunday,Friday,Monday],
  ["ALT","BOR","BRD","BWT","CML","CRO","DAL","DGD","HAR","HIN","JGA","JKT","KDP","KHA","PEN","SHK","TAN","WAT"]),
("Wknd Gen1",
 [Saturday,Sunday,Friday,Monday,Thursday],
 ["ADM","BRD","BWN","BWT","CAR","CML","CRO","DGD","DOS","HAR","JKT","JLB","KDP","KEE","LLA","PEN","RED","SHK","SM ","SOP","TAN"]),
("Peds Wknd",
 [Saturday,Sunday,Friday,Monday,Thursday],
 ["JLB","LKB","LLA","MCP"]),
("Neuro Wknd",
 [Saturday,Sunday,Friday,Monday,Thursday],
 ["BWT","HOL","JGA","KHA","KLA","RED","TAN"]),
("MSK Wknd",
 [Saturday,Sunday,Friday,Monday,Thursday],
 ["BRD","BWN","CAR","HIN","JBM","PEN","SHA"]),
("Body Wknd",
 [Friday,Saturday,Sunday,Monday,Thursday],
 ["BOR","EDW","JGA","KDP","LOT","MOR","TJR","WAT"]),
("RSNA",[Monday,Tuesday,Wednesday],
 ["BRD","HAR","JLB","SM"]),
("TRN",[Wednesday,Thursday,Monday,Tuesday,Friday],
 ["ADM","KDM","KEE","PEN","RRB"])
("HOff",
 [Monday,Thursday,Friday],["ADM","BOR","BWN","BWT","CAR","CLO","CML","CRO","DAL","DGD","DOS","EDW","HIN","HOL","JBM","JGA","JKT","KDM","KDP","KEE","KHA","LKB","LLA","LOT","MCP","MOR","PEN","RED","RRB","SHA","SHK","SM","SOP","SSL","TAN","TJR","WAT"])
("PCO",
 [Monday,Wednesday,Thursday,Friday,Tuesday],
 ["ADM","BOR","BRD","BWN","BWT","CAR","CML","CRO","DAL","DGD","DOS","HAR","HIN","HOL","JBM","JGA","JKT","JLB","KDP","KHA","LKB","LLA","MCP","MOR","PEN","RED","SHK","SM","SOP","TAN","TJR","WAT"])
("LEV",
 [Monday,Wednesday,Friday,Thursday],
 ["EDW"])
("Admin",
 [Wednesday,Thursday,Friday,Tuesday,Monday],
 ["BWN","HAR","HOL","KDP","LOT","SM","TJR"])
("Vac",
 [Wednesday,Thursday,Friday,Tuesday,Monday],
 ["ADM","BOR","BRD","BWN","BWT","CAR","CML","CRO","DAL","DGD","DOS","EDW","HAR","HIN","HOL","JBM","JGA","JKT","JLB","KDM","KDP","KEE","KHA","KLA","LKB","LLA","LOT","MCP","MOR","PEN","RED","RRB","SHA","SHK","SM","SOP","SSL","TAN","TJR","WAT"])
("PCO2",
 [Monday,Tuesday,Wednesday,Thursday,Friday],
 ["BOR","BRD","BWN","HOL","JKT","JLB","KDP","KHA","KLA","LLA","MOR","TAN"])
("BKU",
 [Thursday,Monday,Tuesday,Wednesday,Friday],
 ["ADM","BOR","BRD","BWN","BWT","CAR","CML","CRO","DAL","DGD","DOS","HAR","HIN","HOL","JBM","JGA","JKT","JLB","KDP","KEE","KHA","KLA","LKB","LLA","LOT","MCP","MOR","PEN","RED","SHK","SM","SOP","TAN","TJR","WAT"])
("BDO",
 [Wednesday,Friday,Thursday,Tuesday,Monday],
 ["LOT"])
("DOff",
 [Friday,Tuesday,Monday,Thursday,Wednesday],
 ["ADM","ALT","BOR","BRD","BWN","BWT","CAR","CML","CRO","DAL","DGD","DOS","EDW","HAR","HIN","HOL","JBM","JGA","JKT","JLB","KDP","KEE","KHA","KLA","LKB","LLA","LOT","MCP","MOR","NAZ","PEN","RBC","RED","SHA","SHK","SM","SOP","SSL","TAN","TJR","WAT"])
("WM Cardiac MR",
 [Wednesday,Thursday,Friday,Monday,Tuesday],["EDW","JLB","LLA","TAN","TJR"])
("WMR Late",
 [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday],["ALT","NAZ","RBC"])
("WMR Eve2",
 [Monday,Tuesday,Wednesday,Thursday,Friday],["ADM","ALT","BOR","BRD","BWN","BWT","CAR","CML","CRO","DAL","DGD","DOS","HAR","HIN","HOL","JBM","JGA","JKT","JLB","KDP","KHA","LKB","LLA","MCP","MOR","NAZ","PEN","RBC","RED","SHK","SOP","TAN","TJR","WAT"])
("WMR Eve1",
 [Monday,Tuesday,Wednesday,Thursday,Friday],["ADM","ALT","BOR","BRD","BWN","BWT","CAR","CML","CRO","DAL","DGD","DOS","HAR","HIN","HOL","JBM","JGA","JKT","JLB","KDP","KEE","KHA","KLA","LKB","LLA","LOT","MCP","MOR","PEN","RBC","RED","SHK","SM","SOP","TAN","TJR","WAT"])
("HAW",
 [Monday,Tuesday,Wednesday,Thursday,Friday],["BOR","BWN","HAR","HOL","JKT","TAN"])
("VIR-WMC",
 [Monday,Tuesday,Wednesday,Thursday,Friday],["ADM","CML","DAL","DOS","HAR","KEE","SM","SOP"])
("VIR-VEIN",
 [Wednesday,Thursday,Monday,Tuesday,Friday],["ADM","CAR","CLO","CML","DAL","DOS","HAR","KEE","SM","SOP","WAT"])
("VIR2-WMR",
 [Monday,Tuesday,Wednesday,Thursday,Friday],["ADM","BWN","CML","DAL","DOS","HAR","KEE","KLA","SM","SOP"])
("VIR1-WMR",
 [Monday,Tuesday,Wednesday,Thursday,Friday],["ADM","CML","DAL","DOS","HAR","KEE","SOP"])
("PEDS-WMR",
 [Monday,Tuesday,Wednesday,Thursday,Friday],["JLB","LKB","LLA","MCP"])
("PEDS-RRC/WMList",
 [Tuesday,Wednesday,Thursday,Friday,Monday],["JLB","LKB","LLA","MCP"])
("NEURO-WMR",
 [Monday,Tuesday,Wednesday,Thursday,Friday],["ALT","BWT","HOL","JGA","KHA","KLA","NAZ","RED","TAN"])
("NEURO-FV",
 [Monday,Wednesday,Thursday],["BWT","HOL","JGA","KHA","RED","TAN"])
("NEURO-CLAY",
 [Tuesday,Friday,Monday],["BWN","BWT","HOL","JGA","KHA","RED","TAN"])
("NEURO-BRO-EVE",
 [Monday,Tuesday,Wednesday,Thursday,Friday],["ALT","BWT","HOL","JGA","KHA","KLA","NAZ","RED","TAN"])
("NEURO-KNI",
 [Monday],["BWT","CLO","HOL","JGA","KHA","RED","TAN"])
("MSK-WMN",
 [Tuesday,Thursday],["BRD","BWN","CAR","JBM","PEN","SHA"])
("MSK-MRI-CLA",[Monday,Tuesday],["BRD","BWN","CAR","CLO","HIN","HOL","JBM","PEN","SHA"])
("MSK-RRC",[Tuesday,Monday,Wednesday,Thursday,Friday],["BRD","BWN","CAR","HIN","JBM","PEN"])
("MSK-RMP",[Monday,Wednesday,Friday],["BRD","BWN","CAR","EDW","HIN","JBM","MOR","PEN","SHA"])
("MSK-KNI",[Tuesday,Thursday,Friday,Monday],["BRD","BWN","CAR","JBM","PEN","SHA"])
("MSK-BRI",[Monday,Wednesday,Friday,Tuesday,Thursday],["BRD","BWN","CAR","HIN","JBM","PEN","SHA"])
("MAMMLT/GEN-WMN",[Friday],["EDW"])
("MAMMLT/GEN-WF",[Tuesday],["BOR","BWT","CAR","DGD","EDW","JBM","KDP","LLA","SHK","TJR","WAT"])
("MAMMLT/GEN-RMP",[Tuesday,Thursday],["BOR","BRD","BWN","BWT","CAR","CRO","DGD","EDW","JBM","JGA","KDP","KHA","RED","SHA","SHK","TAN","TJR","WAT"])
("MAMMLT/GEN-KNI",[Monday,Friday,Wednesday,Thursday,Tuesday],["BOR","BRD","BWN","BWT","CAR","DGD","EDW","HIN","HOL","JBM","JGA","KDP","KHA","LLA","PEN","SHA","SHK","TAN","TJR","WAT"])
("MAMMLT/GEN-CED",[Monday,Wednesday,Thursday],["BOR","BRD","BWN","BWT","CAR","CRO","DGD","EDW","JBM","JKT","KDP","KHA","LLA","LOT","PEN","RED","SHK","TAN","TJR","WAT"])
("MAMMLT/GEN-BRO",[Monday,Wednesday,Thursday,Friday],["BOR","CRO","DGD","EDW","JGA","JKT","KDP","LOT","SHK","SSL","TJR"])
("GEN-WMApex",[Wednesday,Monday,Thursday],["ADM","BOR","BRD","CAR","DAL","DGD","DOS","EDW","HOL","JBM","JGA","JKT","JLB","KDP","KEE","KHA","KLA","LKB","LLA","MCP","MOR","PEN","RED","SHA","SHK","SM","TAN","TJR","WAT"])
("GEN-WMC",[Monday,Tuesday,Wednesday,Thursday,Friday],["ADM","BOR","BRD","BWN","BWT","CAR","CML","CRO","DAL","DGD","DOS","EDW","HAR","HIN","HOL","JBM","JGA","JKT","JLB","KEE","KHA","KLA","LKB","LLA","LOT","MCP","MOR","PEN","RED","SHA","SHK","SM","SOP","TAN","TJR","WAT"])
("GEN-FLUORO-WMR",[Monday,Tuesday,Wednesday,Thursday,Friday],["ADM","BOR","BRD","BWN","BWT","CAR","CML","CRO","DAL","DGD","DOS","EDW","HIN","HOL","JBM","JGA","JKT","JLB","KDP","KEE","KHA","KLA","LKB","LLA","MCP","MOR","PEN","SHA","SHK","SM","TAN","TJR","WAT"])
("BREAST-WMN",[Friday,Monday,Wednesday],["CLO","CRO","JKT","LOT","SHK","SSL","TJR"])
("BREAST-WMC/RRC",[Monday,Thursday],["CRO","JKT","LOT","SHK","SSL"])
("BREAST-WMC/BRO",[Wednesday],["CRO","JKT","LOT","SHK","SSL"])
("BREAST-BRO/RRC",[Friday,Tuesday],["CRO","JKT","LOT","SHK","SSL"])
("BREAST-BRO",[Tuesday],["CRO","JKT","LOT","SHK","SSL"])
("BODY-WMR",[Monday,Tuesday,Wednesday,Thursday,Friday],["BOR","EDW","JGA","KDP","MOR","TJR","WAT"])
("BODY-WFO",[Monday,Thursday,Wednesday],["BOR","EDW","JGA","KDP","TJR","WAT"])
("BODY-KNI",[Wednesday,Friday],["BOR","EDW","JGA","KDP","WAT"])
("BODY-CED",[Tuesday,Friday,Wednesday],["BOR","EDW","JGA","KDP","MOR","TJR","WAT"])]
