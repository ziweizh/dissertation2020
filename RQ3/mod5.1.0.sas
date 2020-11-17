proc import datafile="\\my.files.iastate.edu\Users\ziweizh\Desktop\GLIMXX\rating_data.csv" 
out=mydata dbms=csv replace; 
getnames=yes; 
run;

proc glimmix data=mydata method=laplace;
     class ratee rater_num2 rater_num;
     model teach_total = date3 rater_num2 fluency_PC1 nfiller_per_token nrep_per_token L7 gcp_confidence rpvi_v lca_PC1 taales_PC1 sca_PC1 taasc_PC1 taasc_PC3 taaco_PC1/ solution dist=multinomial link=cumlogit;
     random ratee rater_num/ solution;
	 output out=glimmixout pred(blup ilink)=PredProb
	 					   pred(noblup ilink)=PredProb_PA;
run;

