From: Alexandra Tsimpidi [tsimpidi@chemeng.upatras.gr]
Sent: Montag, 16. Februar 2009 13:35
To: Oderbolz Daniel
Subject: size bin codes

Attachments: makearea_2006.f; makepoint_2006.f


	In the attached files are the codes for splitting the aerosol emissions into 8 size bins. Again you have to change the paths of the files.
n1,n2,....n27 are the names of the initial emissions files you want to split them in size bins and m1,m2,...,m27 are the name of the emissions you will create after running these codes. In lines 436-507 in makearea_2006.f you can check the factors that are multiplied with the total emissions of each species that we are using. Please also check if the positions of the aerosol species are identical with your files. For instance sulfate is in the position 28 etc. The same changes you need to make in the second file which
is for the point emissions.    
	
