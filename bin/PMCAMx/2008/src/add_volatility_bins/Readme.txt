From: Alexandra Tsimpidi [tsimpidi@chemeng.upatras.gr]
Sent: Montag, 16. Februar 2009 13:33
To: Oderbolz Daniel
Subject: FW: problem with pmcamx

Attachments: bswap.cvt.f; prep.sp.f; Makefile

 

 


--------------------------------------------------------------------------------

 

In the attach file you can find the source codes in which you can convert your non volatile emissions in volatile.

The main code in which you need to make changes is the prep.sp.f

In line 85, 86 are the emission factors for each volatility bin. The numbers that you can see there are the ones that we are using for our basecase simulations.

In line 92 you have ip=1 if you want to convert the point emissions to volatile and ip=2 when you want to convert the area emissions.

In lines 128, 129 are the name and the path of the new volatile emission file while in lines 136, 137 are the name and the path of the file you want to change. 

In lines 167, 168 is the positions of POC (primary organic emissions) in the emission file.  The different numbers correspond to different size sections. You have to check in which positions are POC in your emission file and change the numbers there and in the rest of the code.

Alexandra  

 
