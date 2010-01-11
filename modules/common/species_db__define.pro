; WORK IN PROGRESS
;+ 
; CLASS_NAME:
; SPECIES_DB
;
; PURPOSE:
; This file contains the species definition for CBM Mechanisms
; It creates a number of hashes that contain the relavant speies (UPPER CASE)
; as Keys. The value is the molecular weight in kg/mol where defined, 0 where it is not defined.
;
; Since we have a part-of hierarchy, we can autamatically compose the hashes higher up
; in the hierarchy
;
; We also define a hash-of-hashes called compound_species. The keys of this hash are names of
; pseudo-species that are the sum of other species (such as PM or SOA)
; the values are proper hash objects that contain the relevant species to be added.
;
; based on the value of the "parameters"
; aerosol_mechanism 
; chemistry_mechanism
; The proper hashes are defined (see the definition section for a list of these hashes)
;
; Note that the species list might be incomplete (comes from the manual)
;
; CATEGORY:
; Data providers
;
; SUPERCLASSES:
;       None.
;
; SUBCLASSES:
;       This class has no subclasses.
;
; CREATION:
;       See SPECIES_DB::INIT
;
; METHODS:
;       Intrinsic Methods
;       This class has the following methods:
;
;       SPECIES_DB::INIT			initializes a new header_parser object
;       SPECIES_DB::CLEANUP		destroys a header_parser object
;
; MODIFICATION HISTORY:
;   Written and documented, 28. July 2009, dco
; 
;  $Id$
;-
; Copyright (C) 2009, Daniel Oderbolz
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

;+
; =============================================================
;
; METHODNAME:
;       SPECIES_DB::INIT
;
; PURPOSE:
;       Creates a species_db object.
;
; CALLING SEQUENCE:
;
;       result = obj_new('species_db', filename)
;
; DESCRIPTION:
;
;       The INIT method creates a new species_db object and
;       initializes it.  The user must supply the chemical and the aerosol mechanism to be used.
;
; MANDATORY INPUTS:
;
;       chemistry_mechanism - the chemistry mechanism used (like CBM03 or SAPRC99)
;       aerosol_mechanism - the aerosol mechanism to be used (CF or CMU)
;
; RETURNS:
;       A new species_db object.
;
; EXAMPLE:
;       sdb = obj_new('species_db','CMB05','CF')
;
;   
;-
; =============================================================
function species_db::init, chemistry_mechanism, aerosol_mechanism
; =============================================================
	COMPILE_OPT IDL2

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Defaults
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	default_aerosol_mechanism = 'CF'
	default_chemistry_mechanism = 'CBM05'
	
	if (n_elements(aerosol_mechanism) EQ 0) then 
		print,'The variable aerosol_mechanism is not defined! Using ' + default_aerosol_mechanism
		aerosol_mechanism = default_aerosol_mechanism
	endif
	
	if (n_elements(chemistry_mechanism) EQ 0) then 
		print,'The variable chemistry_mechanism is not defined! Using ' + default_chemistry_mechanism
		chemistry_mechanism = default_chemistry_mechanism
	endif
	
	; Correct options
	aerosol_mechanism=strtrim(strupcase(aerosol_mechanism),2)
	chemistry_mechanism=strtrim(strupcase(chemistry_mechanism),2)
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Setup hashes
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; These need to be filled explicitly
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	; Gaseous species
	gasses = obj_new('hashtable')
	
	; Biogenic SOA (including oligomers, if available)
	bsoa = obj_new('hashtable')
	
	; Antropogenic SOA (including oligomers, if available)
	asoa = obj_new('hashtable')
	
	; Organic Aerosol (soa is added later)
	oa = obj_new('hashtable')
	
	; Inorganic Aerosol
	ia = obj_new('hashtable')
	
	; Compound species
	compound_species = obj_new('hashtable')
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; These are filled automatically
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	; Secondary organic aerosol (including oligomers, if available)
	soa = obj_new('hashtable')
	
	; All aerosol species
	all_aerosol_species = obj_new('hashtable')
	
	; All species
	all_species = obj_new('hashtable')
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Fill hashes based on aerosol_mechanism and chemistry_mechanism
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	case chemistry_mechanism of 
	
		'CBMIV' : 	begin
	
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						;; gasses
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						gasses->add,'NO2',	0.046
						gasses->add,'NO',	0.030
						gasses->add,'O',	0.0159
						gasses->add,'O3',	0.048
						gasses->add,'NO3',	0
						gasses->add,'O1D',	0.0159
						gasses->add,'OH',	0
						gasses->add,'HO2',	0
						gasses->add,'N2O5',	0
						gasses->add,'HNO3',	0.063
						gasses->add,'HONO',	0.047
						gasses->add,'PNA',	0.079
						gasses->add,'H2O2',	0.034
						gasses->add,'CO',	0.028
						gasses->add,'FORM',	0.030
						gasses->add,'ALD2',	0
						gasses->add,'C2O3',	0
						gasses->add,'XO2',	0
						gasses->add,'PAN',	0.121
						gasses->add,'PAR',	0
						gasses->add,'XO2N',	0
						gasses->add,'ROR',	0
						gasses->add,'NTR',	0
						gasses->add,'OLE',	0
						gasses->add,'ETH',	0
						gasses->add,'TOL',	0.0921
						gasses->add,'CRES',	0
						gasses->add,'TO2',	0
						gasses->add,'OPEN',	0
						gasses->add,'CRO',	0
						gasses->add,'MGLY',	0
						gasses->add,'XYL',	0.1062
						gasses->add,'ISOP',	0.0681
						gasses->add,'ISPD',	0
						gasses->add,'SO2',	0.064
						gasses->add,'SULF',	0
						gasses->add,'MEOH',	0
						gasses->add,'ETOH',	0
						
	
					end
					
		'CBM05' : 	begin
		
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						;; gasses
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						gasses->add,'NO',	0
						gasses->add,'NO2',	0
						gasses->add,'O3',	0
						gasses->add,'O',	0
						gasses->add,'O1D',	0
						gasses->add,'OH',	0
						gasses->add,'HO2',	0
						gasses->add,'H2O2',	0
						gasses->add,'NO3',	0
						gasses->add,'N2O5',	0
						gasses->add,'HONO',	0
						gasses->add,'HNO3',	0
						gasses->add,'PNA',	0
						gasses->add,'CO',	0
						gasses->add,'FORM',	0
						gasses->add,'ALD2',	0
						gasses->add,'C2O3',	0
						gasses->add,'HCO3',	0
						gasses->add,'PAN',	0
						gasses->add,'ALDX',	0
						gasses->add,'CXO3',	0
						gasses->add,'PANX',	0
						gasses->add,'XO2',	0
						gasses->add,'XO2N',	0
						gasses->add,'NTR',	0
						gasses->add,'ETOH',	0
						gasses->add,'MEO2',	0
						gasses->add,'MEOH',	0
						gasses->add,'MEPX',	0
						gasses->add,'FACD',	0
						gasses->add,'ETHA',	0
						gasses->add,'ROOH',	0
						gasses->add,'AACD',	0
						gasses->add,'PACD',	0
						gasses->add,'PAR',	0
						gasses->add,'ROR',	0
						gasses->add,'ETH',	0
						gasses->add,'OLE',	0
						gasses->add,'IOLE',	0
						gasses->add,'ISOP',	0
						gasses->add,'ISPD',	0
						gasses->add,'TERP',	0
						gasses->add,'TOL',	0
						gasses->add,'XYL',	0
						gasses->add,'CRES',	0
						gasses->add,'TO2',	0
						gasses->add,'OPEN',	0
						gasses->add,'CRO',	0
						gasses->add,'MGLY',	0
						gasses->add,'SO2',	0
						gasses->add,'SULF',	0
						
	
					end
					
		'SAPRC99' : begin
		
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						;; gasses
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						gasses->add,'NO2',	0
						gasses->add,'NO',	0
						gasses->add,'O3',	0
						gasses->add,'O',	0
						gasses->add,'NO3',	0
						gasses->add,'N2O5',	0
						gasses->add,'HNO3',	0
						gasses->add,'O1D',	0
						gasses->add,'OH',	0
						gasses->add,'HONO',	0
						gasses->add,'HO2',	0
						gasses->add,'CO',	0
						gasses->add,'HNO4',	0
						gasses->add,'HO2H',	0
						gasses->add,'SO2',	0
						gasses->add,'SULF',	0
						gasses->add,'CXO2',	0
						gasses->add,'HCHO',	0
						gasses->add,'COOH',	0
						gasses->add,'MEOH',	0
						gasses->add,'RO2R',	0
						gasses->add,'ROOH',	0
						gasses->add,'R2O2',	0
						gasses->add,'RO2N',	0
						gasses->add,'RNO3',	0
						gasses->add,'MEK',	0
						gasses->add,'PROD',	0
						gasses->add,'CCO3',	0
						gasses->add,'PAN',	0
						gasses->add,'CO3H',	0
						gasses->add,'CO2H',	0
						gasses->add,'RCO3',	0
						gasses->add,'PAN2',	0
						gasses->add,'CCHO',	0
						gasses->add,'RC3H',	0
						gasses->add,'RC2H',	0
						gasses->add,'BZCO',	0
						gasses->add,'PBZN',	0
						gasses->add,'BZO',	0
						gasses->add,'MCO3',	0
						gasses->add,'MPAN',	0
						gasses->add,'TBUO',	0
						gasses->add,'ACET',	0
						gasses->add,'NPHE',	0
						gasses->add,'PHEN',	0
						gasses->add,'BZNO',	0
						gasses->add,'XN',	0
						gasses->add,'HCO3',	0
						gasses->add,'HC2H',	0
						gasses->add,'RCHO',	0
						gasses->add,'GLY',	0
						gasses->add,'MGLY',	0
						gasses->add,'BACL',	0
						gasses->add,'CRES',	0
						gasses->add,'BALD',	0
						gasses->add,'METH',	0
						gasses->add,'MVK',	0
						gasses->add,'ISPD',	0
						gasses->add,'DCB1',	0
						gasses->add,'DCB2',	0
						gasses->add,'DCB3',	0
						gasses->add,'ETHE',	0
						gasses->add,'ISOP',	0
						gasses->add,'TERP',	0
						gasses->add,'ALK1',	0
						gasses->add,'ALK2',	0
						gasses->add,'ALK3',	0
						gasses->add,'ALK4',	0
						gasses->add,'ALK5',	0
						gasses->add,'ARO1',	0
						gasses->add,'ARO2',	0
						gasses->add,'OLE1',	0
						gasses->add,'OLE2',	0
						gasses->add,'ETOH',	0
						gasses->add,'MTBE',	0
						gasses->add,'MBUT',	0
						
	
					end
		
		else: message,'Chemistry mechanism chemistry_mechanism not supported!"
	
	endcase
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; aerosols
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	case aerosol_mechanism of
	
		'CF' :	begin
	
					asoa->add,'SOA1',	0
					asoa->add,'SOA2',	0
					asoa->add,'SOPA',	0
					
					bsoa->add,'SOA3',	0
					bsoa->add,'SOA4',	0
					bsoa->add,'SOA5',	0
					bsoa->add,'SOA6',	0
					bsoa->add,'SOA7',	0
					asoa->add,'SOPB',	0
					
					oa->add,'POA',	0
					
					ia->add,'PSO4',	0
					ia->add,'PNO3',	0
					ia->add,'PNH4',	0
					ia->add,'PH2O',	0
					ia->add,'NA',	0
					ia->add,'PCL',	0
					ia->add,'PEC',	0
					ia->add,'FPRM',	0
					ia->add,'FCRS',	0
					ia->add,'CPRM',	0
					ia->add,'CCRS',	0
	
				end
				
		else: message,'Aerosol mechanism aerosol_mechanism not supported!"
		
	endcase
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Compile the collected Hashes
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; Organic aerosol also contains the soa species
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	; BSOA
	keys = bsoa->keys()
	for i = 0L, n_elements(keys)-1 do begin
		oa->add,keys(i),bsoa->get(keys(i))
	endfor
	
	; ASOA
	keys = asoa->keys()
	for i = 0L, n_elements(keys)-1 do begin
		oa->add,keys(i),asoa->get(keys(i))
	endfor
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; Aerosols are made up of the species in the subgrops Organic and Inorganic aerosol
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	; Organics
	keys = oa->keys()
	for i = 0L, n_elements(keys)-1 do begin
		all_aerosol_species->add,keys(i),oa->get(keys(i))
	endfor
	
	; Inorganics
	keys = ia->keys()
	for i = 0L, n_elements(keys)-1 do begin
		all_aerosol_species->add,keys(i),ia->get(keys(i))
	endfor
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; All species are aerosols plus gasses
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	; aerosols
	keys = all_aerosol_species->keys()
	for i = 0L, n_elements(keys)-1 do begin
		all_species->add,keys(i),all_aerosol_species->get(keys(i))
	endfor
	
	; gasses
	keys = gasses->keys()
	for i = 0L, n_elements(keys)-1 do begin
		all_species->add,keys(i),gasses->get(keys(i))
	endfor
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; Define a few simple coupound species
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	compound_species->add,'PM',all_aerosol_species
	compound_species->add,'SOA',soa
	compound_species->add,'BSOA',bsoa
	compound_species->add,'ASOA',asoa

	return, 1
end

; =============================================================
; METHODNAME: SPECIES_NAME__DEFINE
;  internal method: defines species db data structure (aka class)
pro species_db__define
; =============================================================
	COMPILE_OPT IDL2
	
	struct = {species_db, $
			gasses: obj_new(), $		;; Hash of gasses
			aerosols: obj_new(), $		;; Hash of aerosols
			compound_species: obj_new(), $		;; Hash of hashes of compound species
			chemistry_mechanism: "", $	;; The chemistry mechanism we are looking at
			aerosol_mechanism: "" $ 		;; The aerosol mechanism we are looking at
			}
  return
end