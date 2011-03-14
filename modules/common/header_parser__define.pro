;+ 
; CLASS_NAME:
; HEADER_PARSER
;
; PURPOSE:
; Parses (ASCII) CAMx Input and output files, returns 2 hashes. 
; Hash scalars cotains the scalars
; Hash species contains the species in the file
;
; CATEGORY:
; Parsers
;
; SUPERCLASSES:
;       None.
;
; SUBCLASSES:
;       This class has no subclasses.
;
; CREATION:
;       See HEADER_PARSER::INIT
;
; DESCRIPTION:
;
;       This is a class that can parse various CAMx files.
;
;
; METHODS:
;       Intrinsic Methods
;       This class has the following methods:
;
;       HEADER_PARSER::INIT			initializes a new header_parser object
;       HEADER_PARSER::CLEANUP		destroys a header_parser object
;       HEADER_PARSER::PARSE		does the actual parsing
;       HEADER_PARSER::IS_OK		returns false if header is inconsistent or incomplete
;       HEADER_PARSER::GET_SPECIES	Returns the (trimmed) species as a hashtable
;       HEADER_PARSER::GET_REVERTED_SPECIES	Returns the (trimmed) species as a reverted hashtable
;       HEADER_PARSER::GET_SCALARS	Returns the scalars as a hashtable
;       HEADER_PARSER::GET_SPECIES_ARR  Returns the (trimmed) species as an array (for reverse indexing)
;       HEADER_PARSER::GET_HEADER_LENGTH	Returns the number of header lines
;       HEADER_PARSER::GET_UPDATE_TIMES	Returns an fltarr containing ibdate,btime,iedate,etime of each update time (using regex)

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
;       HEADER_PARSER::INIT
;
; PURPOSE:
;       Creates a header parser object.
;
; CALLING SEQUENCE:
;
;       result = obj_new('header_parser', filename)
;
; DESCRIPTION:
;
;       The INIT method creates a new header parser object and
;       initializes it.  The user must supply a file that should be parsed.
;
; OPTIONAL INPUTS:
;
;       is_binary - boolean indicating binary if true. Default is false (ASCII assumed)
;
;
; RETURNS:
;       A new header_parser object.
;
; EXAMPLE:
;       ASCII:
;       hp = obj_new('header_parser','CAMx-v4.51-co5-s173-sem064-aug-sept-2003/Outputs/ascii/CAMx-v4.51-co5-s173-sem064-aug-sept-2003.20030906.avrg.grd03.asc')
;
;      BINARY:
;      hp = obj_new('header_parser','/afs/psi.ch/intranet/LAC/projects/BAFU-PSAT/CAMxRuns/Runs/CAMx-v4.51-bafu-psat-2006-s326-sem084/Inputs/broken/run1/bc_bafu-psat_moz_060601.bin',1)
;   
;-
; =============================================================
function header_parser::init, filename, is_binary
; =============================================================
	COMPILE_OPT IDL2
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Report revision
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  my_revision_string='$Id$'
  my_revision_arr=strsplit(my_revision_string,' ',/EXTRACT)
  print,my_revision_arr[1] + ' has revision ' + my_revision_arr[2]
	
	; We need a filename!
	if (N_ELEMENTS(filename) EQ 0)  then message,'I need a filename as parameter!'
	
	; Fix optional stuff
	if (N_ELEMENTS(is_binary) EQ 0)  then is_binary = 0
	
	; Exit if file is not readable
	if (~ FILE_TEST(filename,/READ) ) then message,'File not readable: ' + filename
	
	; Instatiate Hashmaps
	self.scalars = obj_new('hashtable')
	self.species = obj_new('hashtable')
	self.reversed_species = obj_new('hashtable')
	
	; This one contains the suported filetypes
	self.file_types = obj_new('hashtable')
	
	; Populate the self object
	self.filename = filename
	self.is_binary = is_binary

	; Add supported filetypes
	self.file_types->add,'AVERAGE',1
	self.file_types->add,'AIRQUALITY',1
	self.file_types->add,'BOUNDARY',1
	self.file_types->add,'EMISSIONS',1
	
	; Parse it now
	self->parse
	
	return, 1
end

;+
; =============================================================
;
; FUNCTIONNAME:
;       HEADER_PARSER::PREFILL
;
; PURPOSE:
;       Prefills a string for binary read. To read a 
;       character*4(10) String, prefill the target string:
;       target=prefill(4,10)
;       readu,parser_lun,target
;
; CALLING SEQUENCE:
;
;       target=prefill(4,10)
;       
;       
;-
; =============================================================
function header_parser::prefill,charlen,count
; =============================================================

	target=''

	for i=1,count do begin
	
		for j=1,charlen do begin
			target = target + '0'
		endfor
	
	endfor
	
	return,target

end

;+
; =============================================================
;
; METHODNAME:
;       HEADER_PARSER::PARSE
;
; PURPOSE:
;       Parses the given file (internal function)
;
; CALLING SEQUENCE:
;
;       hp->parse
;       
; DESCRIPTION:
;
;       This procedure performs the actual parsing.
;       It is the central function of this class.
;
; OPTIONAL INPUTS:
;       None.
;       
;
; KEYWORD PARAMETERS:
;       None.
;       
;
; EXAMPLE:
;       hp->parse
;       
;-
; =============================================================
pro header_parser::parse
; =============================================================

	COMPILE_OPT IDL2
	
	; Is the file empty?
	if (FILE_TEST(self.filename,/ZERO_LENGTH) ) then message,'File is empty: ' + self.filename
	
	; Setup IO Error handler
	;ON_IOError, IO_Error
	
	; Depending on the flag is_binary we open ascii or not
	
	if ( self.is_binary ) then begin
		; Binary
		openr,parser_lun,self.filename,/GET_LUN,/F77_UNFORMATTED,/SWAP_ENDIAN
		
		; We must prefill all string variables we want to read
		type=self.prefill(4,10)
		note=self.prefill(4,60)
		
		ione=1
		rdum=0.0
		nspec=0
		ibdate=0
		btime=0.0
		iedate=0
		etime=0.0
		iutm=0
		xorg=0.0
		yorg=0.0
		delx=0.0
		dely=0.0
		nx=0
		ny=0
		nz=0
		nx2=0
		ny2=0
		
		readu,parser_lun,type
		readu,parser_lun,note
		readu,parser_lun,nspec
		
		readu,parser_lun,ione
		
		readu,parser_lun,ibdate
		readu,parser_lun,btime
		readu,parser_lun,iedate
		readu,parser_lun,etime
		readu,parser_lun,rdum
		readu,parser_lun,rdum
		readu,parser_lun,iutm
		readu,parser_lun,xorg
		readu,parser_lun,yorg
		readu,parser_lun,delx
		readu,parser_lun,dely
		readu,parser_lun,nx
		readu,parser_lun,ny
		readu,parser_lun,nz
		readu,parser_lun,idum
		readu,parser_lun,idum
		readu,parser_lun,rdum
		readu,parser_lun,rdum
		readu,parser_lun,rdum
		readu,parser_lun,ione
		readu,parser_lun,ione
		readu,parser_lun,nx2
		readu,parser_lun,ny2

		; Store the compressed result
		self.scalars->add,'name',strcompress(type,/REMOVE_ALL)
		self.scalars->add,'type',strcompress(type,/REMOVE_ALL)
		self.scalars->add,'note',strcompress(note,/REMOVE_ALL)
		self.scalars->add,'nspec',nspec
		
		self.scalars->add,'ibdate',ibdate
		self.scalars->add,'btime',btime
		self.scalars->add,'iedate',iedate
		self.scalars->add,'etime',etime
		self.scalars->add,'iutm',iutm
		self.scalars->add,'xorg',xorg
		self.scalars->add,'yorg',yorg
		self.scalars->add,'delx',delx
		self.scalars->add,'dely',dely
		self.scalars->add,'nx',nx
		self.scalars->add,'ny',ny
		self.scalars->add,'nz',nz
		
		; Lets read the species
		arspec=strArr(nspec)
		
		; prefill
		for i=0L, n_elements(arspec) -1 do begin
			arspec[i]=self.prefill(4,10)
		endfor
	
		; Now read nspec species
		readu, parser_lun, arspec
		
		; Trim
		arspec = strcompress(arspec,/REMOVE_ALL)
		
		; Save this array in instance variable
		self.arspec = Ptr_new(arspec)
		
		; Add species to internal hash
		; Value is the index.
		for i=0L, n_elements(arspec) -1 do begin
			self.species->add,strtrim(arspec[i],2),i
			; The same, but reversed (attention: key must be a string)
			self.reversed_species->add,strtrim(i,2),strtrim(arspec[i],2)
		endfor
		
		; Sanity check here!
		if (n_elements(arspec) NE nspec ) then begin
			print,"WRN: nspec (' + strtrim(nspec,2) + ') not equal to number of species counted (' + strtrim(n_elements(arspec),2) + ') We use the counted number!)"
			self.scalars->add,'nspec',LONG(n_elements(arspec))
		endif
		
		if (nx NE nx2 ) then begin
			print,"WRN: Inconsistent header: first nx not eqal no second!"
		endif
		
		if (ny NE ny2 ) then begin
			print,"WRN: Inconsistent header: first ny not eqal no second!"
		endif
		
		if (nx LT 1 ) then print,'WRN: nx less than 1 (' + strtrim(nx,2) + ')!'
		if (ny LT 1 ) then print,'WRN: ny less than 1 (' + strtrim(ny,2) + ')!'
		if (nz LT 1 ) then print,'WRN: nz less than 1 (' + strtrim(nz,2) + ')!'
		
		; Header length
		print,"For binary files, the header length is given in bytes. (Wrong for BC files!)"
		point_lun, -1 * parser_lun, pos
		
		self.header_length = pos
		
	endif else begin
		; ASCII
		openr,parser_lun,self.filename,/GET_LUN
		
		; Initialize variables
	
		; We read whole header into a array first, then parse that
		header_lines=4
		header_arr=strArr(header_lines)
	
		; Currently, all supported headers look the same.
		; Later, we might need a case statment.
		
		; Code valid for AVERAGE, AIRQUALITY, BOUNDARY and EMISSIONS
		
		; Read header w/o species
		readf, parser_lun, header_arr
		
		; Format specifications for writing header lines 2 to 4
		line2 = '(I2, I3, I7, F6.0, I6, F6.0)'
		
		; The thrid line is hard to read. Sometimes, 2 values "stick together"
		; We use Michels criterion: if the length of the 3rd line is LE 100, we use the
		; Format string, otherwise, we parse using strtrim
		line3 = '(F10.1, F11.1, I4, F10.1, F11.1, F7.0, F7.0, I4, I4, I4, I4, I4, F7.0, F7.0, F7.0)'
		line4 = '(4I5)'
		
		; Now parse each line
		; The first line is tougher than it seems since
		; this is valid:
		; AIRQUALITYCAMx-v4.51-bafu3-june-2006-s147-sem045-nib_p20
	
		; Extract type (first 10 characters...)
		; we trim to be on the safe side
		type=strtrim(strmid(header_arr[0],0,10),2)
		note=strtrim(strmid(header_arr[0],10),2)
		
		reads, header_arr[1], ione1, nspec, ibdate, btime, iedate, etime, format=line2
		
		if ( strlen(header_arr[2]) LE 100 ) then begin
			print,"Header line 3 has less than or 100 characters, we use formatted input"
			reads, header_arr[2], rdum1, rdum2, iutm, xorg, yorg, delx, dely, nx, ny, nz, idum1, idum2, rdum3, rdum4, rdum5, format=line3
		endif else begin
			print,"Header line 3 has more than 100 characters, we split the string"
		
			line3 = strSplit(header_arr[2],' ',/EXTRACT)
		
			rdum1=line3[0]
			rdum2=line3[1]
			iutm=line3[2]
			xorg=line3[3]
			yorg=line3[4]
			delx=line3[5]
			dely=line3[6]
			nx=line3[7]
			ny=line3[8]
			nz=line3[9]
			idum1=line3[10]
			idum2=line3[11]
			rdum3=line3[12]
			rdum4=line3[13]
			rdum5=line3[14]
		endelse
		
		reads, header_arr[3], ione2, ione3, nx2, ny2, format=line4
	
		; Is the file type supported?
		if (~ self.file_types->iscontained(type)) then message,'Filetype not supported: ' + type
	
		; Add scalar data to internal hash
		; w/o dummy variables
		; Casted to proper type
		
		; For historical reasons, type is also called name
		self.scalars->add,'name',type
		self.scalars->add,'type',type
		
		self.scalars->add,'note',note
		self.scalars->add,'nspec',LONG(strtrim(nspec,2))
		self.scalars->add,'ibdate',ibdate
		self.scalars->add,'btime',btime
		self.scalars->add,'iedate',iedate
		self.scalars->add,'etime',etime
		self.scalars->add,'iutm',LONG(strtrim(iutm,2))
		self.scalars->add,'xorg',FLOAT(strtrim(xorg,2))
		self.scalars->add,'yorg',FLOAT(strtrim(yorg,2))
		self.scalars->add,'delx',FLOAT(strtrim(delx,2))
		self.scalars->add,'dely',FLOAT(strtrim(dely,2))
		self.scalars->add,'nx',LONG(strtrim(nx,2))
		self.scalars->add,'ny',LONG(strtrim(ny,2))
		self.scalars->add,'nz',LONG(strtrim(nz,2))
		
		; Create array of (trimmed) species
		arspec=strArr(nspec)
	
		; Now read nspec species
		readf, parser_lun, arspec
		
		; Trim
		arspec = strtrim(arspec,2)
		
		; Save this array in instance variable
		self.arspec = Ptr_new(arspec)
		
		; Add species to internal hash
		; Value is the index.
		for i=0L, n_elements(arspec) -1 do begin
			self.species->add,strtrim(arspec[i],2),i
			; The same, but reversed (attention: key must be a string)
			self.reversed_species->add,strtrim(i,2),strtrim(arspec[i],2)
		endfor
		
		; Sanity check here!
		if (n_elements(arspec) NE nspec ) then begin
			print,"WRN: nspec (' + strtrim(nspec,2) + ') not equal to number of species counted (' + strtrim(n_elements(arspec),2) + ') We use the counted number!)"
			self.scalars->add,'nspec',LONG(n_elements(arspec))
		endif
		
		if (nx NE nx2 ) then begin
			print,"WRN: Inconsistent header: first nx not eqal no second!"
		endif
		
		if (ny NE ny2 ) then begin
			print,"WRN: Inconsistent header: first ny not eqal no second!"
		endif
		
		if (nx LT 1 ) then print,'WRN: nx less than 1 (' + strtrim(nx,2) + ')!'
		if (ny LT 1 ) then print,'WRN: ny less than 1 (' + strtrim(ny,2) + ')!'
		if (nz LT 1 ) then print,'WRN: nz less than 1 (' + strtrim(nz,2) + ')!'
		
		; Set the header length
		; The header length does not include any time or height dependent stuff
		
		print,"For ASCII files, the header length is given in lines"
		
		CASE type OF
		
			'AVERAGE':		BEGIN
											; 4 Fixed lines followed by the species
											self.header_length = 4 + nspec
										END
	
			'AIRQUALITY':	BEGIN
											; 4 Fixed lines followed by the species
											self.header_length = 4 + nspec
										END
	
			'BOUNDARY':		BEGIN
											; Its natsty, but we have to search for the first 
											; update time in the file because the headers look totally
											; different under different converters.
											; Inefficent since get_update_times essentially does the same
											
											; Use for efficiency, IDLs STREGEX is SLOOOW.
											MAX_HEADER_LENGTH=1000
											
											; This is what we seek
											regex='^ {1,}[0-9]{4} {1,}[0-9]{1,2}\.[0-9]{0,2} {1,}[0-9]{4} {1,}[0-9]{1,2}\.[0-9]{0,2}$'
		
											print,'Finding end of header of BC file - may take a while...'
											print,'We assume header is less than ' + strtrim(MAX_HEADER_LENGTH,2) + ' lines long'
											
											data=strarr(MAX_HEADER_LENGTH)
											
											; Rewind file
											Point_Lun,parser_lun,0
											
											readf, parser_lun, data
											
											; Apply regex
											found=stregex(data,regex)
											
											; get matches
											matches=WHERE(found NE -1, count)
											
											if (count NE -1) then begin
												; the header lenght is the pos. of the first match because
												; we are 0-based
												self.header_length = matches[0]
											endif else begin
												message,'Could not find first update time, hence cannot determine header lenght!'
											endelse
										END
	
			'EMISSIONS':	BEGIN
											; 4 Fixed lines followed by the species
											self.header_length = 4 + nspec
										END	
	
		ENDCASE
		

	endelse ; binary?
	
	; Free the LUN (closes unit)
	IF N_Elements(parser_lun) NE 0 THEN Free_Lun, parser_lun
	
	return
	
	IO_Error: 
	Print,'header_parser encountered an IO Error:' + !Error_State.Msg
	IF N_Elements(parser_lun) NE 0 THEN Free_Lun, parser_lun
	
end

;+
; =============================================================
;
; METHODNAME:
;       HEADER_PARSER::GET_SPECIES
;
; PURPOSE:
;       Returns the (trimmed) species as a hashtable
;
; CALLING SEQUENCE:
;
;       species_table = hp->get_species()
;       
; DESCRIPTION:
;
;      Just returns an internal object
;
; OPTIONAL INPUTS:
;       None.
;       
;
; KEYWORD PARAMETERS:
;       None.
;       
;     
;-
; =============================================================
function header_parser::get_species
; =============================================================

	return,self.species
end

;+
; =============================================================
;
; METHODNAME:
;       HEADER_PARSER::GET_REVERSED_SPECIES
;
; PURPOSE:
;       Returns the (trimmed) species as a reverted hashtable
;
; CALLING SEQUENCE:
;
;       species_table = hp->get_reverted_species()
;       
; DESCRIPTION:
;
;      Just returns an internal object
;
; OPTIONAL INPUTS:
;       None.
;       
;
; KEYWORD PARAMETERS:
;       None.
;       
;     
;-
; =============================================================
function header_parser::get_reversed_species
; =============================================================

	return,self.reversed_species
end
;+
; =============================================================
;
; METHODNAME:
;       HEADER_PARSER::GET_SPECIES_ARR
;
; PURPOSE:
;       Returns the (trimmed) species as an array
;
; CALLING SEQUENCE:
;
;       species_arr = hp->get_species_arr()
;       
; DESCRIPTION:
;
;      Just returns an internal object (dereferenced)
;
; OPTIONAL INPUTS:
;       None.
;       
;
; KEYWORD PARAMETERS:
;       None.
;       
;     
;-
; =============================================================
function header_parser::get_species_arr
; =============================================================

  ; we store a pointer, but the user expects to get an array,
  ; hence we need to dereference
  return,*self.arspec
end

;+
; =============================================================
;
; METHODNAME:
;       HEADER_PARSER::GET_SCALARS
;
; PURPOSE:
;       Returns the scalars as a hashtable
;
; CALLING SEQUENCE:
;
;       species_table = hp->get_scalars()
;       
; DESCRIPTION:
;
;      Just returns an internal object
;
; OPTIONAL INPUTS:
;       None.
;       
;
; KEYWORD PARAMETERS:
;       None.
;       
;     
;-
; =============================================================
function header_parser::get_scalars
; =============================================================

	return,self.scalars
	
end

;+
; =============================================================
;
; METHODNAME:
;       HEADER_PARSER::GET_HEADER_LENGTH
;
; PURPOSE:
;       Returns the length of the fixed header
;
; CALLING SEQUENCE:
;
;       header_length = hp->get_header_length()
;       
; DESCRIPTION:
;
;      Just returns an internal object
;
; OPTIONAL INPUTS:
;       None.
;       
;
; KEYWORD PARAMETERS:
;       None.
;       
;     
;-
; =============================================================
function header_parser::get_header_length
; =============================================================

	return,self.header_length
end

;+
; =============================================================
;
; METHODNAME:
;       HEADER_PARSER::GET_UPDATE_TIMES
;
; PURPOSE:
;       Returns an fltarr containing ibdate,btime,iedate,etime of each update time (using regex)
;
; CALLING SEQUENCE:
;
;       update_times = hp->get_update_times()
;       
; DESCRIPTION:
;
;      Performs regex on the whole file to extract the header of the 
;      time dependent entries.
;      We basically search for the string ibdate,btime,iedate,etime which
;      should be present in all supported file types.
;
; OPTIONAL INPUTS:
;       None.
;       
;
; KEYWORD PARAMETERS:
;       None.
;       
;     
;-
; =============================================================
function header_parser::get_update_times
; =============================================================

	if ( self.is_binary ) then begin
		print,'WRN: Cannot get update times for binary files'
		return,0
	endif
	
	; Default
	update_times=!VALUES.F_NAN
	
	; This is what we seek
	regex='^ {1,}[0-9]{4} {1,}[0-9]{1,2}\.[0-9]{0,2} {1,}[0-9]{4} {1,}[0-9]{1,2}\.[0-9]{0,2}$'
	; The format is strange
	fmt='(I-15,F10.2,I-15,F10.2)'
	
	n_lines=FILE_LINES(self.filename)
	
	; we read in the whole file
	data=strarr(n_lines)
	
	; Read and release file
	openr,parser_lun,self.filename,/GET_LUN
	print,'We parse the whole file, might take a while...'
	readf, parser_lun, data
	Free_Lun, parser_lun
	
	updates=stregex(data,regex,/EXTRACT)
	
	; Now we get as many entries as lines, 
	; get the non-empty ones
	ind=WHERE(updates NE '',count)
	
	if (count NE 0) then begin
		found_updates=updates[ind]
	
		n_updates=N_Elements(found_updates)
		
		update_times=fltarr(n_updates,4)
		
		for iupdate=0,n_updates-1 do begin
		
			; Parse and store the line
			reads,found_updates[iupdate],ibdate,btime,iedate,etime,format=fmt
			
			update_times[iupdate,0]=ibdate
			update_times[iupdate,1]=btime
			update_times[iupdate,2]=iedate
			update_times[iupdate,3]=etime
		
		endfor ; updates
		
	endif ; found anything?
	
	return,update_times
	
end

;+
; =============================================================
;
; METHODNAME:
;       HEADER_PARSER::CLEANUP
;
; PURPOSE:
;       De-allocates storage and cleans up a header_parser object.
;
; CALLING SEQUENCE:
;
;       OBJ_DESTROY, hp
;       
; DESCRIPTION:
;
;       This procedure performs all clean-up required to remove the
;       object.  All hash table entries are freed.  However, if any of
;       the contained objects are heap data or objects, the user is
;       responsible for freeing those pointers or objects.
;
; OPTIONAL INPUTS:
;       None.
;       
;
; KEYWORD PARAMETERS:
;       None.
;       
;
; EXAMPLE:
;       OBJ_DESTROY, hp
;       
;-
; =============================================================
pro header_parser::cleanup
; =============================================================
  COMPILE_OPT IDL2
  
  OBJ_DESTROY, self.scalars
  OBJ_DESTROY, self.species
  OBJ_DESTROY, self.reversed_species
  
  return
end


; =============================================================
; METHODNAME: HEADER_PARSER__DEFINE
;  internal method: defines header data structure (aka class)
pro header_parser__define
; =============================================================
	COMPILE_OPT IDL2
	
	struct = {header_parser, $
			scalars: obj_new(), $		;; Hash of scalars
			species: obj_new(), $		;; Hash of the species we have (species->index)
			reversed_species: obj_new(), $	;; Reverted species hash (index->species)
			file_types: obj_new(), $	;; Hash of the supported filetypes
			filename: "", $				;; The file we are looking at
			arspec: ptr_new(), $ 		;; the string array that holds the species
			header_length: 0, $ 		;; Length of the current "big header"
			is_binary: 0 $ 				;; Binary IO?
			}
  return
end