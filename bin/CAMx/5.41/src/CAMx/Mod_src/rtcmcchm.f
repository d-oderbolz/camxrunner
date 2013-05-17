      Module rtcmcchm
      include 'rtcmcchm.inc'
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c        This allocates the dynamic memory arrays in the RTCMCCHM.COM
c        include file.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
      Contains
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_RTCMC
c-----------------------------------------------------------------------
c
         subroutine alloc_rtcmc(numtracs,numspec,numrxn,
     &                          numkprm,numphot,numzen,numrct,numprd,
     &                          numjactrm,numeqm,numslo)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
         use camx_includes
         implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c        numtracs   I  number of RTCMC species
c        numspec    I  number of core model species
c        numrxn     I  number of RTCMC reactions
c        numkprm    I  number of rate constant parameters
c        numphot    I  number of photolysis reactions
c        numzen     I  number of photolysis reaction zenith angles
c        numrct     I  number of reactants in each reaction
c        numprd     I  number of products in each reaction
c        numjactrm  I  number of Jacobian terms (an estimate)
c        numeqm     I  number of equilibrium species
c        numslo     I  number of slow species
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numtracs
         integer :: numspec
         integer :: numrxn
         integer :: numkprm
         integer :: numphot
         integer :: numzen
         integer :: numrct
         integer :: numprd
         integer :: numjactrm
         integer :: numeqm
         integer :: numslo
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         allocate( ityprtc  (numrxn) )
         allocate( nrkprm   (numrxn) )
         allocate( rkprmrtc (numrxn,numkprm) )
c
         allocate( ijschm   (numphot) )
         allocate( zenschm  (numzen) )
         allocate( rjschm   (numzen,numphot) )
c
         allocate( itypsp(numtracs+numspec) )
         allocate( itypschm(numtracs+numspec) )
         allocate( lblrxn(numrxn) )
         allocate( nrct(numrxn) )
         allocate( nprd(numrxn) )
         allocate( idxrct(numrxn,numrct) )
         allocate( idxprd(numrxn,numprd) )
         allocate( conschm(numtracs+numspec) )
         allocate( depvschm(numtracs+numspec) )
         allocate( wetschm(numtracs+numspec) )
         allocate( spdcoef(numrxn,numprd) )
         allocate( dryrtc(numtracs) )
         allocate( wetrtc(numtracs) )
         allocate( prdcoef(numrxn,numprd) )
         allocate( spnmrt(numtracs+numspec) )
         allocate( spnmschm(numtracs+numspec) )
         allocate( namrct(numrxn,numrct) )
         allocate( namprd(numrxn,numprd) )
c
         allocate( nrctfst(numrxn) )
         allocate( nprdfst(numrxn) )
         allocate( idxrctfst(numrxn,numrct) )
         allocate( idxprdfst(numrxn,numprd) )
         allocate( prdcofst(numrxn,numprd) )
c
         allocate( ipd(numjactrm) )
         allocate( jpd(numjactrm) )
         allocate( idrxjac(numjactrm) )
         allocate( nspjac(numjactrm) )
         allocate( idspjac(numjactrm,numrct-1) )
         allocate( scoefjac(numjactrm) )
         allocate( coefjac(numjactrm) )
c
         allocate( idslo(numslo) )
         allocate( nslgain(numslo) )
         allocate( nslloss(numslo) )
         allocate( islgain(numslo,numrxn) )
         allocate( islloss(numslo,numrxn) )
         allocate( spdcoslo(numslo,numrxn) )
         allocate( prdcoslo(numslo,numrxn) )
c
         allocate( ideqm(numeqm) )
         allocate( nrxgain(numeqm) )
         allocate( nrxloss(numeqm) )
         allocate( irxgain(numeqm,numrxn) )
         allocate( irxloss(numeqm,numrxn) )
         allocate( irxupdt(numrxn) )
         allocate( spdcoeqm(numeqm,numrxn) )
         allocate( prdcoeqm(numeqm,numrxn) )
c
         allocate( idxfix(numtracs+numspec) )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_RTCMC
c-----------------------------------------------------------------------
c
      end Module
