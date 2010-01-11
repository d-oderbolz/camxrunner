#!/usr/bin/env bash
#
# Script to refresh the NaturalDocs API documentation.
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
#
# Version: $Id$ 
# 
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# Who	When			What
# dco 16.10.08	Created


# Ok, a bit of mindstretching to allow relative operation.
MYDIR=$(dirname $0)

cd $MYDIR
NaturalDocs/NaturalDocs  -r -i .. -xi . -xi ../lib -o HTML ../doc/api/ -p ../doc/api/conf -img ../doc/api/img
