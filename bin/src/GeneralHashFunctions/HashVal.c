/*
 **************************************************************************
 *																								*
 *			  General Purpose Hash Function Algorithms Test					 *
 *																								*
 * Author: Arash Partow - 2002  (modified by Daniel Oderbolz)				 *
 * URL: http://www.partow.net															*
 * URL: http://www.partow.net/programming/hashfunctions/index.html		  *
 *																								*
 * Copyright notice:																		*
 * Free use of the General Purpose Hash Function Algorithms Library is	 *
 * permitted under the guidelines and in accordance with the most current *
 * version of the Common Public License.											 *
 * http://www.opensource.org/licenses/cpl1.0.php								  *
 *																								*
 **************************************************************************
*/



#include <stdio.h>
#include <stdlib.h>
#include "GeneralHashFunctions.h"


int main(int argc, char* argv[])
{
	// The user must pass
	// a string to hash
	// the max length of the hashtable
	// optional: the number of the hash function
	
	char* key;
	unsigned int maxent;
	int fct;
	int len;
	
	// We need to check a couple of things here...
	key=argv[1];
	
	// Lenght
	len=strlen(argv[1]);
	
	// We need to read the integer from the string parameter
	sscanf(argv[2], "%u", &maxent);
	
	printf("Maxent: %u\n",maxent);
	printf("Key: %s\n",key);
	
	// DBJ is the default Hash function
	if ( argc == 3 ) 
	{
		fct=7;
	}
	else 
	{
		sscanf(argv[3], "%d", &fct);
	}
	

	switch (fct){

		case 1: 	printf("%u",RSHash(key,len) % maxent);
							break;
		case 2:		printf("%u",JSHash(key,len) % maxent);
							break;
		case 3:		printf("%u",PJWHash(key,len) % maxent);
							break;
		case 4:		printf("%u",ELFHash(key,len) % maxent);
							break;
		case 5:		printf("%u",BKDRHash(key,len) % maxent);
							break;
		case 6:		printf("%u",SDBMHash(key,len) % maxent);
							break;
		case 7:		printf("%u",DJBHash(key,len) % maxent);
							break;
		case 8:		printf("%u",DEKHash(key,len) % maxent);
							break;
		case 9:		printf("%u",BPHash(key,len) % maxent);
							break;
		case 10:	printf("%u",FNVHash(key,len) % maxent);
							break;
		case 11:	printf("%u",APHash(key,len) % maxent);
							break;
		default:	printf("%u",DJBHash(key,len) % maxent);
	}

	exit(EXIT_SUCCESS);
	return 1;

}
