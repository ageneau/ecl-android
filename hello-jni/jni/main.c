#include <assert.h>
#include <stdio.h>
#include "ecl_boot.h"


int
main()
{
	fprintf(stderr,"STARTING ECL\n");
	ecl_boot("/data/data/com.example.hellojni/app_resources");
	fprintf(stderr,"ECL STARTED\n");

	return 0;
}
