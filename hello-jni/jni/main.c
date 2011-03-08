#include <ecl/ecl.h>

#ifdef __cplusplus
#define ECL_CPP_TAG "C"
#else
#define ECL_CPP_TAG
#endif

extern ECL_CPP_TAG void init_lib_SOCKETS(cl_object);
extern ECL_CPP_TAG void init_lib_BYTECMP(cl_object);


#define compiler_data_text NULL
#define compiler_data_text_size 0
#define VV NULL
#define VM 0
#ifdef __cplusplus
extern "C"
#endif
ECL_DLLEXPORT
void init_ECL_PROGRAM(cl_object cblock)
{
	static cl_object Cblock;
        if (!FIXNUMP(cblock)) {
		Cblock = cblock;
		cblock->cblock.data_text = compiler_data_text;
		cblock->cblock.data_text_size = compiler_data_text_size;
#ifndef ECL_DYNAMIC_VV
		cblock->cblock.data = VV;
#endif
		cblock->cblock.data_size = VM;
		return;
	}
#if defined(ECL_DYNAMIC_VV) && defined(ECL_SHARED_DATA)
	VV = Cblock->cblock.data;
#endif
	
{
	cl_object current, next = Cblock;
	current = read_VV(OBJNULL, init_lib_SOCKETS); current->cblock.next = next; next = current; 
	current = read_VV(OBJNULL, init_lib_BYTECMP); current->cblock.next = next; next = current; 

	Cblock->cblock.next = current;

}
	
}
int
main(int argc, char **argv)
{
	cl_boot(argc, argv);

	CL_CATCH_ALL_BEGIN(ecl_process_env()) {	
		read_VV(OBJNULL, init_ECL_PROGRAM);
		{ 
			const char *lisp_code = 
				"(SI:TOP-LEVEL T) ";
			cl_object output;
			si_select_package(make_simple_base_string("CL-USER"));
			output = si_safe_eval(2, ecl_read_from_cstring(lisp_code), Cnil);
		}
	} CL_CATCH_ALL_END;

	si_exit(0);
}
