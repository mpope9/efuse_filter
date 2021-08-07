/**
 * @brief An Erlang NIF wrapper for binary_fuse8_filters.
 *
 * Heavily inspired from: https://github.com/mpope9/exor_filter/blob/master/c_src/xor_filter_nif.c
 *
 */
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <limits.h>
#include "erl_nif.h"

// This forward declaration is needed because we're using this
// function to override binary_fuse8_filter's use of malloc.
void * nif_zalloc(size_t size);
void * nif_zcalloc(size_t num, size_t size);
#define malloc(size) nif_zalloc(size)
#define calloc(num, size) nif_zcalloc(num, size)
#define free(size) enif_free(size)
#include "binaryfusefilter.h"

static ErlNifResourceType* fuse8_resource_type;

/*
 * Prototypes
 */
void destroy_fuse8_filter_resource(ErlNifEnv* env, void* obj);
ErlNifResourceType* fuse8_filter_resource_type(ErlNifEnv* env);

// portable encoding/decoding helpers
/*
static void
unpack_le_u64(uint64_t * dst, uint8_t const * src) {
    *dst = ((uint64_t)src[7] << 56) | ((uint64_t)src[6] << 48)
           | ((uint64_t)src[5] << 40) | ((uint64_t)src[4] << 32)
           | ((uint64_t)src[3] << 24) | ((uint64_t)src[2] << 16)
           | ((uint64_t)src[1] << 8) | (uint64_t)src[0];
}

static void
pack_le_u64(uint8_t * dst, uint64_t val) {
    dst[0] = val & 0xff;
    dst[1] = (val >> 8) & 0xff;
    dst[2] = (val >> 16) & 0xff;
    dst[3] = (val >> 24) & 0xff;
    dst[4] = (val >> 32) & 0xff;
    dst[5] = (val >> 40) & 0xff;
    dst[6] = (val >> 48) & 0xff;
    dst[7] = (val >> 56) & 0xff;
}
*/

// Allocates 'size' zeroized bytes from the VM.
//
// Erlang does not provide a malloc like function which returns
// zeroized memory. That's not usually a problem, as you can always
// call memset after allocation memory. However, we're overriding the
// fusefilters's use malloc by redefining it before including
// binaryfusefilter.h, leaving us no other option than to the VMs allocator
// and memset into this function.
void *
nif_zalloc(size_t size)
{
    void * mem = enif_alloc(size);
    if (mem) {
        memset(mem, 0, size);
    }
    return mem;
}

void *
nif_zcalloc(size_t num, size_t size)
{
    return nif_zalloc(size * num);
}

void
destroy_fuse8_filter_resource(ErlNifEnv* env, void* obj)
{
    binary_fuse8_t* filter = (binary_fuse8_t*) obj;

    binary_fuse8_free(filter);
}

ErlNifResourceType*
fuse8_filter_resource_type(ErlNifEnv* env)
{
   return enif_open_resource_type(
      env,
      NULL,
      "fuse8_filter_resource",
      destroy_fuse8_filter_resource,
      ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
      NULL
   );
}


/**
 * Creates a new atom.
 */
static ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
   ERL_NIF_TERM ret;

   if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
   {
      return enif_make_atom(env, atom);
   }

   return ret;
}

/**
 * Creates an error tuple.
 */
static ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* mesg)
{
   return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

/**
 * Fills the passed buffer. It must be pre-initialized.
 */
static int
fill_buffer(uint64_t* buffer, ErlNifEnv* env, ERL_NIF_TERM list)
{

   if(!buffer)
   {
      return false;
   }

   ERL_NIF_TERM head;
   uint64_t current = 0;
   for(int i = 0; enif_get_list_cell(env, list, &head, (ERL_NIF_TERM*) &list); i++)
   {
      if(!enif_get_uint64(env, head, &current))
      {
         return false;
      }
      buffer[i] = current;
   }
   return true;
}


/**
 * Initializes a filter from hashed elements.
 */
static ERL_NIF_TERM
fuse8_initialize_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
   ERL_NIF_TERM is_list = argv[0];
   uint32_t list_length;
   uint64_t* value_list;

   if(argc != 1)
   {
      return enif_make_badarg(env);
   }

   if(!enif_is_list(env, is_list))
   {
      return enif_make_badarg(env);
   }

   if(!enif_get_list_length(env, argv[0], &list_length))
   {
      return mk_error(env, "get_list_length_error");
   }

   value_list = enif_alloc(sizeof(uint64_t) * list_length);

   if(value_list == NULL)
   {
      return mk_error(env, "could_not_allocate_memory_error");
   }

   if(!(fill_buffer(value_list, env, argv[0])))
   {
      enif_free(value_list);
      return mk_error(env, "convert_to_uint64_t_error");
   }

    binary_fuse8_t* filter = 
        enif_alloc_resource(fuse8_resource_type, sizeof(binary_fuse8_t));

   if(!binary_fuse8_allocate(list_length, filter))
   {
      enif_free(value_list);
      enif_release_resource(filter);
      return mk_error(env, "fuse8_allocate_error");
   }

   if(!binary_fuse8_populate(value_list, list_length, filter))
   {
      enif_free(value_list);
      binary_fuse8_free(filter);
      enif_release_resource(filter);
      return mk_error(env, "duplicates_in_hash_error");
   }

   enif_free(value_list);

   ERL_NIF_TERM res = enif_make_resource(env, filter);
   // release this resource now its owned by Erlang
   enif_release_resource(filter);
   return res;
}

/**
 * Tests for membership.
 */
static ERL_NIF_TERM
fuse8_contain_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

   if(argc != 2)
   {
      return enif_make_badarg(env);
   }

   ErlNifUInt64 key;
   if(!enif_get_uint64(env, argv[1], &key)) 
   {
      return mk_error(env, "get_key_for_contains_error");
   }

   binary_fuse8_t* filter;
   if(!enif_get_resource(env, argv[0], fuse8_resource_type, (void**) &filter)) 
   {
       // TODO: add serialization
       return mk_error(env, "get_filter_error");
       /*
       ErlNifBinary bin;
       if (!enif_inspect_binary(env, argv[0], &bin)) {
           return mk_error(env, "get_filter_for_contains_error");
       }

       if (bin.size < sizeof(uint64_t) * 2) {
           return mk_error(env, "get_filter_for_contains_bin_wrong_size");
       }

       binary_fuse8_t stack_filter;

       unpack_le_u64(&stack_filter.seed, bin.data);
       unpack_le_u64(&stack_filter.blockLength, bin.data+sizeof(uint64_t));

       if (bin.size != (sizeof(uint64_t)*2) + (stack_filter.blockLength * 3)) {
           return mk_error(env, "get_filter_for_contains_bin_wrong_size");
       }
       stack_filter.fingerprints = bin.data + (sizeof(uint64_t) * 2);
       if(fuse8_contain(key, &stack_filter))
       {
           return mk_atom(env, "true");
       }
       else
       {
           return mk_atom(env, "false");
       }
       */
   }

   if(binary_fuse8_contain(key, filter)) 
   {
      return mk_atom(env, "true");
   }
   else 
   {
      return mk_atom(env, "false");
   }
}

static int
nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
   fuse8_resource_type = fuse8_filter_resource_type(env);
   return 0;
}

static ErlNifFunc nif_funcs[] = {

   {"fuse8_initialize_nif", 1, fuse8_initialize_nif},
   {"fuse8_initialize_nif_dirty", 1, fuse8_initialize_nif,
      ERL_NIF_DIRTY_JOB_CPU_BOUND},
   {"fuse8_contain_nif", 2, fuse8_contain_nif}
   //{"fuse8_to_bin_nif", 1, fuse8_to_bin_nif},
   //{"fuse8_from_bin_nif", 1, fuse8_from_bin_nif},

};

ERL_NIF_INIT(efuse_filter, nif_funcs, nif_load, NULL, NULL, NULL);
