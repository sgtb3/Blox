
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "blox.h"

/**
 * len() - Returns the length of a given struct.
 * @arg arr_struct_ptr : Pointer to an Array struct type
 * 
 * Return: Length of array.
 */
int len(ptr_t arr_struct_ptr) 
{
  return (int) ( *((size_t *) arr_struct_ptr) );
}
