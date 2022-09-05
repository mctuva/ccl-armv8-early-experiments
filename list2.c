/* Calls assembly laguage, and returns just the tag bits.
   11=NIL, 14=T (probably)
   Use 'echo $?' to see result */

long int init_and_call_list2 ();

int main (int argc, char *argv[])
{
  return init_and_call_list2() & 0xF;  /* just return tag bits: 14=pass, 11=fail */
}
