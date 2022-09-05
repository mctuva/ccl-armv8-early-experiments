/* parses 2 command line arguments, passes them to assembly laguage, and returns result for $? */

int parseint (char* string)
{
    int integer = 0;
    for (char digit = *(string++); digit != '\0'; digit = *(string++)) {
      integer = (integer * 10) + (digit - '0');
    }
    return integer;
}

long int init_and_call_expt (long int a, long int b);

int main (int argc, char *argv[])
{
  if (argc == 3)
    return init_and_call_expt(parseint(argv[1]), parseint(argv[2]));
  else
    return -1;
}
