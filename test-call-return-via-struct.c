/* we deliberately chose not to #include anything so we can understand the .s output */
/* file: call-return-struct-slot.c */

struct fn_object {       /* a struct of one slot, with a */
  int (*code)();         /* pointer to a function that returns an int */
  int retval;            /* an integer */
};

int test_fn ();          /* prototype for forward reference */

/* const - was causing test_fn to optimize away the slot dereference */
struct fn_object fn =   /* a globally (file scope) allocated struct */
  {
    test_fn,
    43
  };

int test_fn ()
{
  return fn.retval;             /* shell variable $? should contain this value */
}

int
main ()
{
  /* fn.code = test_fn; */    /* store address of test_fn in struct slot */
  return (*fn.code)();   /* call it through the slot and return its return value */
}

