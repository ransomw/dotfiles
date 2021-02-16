#include <stdlib.h>
#include <libguile.h>

static SCM
my_username(void)
{
  char *s = getenv("USER");
  if(s == NULL)
    return SCM_BOOL_F;
  else
    return scm_from_locale_string(s);
}

static void
inner_main(void *data, int argc, char **argv)
{
  scm_c_define_gsubr("my-username", 0, 0, 0, my_username);
  scm_shell(argc, argv);
}

int
main(int argc, char **argv)
{
  scm_boot_guile(argc, argv, inner_main, 0);
  return 0; /* never reached */
}
