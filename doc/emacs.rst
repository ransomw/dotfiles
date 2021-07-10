`emacs`
=======

`emacs` assigns modes to buffers.
mode to buffer is many to one.

contrast with "mode" in `vim`,
where the user is in any particular mode
at any given time.



abbrev-mode
-----------

Abbreviation mode expands "abbreviations"
into "words."

Take care will whata "word" is:
`_dashbd_run` in region gives

>  "abbrev for 'run'"

not

>  "abbrev for '_dashboard_run`" ,

---

_confession_: I am guilty of a minimalist re-invention this wheel. in `emacs/defuns.el` as `add-abbreviation`/`get-abbreviation`.  I promise to copy that functionality over here to live as archaeology only - just as soon as I learn how to use `abbrev-mode`.

---

**good parts**

* **todo** : "rm" abbrev and/or rm all abbrevs
* `C-x a C-a`, `C-x a l` : `insert-mode-abbev` add abbreviation active only in current major mode


`*Help*` copy over.

```
Global Bindings Starting With C-x a:
key             binding
---             -------

C-x a C-a       add-mode-abbrev
C-x a '         expand-abbrev
C-x a +         add-mode-abbrev
C-x a -         inverse-add-global-abbrev

C-x a e         expand-abbrev
C-x a g         add-global-abbrev
C-x a i         Prefix Command
C-x a l         add-mode-abbrev
C-x a n         expand-jump-to-next-slot
C-x a p         expand-jump-to-previous-slot

C-x a i g       inverse-add-global-abbrev
C-x a i l       inverse-add-mode-abbrev
```


**todos**  copyover "hippie" abbrev and start using that, too


C foundation
------------


**`defun`**

_orign_

```
/* This version of DEFUN declares a function prototype with the right
   arguments, so we can catch errors with maxargs at compile-time.  */
#define DEFUN(lname, fnname, sname, minargs, maxargs, intspec, doc)	\
  SUBR_SECTION_ATTRIBUTE                                                \
  static union Aligned_Lisp_Subr sname =                                \
     {{{ PVEC_SUBR << PSEUDOVECTOR_AREA_BITS },				\
       { .a ## maxargs = fnname },                    \
       minargs, maxargs, lname, intspec, 0}};				\
   Lisp_Object fnname
/*
SUBR_SECTION_ATTRIBUTE possibly blank
*/
```

```
DEFUN ("consp", Fconsp, Sconsp, 1, 1, 0,
       doc: /* Return t if OBJECT is a cons cell.  */
       attributes: const)
  (Lisp_Object object)
{
  if (CONSP (object))
    return Qt;
  return Qnil;
}
```

_annot_

```
DEFUN (
       "consp", /* lname */
       Fconsp, /* fnname */
       Sconsp, /* sname */
       1, /*  minargs  */
       1, /*  maxargs */
       0, /* intspec */
       /* BEGIN doc (throwaway upon macro subst) */
       doc: /* Return t if OBJECT is a cons cell.  */
       attributes: const
       /* END doc */
       )
  (Lisp_Object object)
{
  if (CONSP (object))
    return Qt;
  return Qnil;
}
```

```
static union Aligned_Lisp_Subr Sconsp/**/ =
  {{
      { PVEC_SUBR << PSEUDOVECTOR_AREA_BITS },
      /* { .a ## maxargs = fnname } <<??????*/
      { .a ## 1/*maxargs*/ = Fconsp }, // ?????
      //\..\.\.\/\//\/\\/\/\/\\.......okok
      0/*minargs*/, 1/*maxargs*/, "consp", 0/*intspec*/, 0}
  };
Lisp_Object Fconsp
// end `DEFUN` macro
(Lisp_Object object)
{
  if (CONSP (object))
    return Qt;
  return Qnil;
}
```

conclude that DEFUN macro
1. declares asdf dsa
       static union Aligned_Lisp_Subr
   and assigns value
2. provides the name of function
   returning Lisp_Object

