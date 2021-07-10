"""
deriving yaml.YamlObjectclass

### todo

* describe differences between module- or defn-level `class`
  declaration and type()-instantiated objects, particularly
  methods (if any)
* answer questions in original motivating example from `yaml`
"""
import random

##
# the usual

class TableSeasoning:
    pass


class Salt(TableSeasoning):
    has_iodine = True

    def iodine_amount(self):
        def _add_units(amt):
            return {"parts_per": amt}

        if self.has_iodine:
            return _add_units(random.uniform(3,5))
        return _add_units(0)


class Pepper(TableSeasoning):
    pass


##
# a prelude

# found this bowl of methods defns in the fridge.
# needed them to keep, so they're all in ziplocs
# ziplocs made of function ASTs.


def _nutrition_facts(self,):
    """ needs to be a method because we just make it all up """
    if self._nutrition_facts is None:
        self._nutrition_facts = {
            'calories': random.normalvariate(10, 20),
            'protien': round(random.uniform(10,13)),
        }
    return self._nutrition_facts


def _label_text(self):
    label_width = len("+----------------------+")
    brand_name = self.brandname
    len_underline = label_width - 2
    return """
+----------------------+
|                      |
|       {lab}          |
{lab_underline}
|                      |
{nutri_facts}
    """.format(
        lab=brand_name,
        lab_underline='|'+'-'*len_underline+'|',
        nutri_facts=self.nutrition_facts(),
    )

def _print_label(self):
    raise Exception()


##
# a *blithering* path
#
def type_first():
    """
    type() is a confusing sort-of
    varadic function:  it's 1-ary and 3-ary.
    And, the two arities result in drastically different outcomes
    ...
    """
    on_the_unary_type()
    on_the_ternary_type()


def on_the_unary_type():
    """
    unary `type(obj)` returns an objects type.
    what a type is is a much deeper question.
    see module-lvl comments.

    unary type() calls on an instance of a custom class result in the instance's class, and passing a custom class instance to `type` results in `type` itself.

    todo:
    it is a rule that successive type() calls always, eventually, return `type` itself, so that understanding the unqry type is instructed by describing whn type(arg) does _not_ return  `type`.
    """
    salt = Salt()
    assert type(salt) == Salt
    salt_inheritance_sequence = inheritance_sequence(salt)
    assert salt_inheritance_sequence[0] == Salt
    assert salt_inheritance_sequence[1] == type
    assert inheritance_depth(salt) == 2
    #
    table_seasoning = TableSeasoning()
    table_seasoning_inheritance_sequence = inheritance_sequence(table_seasoning)
    assert table_seasoning_inheritance_sequence == [TableSeasoning, type]


def inheritance_sequence(obj):
    """
    todo: possible misnomer.  do not understand meaning of the successive type() call rule.
    """
    def _helper(obj, seq_so_far):
        if obj == type:
            return seq_so_far
        else:
            type_of_obj = type(obj)
            next_seq_so_far = seq_so_far + [type_of_obj]
            return _helper(type_of_obj, next_seq_so_far)

    return _helper(obj, [])


def inheritance_depth(obj):
    """
    todo: possible misnomer.
"""
    return len(inheritance_sequence(obj))


def on_the_ternary_type():
    """
    NB. sig type^3
               type(name, bases, dic[t])

    ternary type() is akin to class creation
recall:  "blocks" in AST correspond to "execution frames" in interpreter {not program} memory segment, and the E.F. of each module-level class is run upon its module's import.  the locals of that execution frame a stored in a dictionary, so that attribute access is very close to looking up keys in the dictionary.. **todo** seperate section on plain-old classes.
    the third param/arg to 3-ary type() is analogous to the `dict` that a custom class's executrion frame's locals are crammed into upon class definition.  meanwhile -- and proceeding in reverse-order -- the second is much alike with the inheritance (ie parent-class csv) in a custom class declaration/instantiation.

3-`type` is "dynamic" (as constrasted static?) in the sense that declaration is seperated from instantiation.
    """
    name = 'DryRubClassALike' #~ for pork ribs
    bases = [Salt, Pepper]
    brandname = "Rendez"
    dict_ = {
        'brandname': brandname,
        '_nutrition_facts': None,
        "nutrition_facts": _nutrition_facts,
        "label_text": _label_text,
        "print_label": _print_label,
    }
    DryRub = type_(name, bases, dict_)
    assert DryRub.__bases__[0] == Salt
    assert DryRub.__bases__[1] == Pepper
    assert DryRub.__dict__['brandname'] == "Rendez"
    try:
        Salt.__bases__
    except AttributeError:
        assert False
    ##
    dry_rub = DryRub()
    calories = dry_rub.nutrition_facts()['calories']
    label_text = dry_rub.label_text()
    assert (brandname in label_text
            and
            '{}'.format(calories) in label_text
            )
    #
    salt = Salt()
    salt.iodine_amount()['parts_per']
    def builtin_names(name):
        return set([ name for name in dir(salt)
                  if
                     name.startswith('__') and name.endswith('__')
                    ])

    assert builtin_names(salt).difference(
        builtin_names(dry_rub)) == set()
    assert builtin_names(dry_rub).difference(
        builtin_names(salt)) == set()



def type_(*args):
    """
    3-ary type's 2nd arg must be a tuple.
    lists, iterators, and generators are
    not coerced automatically
    """
    if len(args) == 3:
        name, bases, dict_ = args
        return type(name, tuple(bases), dict_)
    return type(*args)


##
# questions still ununderstood


class YamObjectMetaclass(type):
    """
    inherit from `type`.
    that's /why/ it's a metaclass, but
    todo: /what/
    _is_ a metaclass?  still another.
    """

    def __init__(cls, name, bases, kwds):
        (super(YamObjectMetaclass, cls)
         .__init__(name, bases, kwds))



class YamObjectClass(YamObjectMetaclass):
    pass


def meta_question():
    yam_object_metaclass = YamObjectMetaclass()
    yam_object_metametaclass = YamObjectMetametaclass()

