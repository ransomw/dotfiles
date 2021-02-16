#! /usr/bin/env zsh

setopt ERR_EXIT
setopt PIPE_FAIL

dir_a=$1
dir_b=$2

if [[ ! -d $dir_a ]] || \
       [[ ! -d $dir_b ]]; then
    1>&2 print -- \
         "usage: $0 dir1 dir2"
fi

# changing the current working
# directory is a hacky way to do this.
# might be better to pipe find output
# to sed.
curr_dir=$(pwd)
cd $dir_a
list_str_a=$(find . -type f)
cd $curr_dir
cd $dir_b
list_str_b=$(find . -type f)
cd $curr_dir

list_a=(${(f)list_str_a})
list_b=(${(f)list_str_b})

if false; then
    print -- 'list a'
    print -- '======'
    print -l -- $list_a
    print -- 'list b'
    print -- '======'
    print -l -- $list_b
    exit 0
fi

##
# alternate, Zsh-specific
# and shell-only
# implementation of usual
# set-theoretic operations
# more often implemented via
# *nix utility programs.
union=()
a_minus_b=()
for file_a in $list_a; do
    if (($list_b[(Ie)$file_a])); then
        union=($union
               $file_a)
    else
        a_minus_b=($a_minus_b
                   $file_a)
    fi
done
b_minus_a=()
for file_b in $list_b; do
    if ! (($list_b[(Ie)$file_a])); then
        b_minus_a=($b_minus_a
                   $file_b)
    fi
done

union_same=()
union_different=()
for file in $union; do
    if cmp $dir_a/$file \
           $dir_b/$file \
       &>/dev/null; then
        union_same=(
            $union_same
            $file)
    else
        union_different=(
            $union_different
            $file)
    fi
done


print -- 'same files'
print -- '----------'
print -l -- $union_same


print -- 'different files'
print -- '---------------'
print -l -- $union_different

print -- "files in $dir_a and" \
      " not in $dir_b"
print -- "-------------------" \
      "--------------"
print -l -- $a_minus_b

print -- "files in $dir_b and" \
      " not in $dir_a"
print -- "-------------------" \
      "--------------"
print -l -- $b_minus_a
