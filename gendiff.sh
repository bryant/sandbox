if [ $# -ne 1 ] || [ "$1" = "--help" ]
then
    echo "Usage: $0 base_rev"
    exit -1
fi

baserev="$1"
cur=`git branch | grep '^*'`
if [ -n "`echo "$cur" | grep -o detached`" ]
then
    cur=`echo "$cur" | grep -o 'from [0-9a-f]\+' | sed -e 's#from ##g'`
else
    cur=`echo "$cur" | sed -e 's#^* ##g'`
fi
diff="`mktemp -p . --suffix=.diff`"
tempbranch="`basename $diff`"
rm -f "$diff"
git checkout -b $tempbranch &&
git filter-branch --commit-filter '\
    read msg; \
    if [ -n "`echo $msg | grep "\[exclude\]"`" ]; \
    then \
        skip_commit "$@"; \
    else \
        echo $msg | git commit-tree "$@"; \
    fi' "$baserev".."$tempbranch" &&
git diff > "$diff" --unified=11111111 "$baserev" "$tempbranch" -- &&
git checkout "$cur" &&
git branch -D "$tempbranch" &&
git update-ref -d "refs/original/refs/heads/$tempbranch"
