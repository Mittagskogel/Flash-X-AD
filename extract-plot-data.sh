#!/usr/bin/env sh

# matchfields="dens\|eint\|ener\|game\|pres\|temp\|velx"
# matchfields="dens\|eint\|ener\|game\|pres\|temp\|velx\|vely"
# matchfields="c12"
# matchfields="velx"

matchfields="$1"


# offsets=(0 1 2 3 4)
# mantissas=($(seq 4 32))

# Autodetect offset and mantissa ranges:
offsets=($(ls sfocu_ref*.out | sed 's/sfocu_ref\([0-9]\).out/\1/g'))
mantissas=()

for offset in ${offsets[@]}
do
    mantissas+=($(grep object_ref[0-9] ./sfocu_ref${offset}.out | \
        sed 's/.*object_ref[0-9]*_\([0-9]*\).*/\1/g'))
done
mantissas=($(printf "%s\n" "${mantissas[@]}" | sort -ug))


echo -n 'mantissa '

for offset in ${offsets[@]}
do
    # echo -n $(grep -A14 '| L1-ErrNorm |' ./sfocu_ref${offset}.out | tail -n 11 | grep ${matchfields} | \
    #     sed 's/ .*//' | sed 's/ /'${offset}' /g' | sed 's/$/'${offset}'/g')" "
    # echo -n $(grep -A21 '| L1-ErrNorm |' ./sfocu_ref${offset}.out | tail -n 18 | grep ${matchfields} | \
    #     sed 's/ .*//' | sed 's/ /'${offset}' /g' | sed 's/$/'${offset}'/g')" "
    echo -n $(sed -n '/| L1-ErrNorm |/,/FAILURE\|SUCCESS/p; /FAILURE\|SUCCESS/q' ./sfocu_ref${offset}.out | \
        grep ${matchfields} | sed 's/ .*//' | sed 's/ /-'${offset}' /g' | sed 's/$/-'${offset}'/g')" "
done
echo

for m in ${mantissas[@]}
do
    echo -n "${m} "
    for offset in ${offsets[@]}
    do
        # echo -n $(grep -A 51 'object_ref'${offset}'_'${m}'bit' ./sfocu_ref${offset}.out | \
        # grep -A14 '| L1-ErrNorm |' | grep 'velz' -B 10 | grep ${matchfields} | \
        #     sed 's/[a-z]* | \([0-9.e\-]*\) .*/\1/')" "
        # echo -n $(grep -A 63 'object_ref'${offset}'_'${m}'bit' ./sfocu_ref${offset}.out | \
        # grep -A21 '| L1-ErrNorm |' | grep 'vlza' -B 18 | grep ${matchfields} | \
        #     sed 's/[a-z]* | \([0-9.e\-]*\) .*/\1/')" "
        # echo -n $(grep -A 77 'object_ref'${offset}'_'${m}'bit' ./sfocu_ref${offset}.out | \
        # grep -A28 '| L1-ErrNorm |' | grep 'ti44' -B 25 | grep ${matchfields} | \
        #     sed 's/[a-z 1-9]* | \([0-9.e\-]*\) .*/\1/')" "
        # echo -n $(grep -A 77 'object_ref'${offset}'_'${m}'bit' ./sfocu_ref${offset}.out | \
        # grep -A28 '| L1-ErrNorm |' | grep 'visc' -B 25 | grep ${matchfields} | \
        #     sed 's/[a-z 1-9]* | \([0-9.e\-]*\) .*/\1/')" "
        echo -n $(sed -n '/object_ref'${offset}'_'${m}'bit/,/FAILURE\|SUCCESS/p' ./sfocu_ref${offset}.out |\
            sed -n '/| L1-ErrNorm |/,/FAILURE\|SUCCESS/p' | grep ${matchfields} | \
            sed 's/[a-z 1-9]* | \([0-9.e\-]*\) .*/\1/')" "

        # Detect missing data
        grep 'object_ref'${offset}'_'${m}'bit' ./sfocu_ref${offset}.out > /dev/null
        if [ $? -eq 1 ]
        then
           echo -n "NaN "
        fi
    done
    echo
done

echo
