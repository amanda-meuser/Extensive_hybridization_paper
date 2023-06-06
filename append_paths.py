#!/usr/bin/env python3
# adds a prefix and suffix to each line in a file (can def be changed to take command line args)
# USAGE: ./append_paths.py 

# create output file
newcontent = ""
# open input file
with open('2019_efishing_metadata_amymay26_1_jan23AM_CS-CC-BND_EGM19_ID_ONLY.txt') as textfile:
    # open and prepare to write to new file
    with open('2019_efishing_metadata_amymay26_1_jan23AM_CS-CC-BND_EGM19_ID_ONLY_paths.txt', 'w') as newfile:
        # add the stuff to each line and write to the output file
        for line in textfile:
            # line.strip() removes all newlines, as i was having an issue w windows \r\n newlines
            newcontent += '/project/rrg-emandevi/hybrid_ameuser/EGM19_cc/bwa/' + line.strip() + '.sorted.bam\n'
        newfile.write(newcontent)