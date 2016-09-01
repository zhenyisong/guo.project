# this program is designed for Guo Tingwei Project
# to scan the txt file and calculate the frequency
# of SNPs in large human samples
# version 1.001
# this program use python 2.7 < 3.0

# python file_name col_index
# version 1.0001
import re
import os
import sys

# 7.2.5.8. Raw String Notation
# Raw string notation (r"text") keeps regular 
# expressions sane. Without it, every backslash ('\') 
# in a regular expression would have to be prefixed with 
# another one to escape it. For example, the two following 
# lines of code are functionally identical:

# @parameter
#   file_name   the file name of the working directory
#   col_index   the column index of the read column
# @return
# the list of the all sample_id with the 
# phenotype 
# sample_id is not duplicated
# sample_id is number, otherwise, it will 
# lose power

# how to use it
# python tingwei_fouth.py 1 02

def readGenomeData(file_name, col_index):
    file_handle      = open(file_name, 'rU')
    i                = int(col_index)
    result           = []
    for line in file_handle:
        matched_line = re.match(r"\d+",line)
        if matched_line:
            line = line.rstrip('\n')
            genome_line = re.split(",",line)
            #print(genome_line[0])
            if genome_line:
                result.append ([genome_line[0],
                                genome_line[i],
                                ])
    file_handle.close()
    return result

def findReference_locus(genome_data,list_index):
    last_index = len(genome_data)
    tag_dic    = {}
    tag_freq   = []
    ref_locus  = []
    list_index = int(list_index)
    for i in range(0,last_index):
        tag_pair = genome_data[i][list_index]
        
        if re.search("\?",tag_pair):
            continue
        # you need change here;
        tag_pair = re.split(r"_",tag_pair)
        if tag_dic.has_key(str(tag_pair[0])):
            num_string = str(tag_pair[0]) 
            tag_dic[num_string] += 1
        else:
            num_string = str(tag_pair[0]) 
            tag_dic[num_string] = 1
        
        if tag_dic.has_key(str(tag_pair[1])):
            num_string = str(tag_pair[1]) 
            tag_dic[num_string] += 1
        else:
            num_string = str(tag_pair[1]) 
            tag_dic[num_string] = 1
    tag_keys = tag_dic.keys()
    if not bool(tag_keys):
        return -1
    for i in tag_keys:
        tag_freq.append(tag_dic[i])
    max_num  = max(tag_freq)
    for i in tag_keys:
        if tag_dic[i] == max_num:
            ref_locus.append(i)
    return ref_locus

def findAllTags(genome_data,list_index):
    last_index = len(genome_data)
    tag_dic    = {}
    tag_freq   = []
    ref_locus  = []
    for i in range(0,last_index):
        tag_pair = genome_data[i][list_index]
        if re.search("\?",tag_pair):
            continue
        # you need modify here to _ or
        # / according to your output
        #
        tag_pair = re.split(r"_",tag_pair)
        if tag_dic.has_key(str(tag_pair[0])):
            num_string = str(tag_pair[0]) 
            tag_dic[num_string] += 1
        else:
            num_string = str(tag_pair[0])
            tag_dic[num_string] = 1
        
        if tag_dic.has_key(tag_pair[1]):
            num_string = str(tag_pair[1]) 
            tag_dic[num_string] += 1
        else:
            num_string = str(tag_pair[1]) 
            tag_dic[num_string] = 1
    tag_keys = tag_dic.keys()
    return tag_keys

if len(sys.argv) == 4:
    print("we now processing the data")
else:
    print("usage:")
    print("python python_script file_name col_index ref_tag")
    sys.exit(1)
 
working_dir          = '/home/zhenyisong/data/guotingwei'
file_name_inpath     = sys.argv[1]
os.chdir(working_dir)
column_index         = sys.argv[2]
ref_tag              = sys.argv[3]
sample_data_list     = readGenomeData(file_name_inpath,column_index)
all_tags_list        = findAllTags(sample_data_list,1)


#left_all_tags = []
#for i in all_tags_list:
#    if re.match(i,ref_tag):
#        continue
#    left_all_tags.append(i)
#
left_all_tags        = set(all_tags_list) - set([ref_tag])
print(all_tags_list)
print(ref_tag)
print(left_all_tags)


tagged_file_name     = ref_tag + '_' + str(column_index) + '.tag.guotingwei.txt';
numbered_file_name   = ref_tag + '_' + str(column_index) + '.nub.guotingwei.txt';
tagged_file_handle   = open(tagged_file_name,'w')
numbered_file_handle = open(numbered_file_name,'w')
matrix_result_1      = []
matrix_result_2      = []
formats              = ['%s']
seperator            = '\t'
    
for j in left_all_tags:
    formats.append('%s')
#print(len(formats))
format          = seperator.join(formats)
for j in left_all_tags:
    output_list_letter    = []
    output_list_number    = []
    for k in sample_data_list:
        tag_pair       = k[1]
        tag_pattern    = ref_tag + '_' + ref_tag
        if re.match(tag_pattern,tag_pair):
            output_list_letter.append('N/N')
            output_list_number.append('0')
            continue
        tag_pattern    = j + '_' + j
        if re.match(tag_pattern,tag_pair):
            pattern = j + '/' + j
            output_list_letter.append(pattern)
            output_list_number.append('2')
            continue
        tag_pattern    = ref_tag + '_' + j
        if re.match(tag_pattern,tag_pair):
            pattern = 'N/' + j
            output_list_letter.append(pattern)
            output_list_number.append('1')
            continue
        tag_pattern    = j + '_' + ref_tag
        if re.match(tag_pattern,tag_pair):
            pattern = 'N/' + j
            output_list_letter.append(pattern)
            output_list_number.append('1')
            continue
        if re.search("\?",tag_pattern):
            output_list_letter.append('')
            output_list_number.append('')
            continue
        tag_pattern    = ref_tag + '_'
        if re.match(tag_pattern,tag_pair):
                #tag_pair = re.split(r"_",tag_pattern)
            pattern = 'N/' + 'O'
            output_list_letter.append(pattern)
            output_list_number.append('0')
            continue
        tag_pattern    = '_' + ref_tag
        if re.search(tag_pattern,tag_pair):
                #tag_pair = re.split(r"_",tag_pattern)
            pattern = 'N/' + 'O'
            output_list_letter.append(pattern)
            output_list_number.append('0')
            continue
        tag_pattern    = j + '_'
        if re.search(tag_pattern,tag_pair):
      
            pattern = j + '/' + 'O'
            output_list_letter.append(pattern)
            output_list_number.append('1')
            continue
        tag_pattern    = '_' + j
        if re.search(tag_pattern,tag_pair):
            pattern = j + '/' + 'O'
            output_list_letter.append(pattern)
            output_list_number.append('1')
            continue
        pattern_letter = 'O' + '/' + 'O'
        pattern_list   = ''
        output_list_letter.append(pattern_letter)
        output_list_number.append(pattern_list)
    matrix_result_1.append(output_list_letter)
    matrix_result_2.append(output_list_number)

file_header = ['sample_id']
for j in left_all_tags:
    header_j = str(ref_tag) + '_' + j
    file_header.append(header_j)
file_header = format % tuple(file_header)
   
tagged_file_handle.write('%s\n' % file_header)
transpose_matrix = zip(*matrix_result_1)
line_count       = 0
for i in transpose_matrix:
    sample_id   = sample_data_list[line_count][0]
    data_list   = [sample_id]
    line_count += 1
    data_list.extend(i)
    
    data_tuple = tuple (data_list)
    #print(len(data_tuple))
    #data_tuple = str(data_tuple)
    line       = format % data_tuple
    tagged_file_handle.write('%s\n' % line)
 

numbered_file_handle.write('%s\n' % file_header)
transpose_matrix = zip(*matrix_result_2)
line_count       = 0
for i in transpose_matrix:
    sample_id   = sample_data_list[line_count][0]
    data_list   = [sample_id]
    line_count += 1
    data_list.extend(i)
    
    data_tuple = tuple (data_list)
    #print(len(data_tuple))
    #data_tuple = str(data_tuple)
    line       = format % data_tuple
    numbered_file_handle.write('%s\n' % line)
tagged_file_handle.close()
numbered_file_handle.close()