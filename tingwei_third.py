# this program is designed for Guo Tingwei Project
# to scan the txt file and calculate the frequency
# of SNPs in large human samples
# version 1.005
import re
import os
import sys

# 7.2.5.8. Raw String Notation
# Raw string notation (r"text") keeps regular 
# expressions sane. Without it, every backslash ('\') 
# in a regular expression would have to be prefixed with 
# another one to escape it. For example, the two following 
# lines of code are functionally identical:

def readGenomeData(file_name):
    file_handle      = open(file_name, 'rU')
    result           = []
    for line in file_handle:
        matched_line = re.match(r"\d+",line)
        if matched_line:
            genome_line = re.split(r",",line)
            #print(genome_line[0])
            if genome_line:
                result.append ([genome_line[0],
                                genome_line[1],
                                genome_line[2],
                                genome_line[3],
                                genome_line[4],
                                genome_line[5]])
    file_handle.close()
    return result

def findReference_locus(genome_data,list_index):
    last_index = len(genome_data)
    tag_dic    = {}
    tag_freq   = []
    ref_locus  = []
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


 
working_dir          = '/home/zhenyisong/data/guotingwei'
file_name_inpath     = 'test3.csv'
os.chdir(working_dir)
column_index         = 1
guo_csv              = readGenomeData(file_name_inpath)
ref_tags             = findReference_locus(guo_csv,column_index)
all_tags             = findAllTags(guo_csv,column_index)

if ref_tags == -1:
    print("this column has no max ref_locus")
    sys.exit(1)
for i in ref_tags:
    left_all_tags        = set(all_tags) - set(i)
    tagged_file_name     = i + '_' + str(column_index) + '.tag.guotingwei.txt';
    numbered_file_name   = i + '_' + str(column_index) + '.nub.guotingwei.txt';
    tagged_file_handle   = open(tagged_file_name,'w')
    numbered_file_handle = open(numbered_file_name,'w')
    matrix_result_1      = []
    matrix_result_2      = []
    formats              = ['%s']
    seperator            = '\t'
    
    for j in left_all_tags:
        formats.append('%s')
    print(len(formats))
    format               = seperator.join(formats)
    for j in left_all_tags:
        output_list_letter    = []
        output_list_number    = []
        for k in guo_csv:
            tag_pattern = k[column_index]
            tag_comb    = i + '_' + i
            if re.match(tag_comb,tag_pattern):
                output_list_letter.append('N/N')
                output_list_number.append('0')
                continue
            tag_comb    = j + '_' + j
            if re.match(tag_comb,tag_pattern):
                pattern = j + '/' + j
                output_list_letter.append(pattern)
                output_list_number.append('2')
                continue
            tag_comb    = i + '_' + j
            if re.match(tag_comb,tag_pattern):
                pattern = 'N/' + j
                output_list_letter.append(pattern)
                output_list_number.append('1')
                continue
            tag_comb    = j + '_' + i
            if re.match(tag_comb,tag_pattern):
                pattern = 'N/' + j
                output_list_letter.append(pattern)
                output_list_number.append('1')
                continue
            if re.search("\?",tag_pattern):
                output_list_letter.append('')
                output_list_number.append('')
                continue
            tag_comb    = i + '_'
            if re.match(tag_comb,tag_pattern):
                #tag_pair = re.split(r"_",tag_pattern)
                pattern = 'N/' + 'O'
                output_list_letter.append(pattern)
                output_list_number.append('0')
                continue
            tag_comb    = '_' + i
            if re.search(tag_comb,tag_pattern):
                #tag_pair = re.split(r"_",tag_pattern)
                pattern = 'N/' + 'O'
                output_list_letter.append(pattern)
                output_list_number.append('0')
                continue
            tag_comb    = j + '_'
            if re.match(tag_comb,tag_pattern):
                tag_pair = re.split(r"_",tag_pattern)
                pattern = j + '/' + 'O'
                output_list_letter.append(pattern)
                output_list_number.append('1')
                continue
            tag_comb    = '_' + j
            if re.search(tag_comb,tag_pattern):
                tag_pair = re.split(r"_",tag_pattern)
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
        header_j = str(i) + '_' + j
        file_header.append(header_j)
    file_header = format % tuple(file_header)
   
    tagged_file_handle.write('%s\n' % file_header)
    transpose_matrix = zip(*matrix_result_1)
    line_count       = 0
    for i in transpose_matrix:
        sample_id   = guo_csv[line_count][0]
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
        sample_id   = guo_csv[line_count][0]
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