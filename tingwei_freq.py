# this program is designed for Guo Tingwei Project
# to scan the txt file and calculate the frequency
# of SNPs in large human samples
# version 1.02
# this version is derived from guotingwei.py
# this program is adapted to Guo's request to modify
# his alternative method to calculate the SNP frequecy

import re
import os
import sys

# 7.2.5.8. Raw String Notation
# Raw string notation (r"text") keeps regular 
# expressions sane. Without it, every backslash ('\') 
# in a regular expression would have to be prefixed with 
# another one to escape it. For example, the two following 
# lines of code are functionally identical:


# GIT version
# 2
# since 2016-03-02

#
# read the genotype information into the file
#

def readGenomeData(file_name):
    file_handle      = open(file_name, 'rU')
    result           = []
    for line in file_handle:
        matched_line = re.match(r"\d+",line)
        if matched_line:
            line        = line.rstrip('\n')
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

def find_tag_freq(genome_data,list_index):
    last_index  = len(genome_data)
    tag_dic     = {}
    tag_freq    = {}
    tag_count   = []
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
        tag_count.append(tag_dic[i])
    max_num   = max(tag_count)
    total_num = float(sum(tag_count))
    for i in tag_keys:
        tag_freq[i] = tag_dic[i]/total_num
    return (tag_freq,tag_dic,total_num)

def find_genotype(genome_data,list_index):
    last_index       = len(genome_data)
    tag_genotype     = {}
 
    for i in range(0,last_index):
        tag_pair = genome_data[i][list_index]
        
        if re.search("\?",tag_pair):
            continue
        # you need change here;
        tag_pair  = re.split(r"_",tag_pair)
        pattern_1 = str(tag_pair[0]) + '/' + str(tag_pair[1])
        pattern_2 = str(tag_pair[1]) + '/' + str(tag_pair[0])
        if tag_genotype.has_key(pattern_1): 
            tag_genotype[pattern_1] += 1
        elif tag_genotype.has_key(pattern_2):
            tag_genotype[pattern_2] += 1
        else:
            tag_genotype[pattern_1]  = 1
    return tag_genotype




 
working_dir                            = '/home/zhenyisong/data/guotingwei'
file_name_inpath                       = 'test3.csv'
os.chdir(working_dir)
column_index                           = 1
guo_csv                                = readGenomeData(file_name_inpath)
tag_freq,tag_dic,total_num             = find_tag_freq(guo_csv,column_index)
tag_freq_file_name   = str(column_index) + '_1' + '.tag_freq.guotingwei.secondary.txt';
tag_freq_file_handle = open(tag_freq_file_name,'w')
formats              = ['%s','%s','%s','%s']
seperator            = '\t'
format               = seperator.join(formats)
file_header          = ['genotype','frequency', 'N', 'Total' ]
file_header          = format % tuple(file_header)
tag_freq_file_handle.write('%s\n' % file_header)
for genotype in tag_freq.keys():
    data_line   = (genotype,tag_freq[genotype],tag_dic[genotype],total_num)
    data_line   = format % data_line
    tag_freq_file_handle.write('%s\n' % data_line)
tag_freq_file_handle.close()
