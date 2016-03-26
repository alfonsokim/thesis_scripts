import sys
import re

def parse_stat(regexp, line, stats_dict):
    match = regexp.match(line)
    if match and match.groupdict() != {}:
        stats_dict.update(match.groupdict())
        return True
    return False

def parse_global_stats(report):
    global_stats = {}
    cores_re = re.compile(r"Number of PEs \(MPI ranks\):\s*(?P<cores>\d*).*")
    input_size_re = re.compile(r"Program invocation:  \./scalability_column_sort\+pat (?P<size>\d*)")
    l1_re = re.compile(r'PAPI_L1_DCM\s*(?P<l1_rate>[\d|\.|/|\w]*)\s*(?P<l1_misses>[\d|\.|/|\w]*) misses')
    tlb_re = re.compile(r'PAPI_TLB_DM\s*(?P<tlb_rate>[\d|\.|/|\w]*)\s*(?P<tlb_misses>[\d|\.|/|\w]*) misses')
    l1_access_re = re.compile(r'PAPI_L1_DCA\s*(?P<l1_access>[\d|\.|/|\w]*)\s*(?P<l1_refs>[\d|\.|/|\w]*) refs')
    l2_refills_re = re.compile(r'\s*ALL\s*(?P<l2_refill_rate>[\d|\.|/|\w]*)\s*(?P<l2_refills>[\d|\.|/|\w]*) fills')
    tlb_util_re = re.compile(r'TLB utilization\s*(?P<tlb_util>[\d|\.|/|\w]*)\s*(?P<tlb_util_avg>[\d|\.|/|\w]*) avg uses')
    l1_hit_ratio_re = re.compile(r'D1 cache hit\,miss ratios\s*(?P<l1_hit_ratio>[\d|\.|\%|\w]*) hits\s*(?P<l1_miss_ratio>[\d|\.|\%|\w]*) misses')
    l2_hit_ratio_re = re.compile(r'D2 cache hit\,miss ratio\s*(?P<l2_hit_ratio>[\d|\.|\%|\w]*) hits\s*(?P<l2_miss_ratio>[\d|\.|\%|\w]*) misses')
    sys_to_l1_re = re.compile(r'System to D1 bandwidth\s*(?P<sys_to_l1_rate>[\d|\.|/|\w]*)\s*(?P<sys_to_l1>[\d|\.|\%|\w]*) bytes')
    l1_to_l2_re = re.compile(r'D2 to D1 bandwidth\s*(?P<l2_to_l1_rate>[\d|\.|/|\w]*)\s*(?P<l2_to_l1>[\d|\.|\%|\w]*) bytes')
    stats_regexp = [cores_re, input_size_re, l1_re, tlb_re, l1_access_re, l2_refills_re, 
                    tlb_util_re, l1_hit_ratio_re, l2_hit_ratio_re, sys_to_l1_re, l1_to_l2_re]
    for line in report:
        for regexp in stats_regexp:
            parse_stat(regexp, line.strip(), global_stats)

    return global_stats



def parse_mpi_messages_table(report, table_name, head=None):
    """
       Sent | Sent Msg |    Sent Msg | 16<= MsgSz |     256<= |Sent Distance
        Msg |    Count | Total Bytes | <256 Count |     MsgSz |
     Count% |          |             |            |     <4KiB |
            |          |             |            |     Count |
    """
    table_stats = {}
    row_re = re.compile(r'[\s|\|]*(?P<rate>[\d|\.]*)\%[\s|\|]*(?P<count>[\d|\.]*)[\s|\|]*(?P<bytes>[\d|\.]*)[\s|\|]*(?P<size_1>[\d|\.]*)[\s|\|]*(?P<size_2>[\d|\.]*)[\s|\|]*(?P<distance>[\d|\.]*)')
    in_table = False
    table_stats = []
    c = 0
    for line in report:
        line = line.strip()
        if table_name in line:
            in_table = True
        if in_table and '='*20 in line:
            in_table = False
        if in_table:
            stats = {}
            if parse_stat(row_re, line, stats):
                table_stats.append(stats)
                c += 1
                if c == head: break
    return table_stats



def parse(file_name):
    report = open(file_name, 'r')
    global_stats = parse_global_stats(report)
    for name, value in global_stats.iteritems():
        print '%s:   %s' % (str(name), str(value))
    report.seek(0)
    print "="*10
    table_stats = parse_mpi_messages_table(report, 'MPI Sent Message Stats by Distance', head=2)
    for stat in table_stats:
        for name, value in stat.iteritems():
            print '%s:   %s' % (str(name), str(value))


if __name__ == '__main__':
    parse(sys.argv[1])