#!/usr/bin/ruby

f = ARGV.shift

t1 = 0.0
lc = 1
p1 = 0
ps_lost = 0
File.readlines(f).each do |line|
    if line[0] == "["
        data = line.scan(/\[\d+\.\d+\]/)[0]
        data.chop!
        data.slice!(0)
        t2 = data.to_f
        p_time = line.scan(/time=\S+/)[0]
        p_time.slice!(0,5)
        p_time = p_time.to_f
        time = Time.at(t2)
        gap = t2 - t1
        p2 = line.scan(/icmp_seq=\d+/)[0]
        p2.slice!(0,9)
        p2 = p2.to_i
        p_count_gap = p2 - p1
        if p_count_gap > 1
            ps_lost += (p_count_gap - 1) # -1, since if 1 then 3, we're missing packet #2, which is just 1 packet, not 2 packets
        end

        # display diagnostic messages only when comparing things after the first line
        if lc != 1
            puts "line #{lc}\t@ #{time} -> #{100*(gap).round/100} sec gap b/n packets" if (gap > 2)
            puts "line #{lc}\t@ #{time} -> #{p_time} ms packet time" if p_time > 50
            puts "line #{lc}\t@ #{time} -> #{p_count_gap - 1} packets lost" if p_count_gap > 1
        end
        t1 = t2
        p1 = p2
        lc += 1
    end
end
# lc - 1, since last (lc += 1) does not count
puts "\npackets lost: #{ps_lost} out of #{lc - 1}, or #{100*ps_lost/lc.to_f}%"
