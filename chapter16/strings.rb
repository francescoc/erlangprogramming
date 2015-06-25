def fac n
 if (n<=0) then 1 else n*(fac (n-1)) end
end

def str
  "3"
end

def n
  str.to_i
end

puts "n is #{n}"


