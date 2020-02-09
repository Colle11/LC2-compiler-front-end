=begin
We test the generation of labels for the loops that may lead to
single TAC instructions labelled with more than one label
=end

def int main()
  while true
  end
  int i = 0
  return 42
  while i < 10
    i++
  end
  42
  until i > 5
    --i
  end
  42
  loop do
    break
  end
end
