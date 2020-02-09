=begin
We test the robustness of codegen of empty if-branches
that may be problematic for the generation of unnecessary labels
=end

def int main()
  if true then # no-op guard
    # empty body
  elsif true then # no-op guard
    # emtpy body
  else
    # empty body
  end
  43; # to mark the end of first if
  if 1 < 2 then
    if 4 > 5
      # empty body
    end
  elsif true then # no-op guard
    # non obvious empty body
    if true
    else
    end
  end
  return 0
end

def f() # empty function
  if true
  end
end
