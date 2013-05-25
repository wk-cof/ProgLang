class A
  X = [0]
  def foo
    X
  end
end

class B < A
  X = X.concat( [1,
                 2])
  def bar
    puts yield
  end
end

