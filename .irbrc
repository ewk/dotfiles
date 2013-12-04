require 'irb/completion'

IRB.conf[:USE_READLINE] = true
IRB.conf[:SAVE_HISTORY] = 50
puts "Hola!"

# use: 'time {COMMAND}'
def time(&block)
  require 'benchmark'
  result = nil
  timing = Benchmark.measure do
    result = block.()
  end
  puts "It took: #{timing}"
  result
end
