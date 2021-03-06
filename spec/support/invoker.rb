# encoding: utf-8

require 'open3'

# http://dota2.ru/guides/880-invokirkhakha-sanstrajk-ni-azhydal-da/
#
class MiniTest::Test
  # Quas Wex Exort.
  #
  def invoke_cache
    @invoke_cache ||= {}
  end

  # So begins a new age of knowledge.
  #
  def invoke(*argv)
    return invoke_cache[argv] if invoke_cache.has_key? argv

    arguments = argv.dup
    options = (arguments.last.is_a? Hash) ? arguments.pop : {}
    executable = File.expand_path('../../../bin/greeb', __FILE__)
    status = nil

    Open3.popen3(executable, *arguments) do |i, o, _, t|
      i.puts options[:stdin] if options[:stdin]
      i.close
      invoke_cache[argv] = o.readlines.map(&:chomp!)
      status = t.value
    end

    invoke_cache[argv] if status.success?
  end
end
