task :deliver => [] do
  version = `git rev-list HEAD | wc -l`.strip
  sh "git archive master | bzip2 > erlrake_#{version}.tar.bz2"
end

task :default => :deliver
