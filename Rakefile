#-*-ruby-*-
require 'rake'

task :default => ["erlang:compile", "erlang:releases"]

task :deliver => [] do
  version=`svnversion -n`
  export_directory="/tmp/rakelib"
  sh "rm -fr #{export_directory}; svn export rakelib #{export_directory}"
  sh "tar -czv -f rakelib_#{version}.tgz -C  /tmp rakelib"
end
