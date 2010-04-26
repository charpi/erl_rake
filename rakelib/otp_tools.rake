# -*-ruby-*-
# Copyright 2010 Nicolas Charpentier
# Distributed under BSD licence
require 'rake/clean'

namespace :otp do
  
  directory "bin"
  directory "log"
  directory "pipes"


  CLEAN.include "bin"
  CLEAN.include "log"
  CLEAN.include "pipes"

  CLEAN.include "erl_crash.dump"

  desc "Start an erlang shell"
  task :shell do
    sh %{#{ERL_TOP}/bin/erl -pa lib/*/ebin -pa lib/*/test}
  end

  desc "Start release"
  task :start_release, :version do |t, args|
    sh %{#{ERL_TOP}/bin/erl -boot releases/#{args.version}/start -config releases/#{args.version}/sys}
  end

  desc "Create a new OTP application"
  task :new_application, :name do |t, args|
    app_name = args.name
    root_directory = "lib/#{app_name}"
    app_file_name = app_name + ".app.src"
    FileUtils.mkdir_p root_directory
    FileUtils.mkdir_p root_directory + "/src"
    FileUtils.mkdir_p root_directory + "/test"
    FileUtils.mkdir_p root_directory + "/include"
    FileUtils.mkdir_p root_directory + "/priv"
    FileUtils.mkdir_p root_directory + "/doc"
    File.open(root_directory + "/vsn.config", 'a') do |file| 
      file.write("{vsn,\"0.1\"}.\n")
    end
  
    File.open(root_directory + "/src/" + app_file_name, 'w') do |file|
      lines = ["{application, " + app_name + ",\n",
               "[{description, \"\"},\n",
               "{vsn, %VSN%},\n",
               "{modules, [%MODULES%]},\n",
               "{registered, []},\n",
               "{applications, [kernel, stdlib, sasl]}\n",
               "]}."]
      lines.each do |line|
        file.write(line)
      end
    end
  end

  ## Thanks to DangerDawson
  desc "Create a new OTP release"
  task :new_release, :name do |t, args|
    release_name = args.name
    root_directory = "lib/#{release_name}"
    release_file_name = release_name + ".rel.src"
    FileUtils.mkdir_p root_directory
    FileUtils.mkdir_p root_directory + "/release_config"
    FileUtils.mkdir_p root_directory + "/src"
    File.open(root_directory + "/vsn.config", 'a') do |file| 
      file.write("{release_name,\"initial\"}.\n")
    end
  
    File.open(root_directory + "/src/" + release_file_name, 'w') do |file|
      lines = ["{release,\n",
               "{\"#{release_name}\", \"\"},\n",
               "{erts, \"_\"},\n",
               "[{kernel, \"_\"},\n",
               "{stdlib, \"_\"},\n",
               "{sasl, \"_\"},\n",
               "{#{release_name}, \"_\"}]\n",
               "}.\n" ]
      lines.each do |line|
        file.write(line)
      end
    end

    File.open(root_directory + "/release_config/startup.conf", 'w') do |file| 
      file.write("ERL_FLAGS=\"\"\n")
      file.write("export ERL_FLAGS\n")
    end
    
    File.open(root_directory + "/release_config/sys.config", 'w') do |file| 
      file.write("[].")
    end
  end
  ## End Thanks 

  def collect_local_files
    local_releases = FileList.new('lib/*/src/*.rel.src')
    result = local_releases.inject({}) do |acc, d|
      config_file = d.pathmap("%d/../vsn.config")
      vsn = extract_version_information(config_file,"release_name").gsub(/\"/,"")
      dir = "releases/" + vsn 
      directory dir
      CLEAN.include dir

      new =  "releases/" + vsn + "/start.boot"
      script = d.pathmap("%{src,ebin}X")
      script = script.ext("")
      script = script.pathmap("%{src,ebin}X-" + vsn +".rel")
      file new => [dir, script] do |t|
        make_boot_file("releases/" + vsn , script.ext(""))
        sh "mv releases/#{vsn}/#{script.ext("").pathmap("%f")}.boot releases/#{vsn}/start.boot"
      end

      sys_config = d.pathmap("%d/../release_config/sys.config")
      new_config = "releases/" + vsn + "/sys.config"
      file new_config => sys_config do
        sh "cp #{sys_config} #{new_config}"
      end
      acc[new] = new
      acc[new_config] = new_config
      acc
    end
    result.keys
  end

  LOCAL_ERL_FILES = collect_local_files

  desc "Deploy all releases locally"
  task :deploy => ["erlang:release_files"] + 
    LOCAL_ERL_FILES do
  end

  
#   desc "Build an initial empty target system"
#   task :initial_target, :name, :version, :needs => ["erlang:releases"] do |t,args|
#     release_name = FileList.new("lib/*/ebin/"+File.join(args.name+'-'+args.version+'.rel'))
#     release_archive = File.join('tmp',args.name+'-'+args.version+'.tar.gz')
    
#     if !File.file?(release_name[0])
#       puts "The release #{args.name}-#{args.version} doesn't exist"
#       exit(-1)
#     end

#     FileUtils.makedirs('tmp')
#     FileUtils.makedirs('targets')
#     run_script("make_target", [release_name.ext(""),"tmp","targets",ERL_TOP] +
#                ERL_DIRECTORIES)
    
#     FileUtils.rm_r('tmp')
#   end

#   desc "Start an erlang system from a local release"
#   task :start_local, :name, :daemon, :needs => ["bin", "log", "pipes",
#                                                 "bin/connect_local",
#                                                 "bin/start_local",
#                                                 "bin/start_erl_local"] do |t, args|
    
#     opt = if args.daemon 
#             "\"-daemon\""
#           else
#             "\" \""
#           end
#     rel = FileList.new("lib/*/ebin/#{args.name}-*.rel").first
#     boot = rel.ext("").pathmap("release_local/%f")
#     conf = rel.pathmap("%d/../release_config/sys")
#     sh "bin/start_local #{boot} #{conf} #{opt} #{ERL_FLAGS}"
#   end


#   file 'bin/connect_local' do
#     File.open("bin/connect_local",'w') do |file|
#       file.write("#!/bin/sh \n")
#       file.write("#{ERL_TOP}/bin/to_erl pipes/ \n")
#       file.chmod(0755)
#     end
#   end

#   file 'bin/start_local' do
#     File.open("bin/start_local",'w') do |file|
#       lines = ["# $1 = boot file to use",
#                "# $2 = config_file to use",
#                "# $3 = -daemon or nothing",
#                "#!/bin/sh ",
#                "ROOTDIR=#{ERL_TOP}",
#                "boot=$1",
#                "shift",
#                "config=$1",
#                "shift",
#                "daemon=$1",
#                "shift",
#                "$ROOTDIR/bin/run_erl $daemon pipes/ log/ \"exec bin/start_erl_local"\
#                " #{ERL_TOP} $boot $config $*\""]
#       lines.each do |line|
#         file.write(line)
#         file.write("\n")
#       end
#       file.chmod(0755)
#     end
#   end

#   file 'bin/start_erl_local' do
#     File.open("bin/start_erl_local",'w') do |file|
#       erts = FileList.new("#{ERL_TOP}/erts-*")
#       lines = ["ROOTDIR=$1",
#                "RELDIR=$1",
#                "BINDIR=#{erts}/bin",
#                "EMU=beam",
#                "PROGNAME=`echo $0`",
#                "export EMU",
#                "export ROOTDIR",
#                "export BINDIR",
#                "export PROGNAME",
#                "export RELDIR",
#                "shift",
#                "boot=$1",
#                "shift",
#                "config=$1",
#                "shift",
#                "exec $BINDIR/erlexec -boot $boot -config $config $*"]
#       lines.each do |line|
#         file.write(line)
#         file.write("\n")
#       end
#       file.chmod(0755)
#     end
#   end

end
