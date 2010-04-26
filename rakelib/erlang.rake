# -*-ruby-*-
# Copyright 2008-2009 Nicolas Charpentier
# Distributed under BSD licence

require 'rake/clean'

if File.file?('erlang_config.rb') 
  require 'erlang_config'  
else
  puts "erlang_config.rb file is missing."
  puts "You need to fill it with your local configuration."
  puts "An sample has been generated for you."
  File.open("erlang_config.rb",'w') do |file|
    file.write("#ERL_TOP is the location of the erlang installation.\n")
    file.write("ERL_TOP=\"/usr/local/lib/erlang\"\n")
    file.write("#USE_EMAKE tells erl_rake how to compile erlang sources.\n")
    file.write("#Valid values are true and false.\n")
    file.write("USE_EMAKE=false\n")
    file.write("#ERLC_FLAGS are the flags to be passed to the compiler ERLC.\n")
    file.write("ERLC_FLAGS=\"\"\n")
    file.write("#EMAKE_COMPILE_OPTIONS are the flags to be passed to the EMAKE.\n")
    file.write("EMAKE_COMPILE_OPTIONS = []\n")
    file.write("#ERL_FLAGS are the flags to be passed to ERL when erl_rake\n")
    file.write("#start a node.\n")
    file.write("ERL_FLAGS=\"\"\n")
    file.write("#Check app file for missing dependencies\n")
    file.write("CHECK_APP=true\n")
  end
  exit(-1)
end

require 'rake/loaders/makefile'
import '.depend_erlang.mf'

namespace :erlang do
  
  def handle_test(mode, args)
    directories = ERL_DIRECTORIES + ERL_DIRECTORIES.pathmap("%{ebin,test}X")
    if args.name
      description = "lib/" + args.name + "/test.desc"
      if File.file?(description)
        run_description_test(description, mode, args.name, directories)
      else
        run_application_test(args.name, mode, directories)
      end
    else
      ERL_APPLICATIONS.each do |application|
        description = application.pathmap("%{ebin,test}d/test.desc")
        if File.file?(description)
          run_description_test(description, mode, application, directories)
        else
          run_application_test(application, mode, directories)
        end
      end
    end
  end
    
  def collect_boot_files(releases)
    releases.collect { |elt|
        elt.pathmap("%{src,ebin}X.boot")
    }
  end
  
  def collect_release_files(rel_sources)
    rel_sources.inject({}) do |acc,d|
      config_file = d.pathmap("%d/../vsn.config")
      vsn = extract_version_information(config_file,"release_name").gsub(/\"/,"")

      map_expression = "%{src,ebin}X-" + vsn  + ".rel"
      rel_file = d.ext("").pathmap(map_expression) 
      acc[rel_file] = d
#      directory "release_local"
#      acc[rel_file.pathmap("release_local/%f")] = d
      acc
    end
  end
  
  def collect_generated_archives(releases)
    releases.pathmap("distrib/%f").ext(".tar.gz")
  end
  
  def extract_version_information(file, type)
    informations = []
    IO.foreach(file) { |line|
      informations << $1 if line =~ /\{#{type},(.*)\}/
    }
    informations[0]
  end
  
  def application_modules(app_file)
    modules = FileList.new(app_file.pathmap("%d/*.beam")).pathmap("%f").ext("")
    modules = modules.map {|item| item.gsub(/^([A-Z].*)/, '\'\1\'')}
    modules = modules.join(', ')
  end
  
  def check_dependencies (file)
    dependencies = []
    IO.foreach(file) { |line|
      header = if line =~ /^-include\("(.*)"\)/
                 file.pathmap("%d/#$1")
               else
                 if line =~ /^-include_lib\("(.*)"\)/
                   tmp = "lib/"+$1
                   if File.file?(tmp)
                     tmp
                   end
                 end
               end
      if header 
        dependencies << "#{header}"
        dependencies <<  check_dependencies(header)
      end
    } if File.file?(file)
    dependencies.flatten
  end
  
  def erlang_include_dependencies
    erlang_source_dependencies + erlang_test_dependencies
  end

  def run_application_test(application, mode, directories)
    application_name = application.pathmap("%f").ext("")
    run_script("run_test",[mode, application_name, "all_tests"] + directories)
  end

  def run_description_test(description, mode, application, directories)
    application_name = application.pathmap("%f").ext("")
    run_script("run_test",[mode, application_name, description, ] + directories)
  end

  def run_script(script, parameters)
    script_file = File.dirname(__FILE__) + "/escripts/" + script
    #puts "#{ERL_TOP}/bin/escript #{script_file} #{parameters.join(' ')}"
    sh %{#{ERL_TOP}/bin/escript #{script_file} #{parameters.join(' ')}}
  end

  def make_boot_file(output, source)
    if output =~ /^releases(.*)/
      style = "local"
    else
      style = "releases"
    end
    run_script("make_script",[style, source, output] + ERL_DIRECTORIES)
  end
  
  ERL_SOURCES = FileList['lib/*/src/*.erl']
  ERL_BEAM = ERL_SOURCES.pathmap("%{src,ebin}X.beam")

  ERL_TESTS = FileList['lib/*/test/*.erl']
  ERL_BEAM_TESTS = ERL_TESTS.pathmap("%X.beam")
  ERL_UNIT_TESTS = FileList['lib/*/test/*.erl']

  ERL_ASN_SOURCES = FileList['lib/*/src/*.asn']

  src_to_ebin = "%{src,ebin}X"

  ERL_DIRECTORIES = FileList.new('lib/*/src').pathmap(src_to_ebin)

  ERL_APPLICATIONS = FileList.new('lib/*/src/*.app.src').pathmap(src_to_ebin)


  ERL_RELEASE_FILES_DEP = collect_release_files(FileList.new('lib/*/src/*.rel.src'))
  ERL_RELEASE_FILES = FileList.new(ERL_RELEASE_FILES_DEP.keys)
  ERL_BOOT_FILES = collect_boot_files(ERL_RELEASE_FILES)

  ERL_RELEASE_ARCHIVES = collect_generated_archives(ERL_RELEASE_FILES)

  ERL_RELEASE_ARCHIVES.each do |d|
    CLEAN.include d
  end

  directory "releases"

  directory "distrib"
  CLEAN.include "distrib"

  ERL_DIRECTORIES.each do |d| 
    directory d
    CLEAN.include d
  end

  CLEAN.include("lib/*/test/*.beam")
  CLEAN.include("lib/*/cover")

  def beam_dependencies_with_emake(beam, file) 
    dependencies = check_dependencies(file).uniq
    dependencies.collect { |dependency|
      ["#{beam}: #{dependency}", "Emakefile: #{dependency}"]
    } + ["#{beam} : #{file}", "#{beam}: Emakefile"]
  end

  def beam_dependencies(beam, file) 
    dependencies = check_dependencies(file).uniq
    dependencies.collect { |dependency|
      ["#{beam}: #{dependency}"]
    } + ["#{beam} : #{file}"]
  end

  if USE_EMAKE 
    def erlang_test_dependencies
      FileList['lib/*/test/*.erl'].collect { |file|
        beam = file.pathmap("%X.beam")
        beam_dependencies_with_emake(beam, file) 
      }.flatten
    end
    
    def erlang_source_dependencies
      FileList['lib/*/src/*.erl'].collect { |file|
        beam = file.pathmap("%{src,ebin}X.beam")
        beam_dependencies_with_emake(beam, file)
     }.flatten
    end

    rule '.beam' => "Emakefile"

    file "Emakefile" => ERL_SOURCES + ERL_TESTS do |t|
      source_directories = t.prerequisites.collect { |elt| elt.pathmap("%d")}
      File.open("Emakefile",'w') do |file|
        
        source_directories.uniq.each do |elt|
          target_directory = elt.pathmap("%{src,ebin}X")
          options = ["{outdir,\"#{target_directory}\"}",
                     "{i, \"lib\"}",
                     "{i,\"lib/*/include\"}"] + EMAKE_COMPILE_OPTIONS
          option_string = "[#{options.join(',')}]" 
          file.write("{\"#{elt}/*\",#{option_string}}.\n")
        end
      end
    end
    CLEAN.include "Emakefile"
    
    desc "Compile Erlang sources"
    task :modules => ERL_DIRECTORIES + ERL_BEAM + ERL_BEAM_TESTS do
      sh "#{ERL_TOP}/bin/erl -make"
    end

  else
    def erlang_test_dependencies
      FileList['lib/*/test/*.erl'].collect { |file|
        beam = file.pathmap("%X.beam")
        beam_dependencies(beam, file)
      }.flatten
    end
    
    def erlang_source_dependencies
      FileList['lib/*/src/*.erl'].collect { |file|
        beam = file.pathmap("%{src,ebin}X.beam")
        beam_dependencies(beam, file)
      }.flatten
    end
    
    rule ".beam" =>  ["%{ebin,src}X.erl"] do |t|
      output = t.name.pathmap("%d")
      sh "#{ERL_TOP}/bin/erlc -Ilib #{ERLC_FLAGS} -o #{output} #{t.source}"
    end

    desc "Compile Erlang sources"
    task :modules => ERL_DIRECTORIES + ERL_BEAM + ERL_BEAM_TESTS

  end

  rule '.app' => ["%{ebin,src}X.app.src",
                  "%{ebin,src}d/../vsn.config"] do |t|
    configuration = t.name.pathmap("%d/../vsn.config")
    vsn = extract_version_information(configuration,"vsn")
    modules = application_modules t.name
    app_name = t.name.pathmap("%f").ext("")
    File.open(t.name, 'w') do |outf|
      File.open(t.source) do |inf|
        inf.each_line do |ln|
          outf.write(ln.gsub('%VSN%', vsn).gsub('%MODULES%',modules))
        end
      end
    end
    if CHECK_APP
      run_script("check_dependency", [app_name] + ERL_DIRECTORIES)
    end
  end

  def release_to_src(a)
    value = ERL_RELEASE_FILES_DEP[a]
    [value, a.pathmap("%d") ]
  end

  rule ".rel" => proc {|a| release_to_src(a)}  do |t|
    configuration = t.source.pathmap("%d/../vsn.config")
    release_name = extract_version_information(configuration,"release_name")
    output = t.name.pathmap("%X.rel")
    run_script("make_release_file",[t.source,output,release_name] + ERL_DIRECTORIES)
  end

  rule ".boot" => [".rel"] do |t|
    output = t.name.pathmap("%d")
    make_boot_file(output, t.source.ext(""))

  end

  rule ".tar.gz" => [proc {|a|
                       FileList.new(a.ext("").ext("").pathmap("lib/*/ebin/%f.rel"))},
                     "distrib"] do |t|
    source = t.source.ext("")
    run_script("make_release", [source,"distrib"] + ERL_DIRECTORIES)
  end

  ERL_ASN_SOURCES.each do |source|
    hrl = source.pathmap("%X.hrl")
    erl = source.pathmap("%X.erl")
    beam = erl.pathmap("%{src,ebin}X.beam")
    asndb = source.pathmap("%X.asn1db")
    file hrl => source do
      sh "#{ERL_TOP}/bin/erlc +noobj -Ilib #{ERLC_FLAGS} -o #{hrl.pathmap("%d")} #{source}"
      sh "#{ERL_TOP}/bin/erlc -Ilib #{ERLC_FLAGS} -o #{beam.pathmap("%d")} #{erl}"
    end
    file erl => source do
      sh "#{ERL_TOP}/bin/erlc +noobj -Ilib #{ERLC_FLAGS} -o #{hrl.pathmap("%d")} #{source}"
      sh "#{ERL_TOP}/bin/erlc -Ilib #{ERLC_FLAGS} -o #{beam.pathmap("%d")} #{erl}"
    end
    file asndb => source do
      sh "#{ERL_TOP}/bin/erlc +noobj -Ilib #{ERLC_FLAGS} -o #{asndb.pathmap("%d")} #{source}"
      sh "#{ERL_TOP}/bin/erlc -Ilib #{ERLC_FLAGS} -o #{beam.pathmap("%d")} #{erl}"
    end
    CLEAN.include hrl
    CLEAN.include asndb
    CLEAN.include erl
    CLEAN.include beam
  end

  CLEAN.include ".depend_erlang.mf"


  file ".depend_erlang.mf" => ERL_SOURCES + ERL_TESTS do
    File.open(".depend_erlang.mf",'w') {|file|
      erlang_include_dependencies.each do |l|
        file.write("#{l}\n")
      end
    }
  end


  desc "Run erlang unit test for all or a specific application."\
  "(No name given mean all applications)"
  task :tests, [:name] => [:applications] + ERL_BEAM_TESTS do
    |t, args|
    handle_test("test", args)
  end

  desc "Run cover on unit test for all or a specific application."\
  "(No name given mean all applications)"
  task :cover, [:name] => [:applications] + ERL_BEAM_TESTS do
    |t, args|
    handle_test("cover", args)
  end

  desc "Build application resource file"
  task :applications => [:modules] + ERL_APPLICATIONS

  desc "Build erlang boot files"
  task :release_files => [:applications] + ERL_RELEASE_FILES + ERL_BOOT_FILES

  desc "Generate a release tarball in  the distrib directory"
  task :releases => [:release_files] + ERL_RELEASE_ARCHIVES

  CLEAN.include "lib/*/doc/*.html"
  CLEAN.include "lib/*/doc/*.css"
  CLEAN.include "lib/*/doc/*.png"
  CLEAN.include "lib/*/doc/edoc-info"

  desc "Buid Application documentation"
  task :edoc, [:name] => [:applications] do |t,args|
    names = if args.name
              FileList.new('lib/#{args.name}/src/*.app.src').pathmap(src_to_ebin)
            else
              ERL_APPLICATIONS
            end
    names.each do |application|
      puts "#{application}"
      configuration = application.pathmap("%d/../vsn.config")
      vsn = extract_version_information(configuration,"vsn")
      modules = application_modules t.name
      
      app_name = t.name.pathmap("%f").ext("")
      
      overview = application.pathmap("%d/../doc/overview.edoc")
      tmp = application.pathmap("%d/../doc/overview.edoc.tmp")
      out = File.open(tmp, 'w')
      if File.file?(overview)
        puts "=== #{overview}"
        File.open(overview, 'r+') do |inf|
          inf.each_line do |ln|
            puts "#{ln}"
            ln.gsub!('@version %VSN%', '@version #{vsn}')
            out.write(ln)
          end
        end
      end
      out.close()
      File.rename(tmp,overview)

      name = application.pathmap("%f").ext("")
      run_script("make_doc",[name] + ERL_DIRECTORIES)
    end
  end

  desc "Run dialyzer"
  task :dialyzer do
    sh "#{ERL_TOP}/bin/dialyzer --src -I lib #{ERL_DIRECTORIES.pathmap("-I%{ebin,include}X")} #{ERL_DIRECTORIES.pathmap("-c %{ebin,src}X")} #{ERL_DIRECTORIES.pathmap("-c %{ebin,test}X")} -Wunderspecs -Wspecdiffs --no_native"
  end

  desc "Compile all projects"
  task :compile => [:modules, :applications, :release_files]

  task :default => [:compile]

end
