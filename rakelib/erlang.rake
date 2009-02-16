
# -*-ruby-*-
# Copyright 2008-2009 Nicolas Charpentier
# Distributed under BSD licence


if File.file?('erlang_config.rb') 
  require 'erlang_config'  
else
  puts "erlang_config.rb file is missing."
  puts "You need to fill it with your local configuration."
  puts "An sample has been generated for you."
  File.open("erlang_config.rb",'w') do |file|
    file.write("ERL_TOP=\"<path to your erlang installation>\"\n")
    file.write("ERLC_FLAGS=\"\"\n")
    file.write("ERL_FLAGS=\"\"\n")
    file.write("USE_EMAKE=false\n")
    file.write("EMAKE_COMPILE_OPTIONS = []\n")
  end
  exit(-1)
end

require 'rake/clean'
require 'rake/loaders/makefile'
import '.depend_erlang.mf'

namespace :erlang do
  
  def collect_boot_files(releases)
    releases.collect { |elt|
      if elt =~ /^release/
        local_boot = elt.ext("").pathmap("%d/%f.boot")
        file local_boot => elt do
          make_boot_file(elt.pathmap("%d"), elt.ext(""))
        end
        local_boot
      else
        elt.pathmap("%{src,ebin}X.boot")
      end
    }
  end
  
  def collect_release_files(rel_sources)
    rel_sources.inject({}) do |acc,d|
      config_file = d.pathmap("%d/../vsn.config")
      vsn = extract_version_information(config_file,"release_name").gsub(/\"/,"")
      directory "release_local"
      map_expression = "%{src,ebin}X-" + vsn  + ".rel"
      rel_file = d.ext("").pathmap(map_expression) 
      acc[rel_file] = d
      acc[rel_file.pathmap("release_local/%f")] = d
      acc
    end
  end
  
  def collect_generated_archives(releases)
    tmp = releases.delete_if { |elt| elt =~ /^release_local/ }
    tmp = tmp.pathmap("releases/%f").ext(".tar.gz")
    tmp
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
    modules = "[" + modules.join(', ') + "]"
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

  def run_application_test(application, directories)
    application_name = application.pathmap("%f").ext("")
    run_script("run_test",["application", application_name] + directories)
  end

  def run_description_test(description, directories)
    run_script("run_test",["file", description] + directories)
  end

  def run_script(script, parameters)
    script_file = "rakelib/escripts/" + script
    #  puts "#{ERL_TOP}/bin/escript #{script_file} #{parameters.join(' ')}"
    sh "#{ERL_TOP}/bin/escript #{script_file} #{parameters.join(' ')}"
  end

  def make_boot_file(output, source)
    if output =~ /^release_local/
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
  directory "applications"
  CLEAN.include "release_local"

  ERL_DIRECTORIES.each do |d| 
    directory d
    CLEAN.include d
  end

  CLEAN.include("lib/*/test/*.beam")

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
      sh "#{ERL_TOP}/bin/erl -noinput -s make all -s erlang halt "
    end
    CLEAN.include "Emakefile"
    
#     rule ".beam" =>  ["Emakefile"] do |t|
#       output = t.name.pathmap("%d")
#       sh "#{ERL_TOP}/bin/erl -noinput -s make all -s erlang halt "
#     end
    
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
  end
  rule '.app' => ["%{ebin,src}X.app.src",
                  "%{ebin,src}d/../vsn.config"] do |t|
    configuration = t.name.pathmap("%d/../vsn.config")
    vsn = extract_version_information(configuration,"vsn")
    modules = application_modules t.name
    File.open(t.name, 'w') do |outf|
      File.open(t.source) do |inf|
        inf.each_line do |ln|
          outf.write(ln.gsub('%VSN%', vsn).gsub('%MODULES%',modules))
        end
      end
    end
  end

  def release_to_src(a)
    if a =~ /^release/
      [ERL_RELEASE_FILES_DEP[a], a.pathmap("%d") ]
    else 
      [ERL_RELEASE_FILES_DEP[a], a.pathmap("%d") ]
    end

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
                       FileList.new(a.ext("").ext("")\
                                    .pathmap("lib/*/ebin/%f.rel"))},
                     "releases"] do |t|
    source = t.source.ext("")
    run_script("make_release", [source,"releases"] + ERL_DIRECTORIES)
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

  desc "Compile Erlang sources"
  task :modules => ERL_DIRECTORIES + ERL_BEAM + ERL_BEAM_TESTS

  desc "Run erlang unit test for all or a specific application."\
  "(No name given mean all applications)"
  task :tests, :name, :needs => [:applications] + ERL_BEAM_TESTS do
    |t, args|
    directories = ERL_DIRECTORIES + ERL_DIRECTORIES.pathmap("%{ebin,test}X")
    if args.name
      description = "lib/" + args.name + "/test.desc"
      if File.file?(description)
        run_description_test(description, directories)
      else
        run_application_test(args.name, directories)
      end
    else
      ERL_APPLICATIONS.each do |application|
        description = application.pathmap("%{ebin,test}d/test.desc")
        if File.file?(description)
          run_description_test(description, directories)
        else
          run_application_test(application, directories)
        end
      end
    end
    
  end

  desc "Build application resource file"
  task :applications => [:modules] + ERL_APPLICATIONS

  desc "Build erlang boot files"
  task :release_files => [:applications] + ERL_RELEASE_FILES  + ERL_BOOT_FILES

  desc "Build release tarball"
  task :releases => [:release_files] + ERL_RELEASE_ARCHIVES

  CLEAN.include "lib/*/doc/*.html"
  CLEAN.include "lib/*/doc/*.css"
  CLEAN.include "lib/*/doc/*.png"
  CLEAN.include "lib/*/doc/edoc-info"

  desc "Buid Application documentation"
  task :edoc, :name, :needs => [:applications] do |t,args|
    names = if args.name
              [args.name]
            else
              ERL_APPLICATIONS
            end
    names.each do |application|
      name = application.pathmap("%f").ext("")
      run_script("make_doc",[name] + ERL_DIRECTORIES)
    end
  end

  desc "Run dialyzer"
  task :dialyzer do
    sh "#{ERL_TOP}/bin/dialyzer --src -I lib -I lib/sample_rake/include -c #{ERL_DIRECTORIES.pathmap("%{ebin,src}X")}"
  end

  desc "Compile all projects"
  task :compile => [:modules, :applications, :tests]

  task :default => [:compile]

  desc "Build Application packages"
  task :package => [:applications] do
    ERL_APPLICATIONS.each do |application|
      application_directory = application.pathmap("%{ebin}d")
      name = application.pathmap("%f").ext("")
      all_files = FileList.new(application_directory+"/*/*")
    end
  end
end
