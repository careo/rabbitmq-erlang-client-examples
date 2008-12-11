require 'rake'
require 'rake/clean'

# Configuration
START_MODULE = "stocks_example"
TEST_MODULE = "test_stocks_example"
MNESIA_DIR = "/tmp"


# No Need to change
PWD = `pwd`.strip
INCLUDE = "include"
ERLC_FLAGS = "-I#{INCLUDE} +warn_unused_vars +warn_unused_import"

SRC = FileList['src/**/*.erl']
OBJ = SRC.pathmap("%{src,ebin}X.beam")
CLEAN.include(['**/*.dump'])
CLEAN.include(['**/*.beam'])
CLOBBER.include(['**/*.beam'])

directory 'ebin'


rule ".beam" =>  ["%{ebin,src}X.erl"] do |t|
  sh "erlc -pa ebin -W #{ERLC_FLAGS} -o ebin #{t.source}"
end

desc "Compile all"
task :compile => ['ebin'] + OBJ

desc "Open up a shell"
task :shell => [:compile] do
    sh("erl -sname #{START_MODULE} -pa #{PWD}/ebin")
end

desc "Open up a shell and run #{START_MODULE}:start()" 
task :run => [:compile] do
    sh("erl -sname #{START_MODULE} -pa #{PWD}/ebin -run #{START_MODULE} start")
end

desc "Run Unit Tests" 
task :test do
  sh("erl -noshell -s #{TEST_MODULE} test -s init stop")
end


desc "Generate Documentation"
task :doc do
    sh("cd doc && erl -noshell -run edoc files ../#{SRC.join(" ../")} -run init stop")
end

task :default => :compile
