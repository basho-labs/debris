SConscript('src/SConscript', variant_dir='build')
env = Environment()
if 'docs' in COMMAND_LINE_TARGETS:
  env.Command('./docs/html/index.html', '', "doxygen")

