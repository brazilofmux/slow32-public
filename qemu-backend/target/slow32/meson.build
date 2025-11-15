slow32_ss = ss.source_set()
slow32_ss.add(files(
  'cpu.c',
  'helper.c',
  'mmio.c',
  'translate.c',
))

slow32_system_ss = ss.source_set()

target_arch += {'slow32': slow32_ss}
target_system_arch += {'slow32': slow32_system_ss}
