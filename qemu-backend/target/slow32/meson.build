slow32_ss = ss.source_set()
slow32_ss.add(files(
  'cpu.c',
  'helper.c',
  'mmio.c',
  'translate.c',
))

# The crypto service is the host platform's library (s32_crypto_host.h):
# CommonCrypto on macOS needs nothing; elsewhere OpenSSL's libcrypto 3.x if
# it is there, and without it the service is not offered.
if host_os != 'darwin'
  slow32_libcrypto = dependency('libcrypto', version: '>=3.0', required: false)
  if slow32_libcrypto.found()
    slow32_ss.add(slow32_libcrypto)
    slow32_ss.add(when: slow32_libcrypto, if_true: declare_dependency(compile_args: ['-DS32_CRYPTO_OPENSSL']))
  endif
endif

slow32_system_ss = ss.source_set()

target_arch += {'slow32': slow32_ss}
target_system_arch += {'slow32': slow32_system_ss}
