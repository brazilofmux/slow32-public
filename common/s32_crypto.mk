# The crypto service is the host platform's library (common/s32_crypto_host.h).
# macOS: CommonCrypto, part of libSystem -- nothing to add.  Elsewhere:
# OpenSSL's libcrypto 3.x when pkg-config finds it; without it the service
# is simply not offered.  `make S32_CRYPTO=0` builds without it anywhere.
S32_CRYPTO_CFLAGS :=
S32_CRYPTO_LIBS :=
ifeq ($(S32_CRYPTO),0)
S32_CRYPTO_CFLAGS := -DS32_CRYPTO_NONE
else ifneq ($(shell uname -s),Darwin)
ifeq ($(shell pkg-config --atleast-version=3.0 libcrypto 2>/dev/null && echo yes),yes)
S32_CRYPTO_CFLAGS := -DS32_CRYPTO_OPENSSL $(shell pkg-config --cflags libcrypto)
S32_CRYPTO_LIBS := $(shell pkg-config --libs libcrypto)
endif
endif
