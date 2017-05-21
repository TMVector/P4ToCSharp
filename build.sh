
# Download paket if not present
if [ ! -f .paket/paket.exe ]; then
  .paket/paket.bootstrapper.exe
  chmod u+x .paket/paket.exe
fi
if [ ! -f .paket/paket.exe ]; then
  echo Could not run paket
fi

# Restore packages
.paket/paket.exe restore

# Build
msbuild
