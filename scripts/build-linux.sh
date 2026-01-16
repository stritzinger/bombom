#!/usr/bin/env bash
set -euo pipefail

# Build script for creating a standalone BomBom binary
#
# This script:
# 1. Downloads and extracts OTP (Erlang/OTP runtime)
# 2. Downloads musl runtime library
# 3. Builds piadina and azdora tools
# 4. Builds the bombom Erlang application
# 5. Assembles a payload directory with OTP + bombom
# 6. Packages everything into a single binary using azdora/piadina
#
# Required environment variables:
#   OTP_TARBALL_URL  - URL to download OTP tarball (beam-machine-universal)
#   MUSL_SO_URL      - URL to download musl libc shared object
#   OUTPUT           - Output filename for the final binary
#
# Optional environment variables:
#   BOMBOM_REPO_URL  - Git repository URL for bombom (default: https://github.com/stritzinger/bombom.git)
#   BOMBOM_REF       - Git ref to checkout (default: main)
#   PIADINA_REPO_URL - Git repository URL for piadina (default: https://github.com/stritzinger/piadina.git)
#   PIADINA_REF      - Git ref to checkout (default: main)
#   APP_VER          - Application version (default: dev)
#   OTP_VERSION      - OTP version string (default: unknown)

#=============================================================================
# Constants
#=============================================================================

readonly CURL_RETRY_ARGS=(--retry 3 --retry-delay 2 --retry-all-errors --max-time 180)
readonly LIBARCHIVE_REPO="https://github.com/libarchive/libarchive.git"
readonly LIBARCHIVE_HEADER="libarchive/libarchive/archive.h"

#=============================================================================
# Helper Functions
#=============================================================================

# Print a log message with consistent formatting
log() {
  echo "==> $*"
}

# Print an error message and exit
die() {
  echo "ERROR: $*" >&2
  exit 1
}

# Validate that a file exists, or die with an error
validate_file_exists() {
  local file="$1"
  local context="${2:-}"
  if [[ ! -f "$file" ]]; then
    die "File not found: $file${context:+ ($context)}"
  fi
}

# Validate that a directory exists, or die with an error
validate_directory_exists() {
  local dir="$1"
  local context="${2:-}"
  if [[ ! -d "$dir" ]]; then
    die "Directory not found: $dir${context:+ ($context)}"
  fi
}

# Validate that a command exists on PATH
validate_command_exists() {
  local cmd="$1"
  local context="${2:-}"
  if ! command -v "$cmd" >/dev/null 2>&1; then
    die "Command not found: $cmd${context:+ ($context)}"
  fi
}

# Download a file with retry logic
download_file() {
  local url="$1"
  local output="$2"
  log "Downloading: $url"
  curl -fL "${CURL_RETRY_ARGS[@]}" "$url" -o "$output"
}

# Clone a git repository and checkout a specific ref
git_clone_and_checkout() {
  local repo_url="$1"
  local dest_dir="$2"
  local ref="${3:-main}"
  log "Cloning $repo_url (ref: $ref) to $dest_dir"
  rm -rf "$dest_dir"
  git clone "$repo_url" "$dest_dir"
  (cd "$dest_dir" && git checkout "$ref")
}

# Find a directory matching a pattern, or die
find_directory_or_die() {
  local search_dir="$1"
  local pattern="$2"
  local context="$3"
  local result
  result="$(find "$search_dir" -maxdepth 2 -type d -name "$pattern" -print | head -n1)"
  if [[ -z "$result" ]]; then
    echo "Contents of $search_dir:" >&2
    ls -la "$search_dir" >&2 || true
    die "Could not find $pattern under $search_dir${context:+ ($context)}"
  fi
  echo "$result"
}

# Compute SHA256 checksum (cross-platform)
compute_checksum() {
  local file="$1"
  local output="$2"
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$file" > "$output"
  else
    shasum -a 256 "$file" > "$output"
  fi
}

#=============================================================================
# Build Steps
#=============================================================================

# Step 1: Download and extract OTP
download_and_extract_otp() {
  log "1. Download OTP"
  download_file "$OTP_TARBALL_URL" "$WORK/otp.tar.gz"

  rm -rf "$WORK/otp"
  mkdir -p "$WORK/otp"

  # beam-machine-universal tarballs contain a top-level directory
  log "Extracting OTP tarball"
  tar -xzf "$WORK/otp.tar.gz" -C "$WORK/otp"

  # OTP_ROOT is the directory that contains erts-*
  local erts_path
  erts_path="$(find_directory_or_die "$WORK/otp" "erts-*" "after extracting OTP tarball")"
  OTP_ROOT="$(dirname "$erts_path")"

  # Determine ERTS_DIR name
  ERTS_DIR="$(cd "$OTP_ROOT" && ls -d erts-* | head -n1)"
  if [[ -z "${ERTS_DIR}" ]]; then
    echo "Contents of $OTP_ROOT:" >&2
    ls -la "$OTP_ROOT" >&2 || true
    die "Could not determine ERTS_DIR (erts-*) under OTP_ROOT=$OTP_ROOT"
  fi

  log "OTP_ROOT=$OTP_ROOT, ERTS_DIR=$ERTS_DIR"
}

# Step 2: Download musl runtime
download_musl() {
  log "2. Download musl runtime"
  download_file "$MUSL_SO_URL" "$WORK/libc-musl.so"

  # If running arm64 under qemu on an x86 host, qemu-aarch64 expects this exact filename in /tmp
  # Extract filename from URL (remove query parameters)
  local musl_name
  musl_name="$(basename "${MUSL_SO_URL%%\?*}")"
  cp -a "$WORK/libc-musl.so" "/tmp/$musl_name"
  chmod 755 "/tmp/$musl_name"
  log "Copied musl runtime to /tmp/$musl_name for qemu compatibility"
}

# Step 3: Clone piadina
clone_piadina() {
  log "3. Clone piadina"
  # Ensure we are not inside the target dir before git_clone_and_checkout deletes it
  cd "$WORK"
  git_clone_and_checkout "$PIADINA_REPO_URL" "$PIADINA_SRC" "$PIADINA_REF"
}

# Step 4: Build local libraries
build_local_libraries() {
  log "4. Build local libraries"
  cd "$PIADINA_SRC"

  git clone https://github.com/PJK/libcbor.git
  cd libcbor

  CC=musl-gcc cmake -B _build \
    -DCMAKE_C_COMPILER_AR=/usr/bin/ar \
    -DCMAKE_C_COMPILER_RANLIB=/usr/bin/ranlib \
    -DBUILD_SHARED_LIBS=OFF \
    -DWITH_TESTS=OFF

    cmake --build _build -j$(nproc)

  cd "$PIADINA_SRC"

  git clone https://github.com/libarchive/libarchive.git
  cd libarchive

  CC=musl-gcc cmake -B _build \
    -DCMAKE_C_COMPILER_AR=/usr/bin/ar \
    -DCMAKE_C_COMPILER_RANLIB=/usr/bin/ranlib \
    -DENABLE_TEST=OFF \
    -DENABLE_OPENSSL=OFF \
    -DENABLE_ZLIB=OFF \
    -DENABLE_BZip2=OFF \
    -DENABLE_LZMA=OFF \
    -DENABLE_ZSTD=OFF \
    -DENABLE_LZ4=OFF \
    -DENABLE_EXPAT=OFF \
    -DENABLE_ICONV=OFF \
    -DPOSIX_REGEX_LIB=NONE \
    -DENABLE_LIBB2=OFF \
    -DENABLE_LIBXML2=OFF

    cmake --build _build -j$(nproc)
}

# Step 5: Build piadina and azdora
build_piadina_azdora() {
  log "5. Build piadina + azdora"

  # Ensure we are not inside the target dir before git_clone_and_checkout deletes it
  cd "$WORK"

  # Build piadina and azdora
  log "Running autogen.sh"
  (cd "$WORK/piadina-src" && chmod +x autogen.sh && ./autogen.sh)

  log "Running configure"
  (cd "$WORK/piadina-src" && ./configure)

  log "Building (using $(nproc) parallel jobs)"
  (cd "$WORK/piadina-src" && make -j"$(nproc)")

  log "Run test suite to verify that the binaries are statically linked"
  (cd "$WORK/piadina-src" && make check)

  # Verify and copy binaries
  validate_file_exists "$WORK/piadina-src/piadina/piadina" "piadina binary"
  validate_file_exists "$WORK/piadina-src/azdora/azdora" "azdora binary"

  PIADINA_BIN="$WORK/piadina"
  AZDORA_BIN="$WORK/azdora"

  cp "$WORK/piadina-src/piadina/piadina" "$PIADINA_BIN"
  cp "$WORK/piadina-src/azdora/azdora" "$AZDORA_BIN"
  chmod +x "$PIADINA_BIN" "$AZDORA_BIN"

  log "Built piadina and azdora successfully"
}

# Step 6: Install OTP
install_otp() {
  log "6. Run OTP ./Install"
  if [[ -f "$OTP_ROOT/Install" ]]; then
    chmod +x "$OTP_ROOT/Install"
    # Answer "n" to use SASL startup (default) instead of minimal system startup
    echo "n" | (cd "$OTP_ROOT" && ./Install "$(pwd)")
  else
    die "Could not find OTP Install script at $OTP_ROOT/Install"
  fi

  # Use DOWNLOADED OTP as the builder runtime for rebar3
  export PATH="$OTP_ROOT/bin:$OTP_ROOT/$ERTS_DIR/bin:$PATH"
  log "Updated PATH to use OTP from $OTP_ROOT"
}

# Step 7: Build bombom
build_bombom() {
  log "7. Build bombom"
  git_clone_and_checkout "$BOMBOM_REPO_URL" "$WORK/bombom-src" "$BOMBOM_REF"

  validate_command_exists rebar3 "expected to be installed by container/workflow wrapper"

  (cd "$WORK/bombom-src" && PATH="$OTP_ROOT/bin:$OTP_ROOT/$ERTS_DIR/bin:$PATH" rebar3 escriptize)

  BOMBOM_ESCRIPT="$WORK/bombom-src/_build/default/bin/bombom"
  validate_file_exists "$BOMBOM_ESCRIPT" "bombom escript"
  log "Built bombom successfully"
}

# Step 8: Assemble payload directory
assemble_payload() {
  log "8. Assemble payload directory"
  PAYLOAD_DIR="$WORK/cassone/bombom"
  rm -rf "$WORK/cassone"
  mkdir -p "$PAYLOAD_DIR"

  # Copy OTP layout into payload
  log "Copying OTP layout to payload"
  cp -a "$OTP_ROOT/bin" "$PAYLOAD_DIR/"
  cp -a "$OTP_ROOT/$ERTS_DIR" "$PAYLOAD_DIR/"
  cp -a "$OTP_ROOT/lib" "$PAYLOAD_DIR/"

  # Copy bombom into payload bin
  cp -a "$BOMBOM_ESCRIPT" "$PAYLOAD_DIR/bin/bombom"

  # Copy musl runtime to payload root (used by patching)
  cp -a "$WORK/libc-musl.so" "$PAYLOAD_DIR/"
  chmod +x "$PAYLOAD_DIR/libc-musl.so"

  # Remove epmd link if present (PDF Step 5)
  rm -f "$PAYLOAD_DIR/bin/epmd"

  log "Payload directory assembled at $PAYLOAD_DIR"
}

# Step 9: Package binary
package_binary() {
  log "9. Package into $OUTPUT"

  local musl_interpreter='{PAYLOAD_ROOT}/libc-musl.so'
  local patch_args=()

  # Helper to add patch argument if file exists
  add_patch_if_exists() {
    local rel="$1"
    if [[ -e "$PAYLOAD_DIR/$rel" ]]; then
      patch_args+=(--meta "PATCHELF_SET_INTERPRETER[]=${rel}:${musl_interpreter}")
    fi
  }

  # List of binaries to patch (combining erts-* and top-level bin/*)
  # These are the binaries that need musl interpreter patching
  local binaries_to_patch=(
    # ERTS binaries
    "$ERTS_DIR/bin/escript"
    "$ERTS_DIR/bin/run_erl"
    "$ERTS_DIR/bin/yielding_c_fun"
    "$ERTS_DIR/bin/erlc"
    "$ERTS_DIR/bin/epmd"
    "$ERTS_DIR/bin/dialyzer"
    "$ERTS_DIR/bin/inet_gethost"
    "$ERTS_DIR/bin/beam.smp"
    "$ERTS_DIR/bin/erlexec"
    "$ERTS_DIR/bin/to_erl"
    "$ERTS_DIR/bin/erl_child_setup"
    "$ERTS_DIR/bin/heart"
    "$ERTS_DIR/bin/dyn_erl"
    "$ERTS_DIR/bin/ct_run"
    "$ERTS_DIR/bin/typer"
    "$ERTS_DIR/bin/erl_call"
    # Top-level bin/* scripts and helpers
    "bin/run_erl"
    "bin/erlc"
    "bin/dialyzer"
    "bin/erl_call"
    "bin/to_erl"
    "bin/ct_run"
    "bin/typer"
    "bin/escript"
  )

  log "Collecting binaries for patching"
  for binary in "${binaries_to_patch[@]}"; do
    add_patch_if_exists "$binary"
  done

  # lib/erl_interface*/bin/erl_call (PDF list)
  local erl_if_dir
  erl_if_dir="$(cd "$PAYLOAD_DIR" && ls -d lib/erl_interface* 2>/dev/null | head -n1 || true)"
  if [[ -n "$erl_if_dir" ]]; then
    add_patch_if_exists "$erl_if_dir/bin/erl_call"
  fi

  # lib/os_mon*/priv/bin/{cpu_sup,memsup} (PDF list)
  local os_mon_dir
  os_mon_dir="$(cd "$PAYLOAD_DIR" && ls -d lib/os_mon* 2>/dev/null | head -n1 || true)"
  if [[ -n "$os_mon_dir" ]]; then
    add_patch_if_exists "$os_mon_dir/priv/bin/cpu_sup"
    add_patch_if_exists "$os_mon_dir/priv/bin/memsup"
  fi

  if [[ "${#patch_args[@]}" -lt 1 ]]; then
    echo "Payload root contents:" >&2
    (cd "$PAYLOAD_DIR" && find . -maxdepth 3 -type f | sed 's|^\./||' | head -n 200) >&2
    die "No PATCHELF_SET_INTERPRETER entries were generated. Payload layout unexpected."
  fi

  log "Packaging with azdora (${#patch_args[@]} binaries to patch)"
  "$AZDORA_BIN" \
    --launcher "$PIADINA_BIN" \
    --payload "$PAYLOAD_DIR" \
    --output "$DIST/$OUTPUT" \
    --meta APP_NAME="BomBom" \
    --meta APP_VER="$APP_VER" \
    --meta OTP_VER="$OTP_VERSION" \
    --meta ENTRY_POINT=bin/escript \
    --meta ENTRY_ARGS[]="{PAYLOAD_ROOT}/bin/bombom" \
    --meta ENV.ERL_ROOTDIR="{PAYLOAD_ROOT}" \
    "${patch_args[@]}"

  log "Binary packaged successfully"
}

# Step 10: Generate checksum
generate_checksum() {
  log "10. Generate checksum"
  (
    cd "$DIST"
    compute_checksum "$OUTPUT" "$OUTPUT.sha256"
  )
  log "Checksum generated: $OUTPUT.sha256"
}

#=============================================================================
# Main
#=============================================================================

main() {
  # Validate required environment variables
  : "${OTP_TARBALL_URL:?Set OTP_TARBALL_URL}"
  : "${MUSL_SO_URL:?Set MUSL_SO_URL}"
  : "${OUTPUT:?Set OUTPUT}"

  # Set optional environment variables with defaults
  : "${BOMBOM_REPO_URL:=https://github.com/stritzinger/bombom.git}"
  : "${BOMBOM_REF:=main}"
  : "${APP_VER:=dev}"
  : "${OTP_VERSION:=unknown}"
  : "${PIADINA_REPO_URL:=https://github.com/stritzinger/piadina.git}"
  : "${PIADINA_REF:=main}"

  # Setup directories
  readonly ROOT="$(pwd)"
  readonly WORK="$ROOT/_work"
  readonly DIST="$ROOT/dist"
  readonly PIADINA_SRC="$WORK/piadina-src"

  mkdir -p "$WORK" "$DIST"

  # Declare variables that will be set by build steps
  local OTP_ROOT
  local ERTS_DIR
  local PIADINA_BIN
  local AZDORA_BIN
  local BOMBOM_ESCRIPT
  local PAYLOAD_DIR

  # Execute build steps
  download_and_extract_otp
  download_musl
  clone_piadina
  build_local_libraries
  build_piadina_azdora
  install_otp
  build_bombom
  assemble_payload
  package_binary
  generate_checksum

  log "Done"
  ls -lh "$DIST/$OUTPUT" "$DIST/$OUTPUT.sha256"
}

main "$@"
