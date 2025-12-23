#!/usr/bin/env bash
set -euo pipefail

# Local Docker runner for the CI build (amd64 + arm64)
# Artifacts end up in ./dist/<arch>/
#
# Usage:
#   ./scripts/docker.sh [amd64|arm64]
#   If no architecture is specified, builds both amd64 and arm64

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DIST_ROOT="$ROOT/dist"

OTP_VERSION="${OTP_VERSION:-28.1.1}"
OPENSSL_VERSION="${OPENSSL_VERSION:-3.5.1}"
MUSL_VERSION="${MUSL_VERSION:-1.2.5}"

BOMBOM_REPO_URL="${BOMBOM_REPO_URL:-https://github.com/stritzinger/bombom.git}"
BOMBOM_REF="${BOMBOM_REF:-main}"
PIADINA_REPO_URL="${PIADINA_REPO_URL:-https://github.com/stritzinger/piadina.git}"
PIADINA_REF="${PIADINA_REF:-main}"

OTP_CDN_BASE_URL="${OTP_CDN_BASE_URL:-https://beam-machine-universal.b-cdn.net}"
BEAMMACHINE_HOME_URL="${BEAMMACHINE_HOME_URL:-https://beammachine.cloud/}"
REBAR3_URL="${REBAR3_URL:-https://s3.amazonaws.com/rebar3/rebar3}"

IMAGE_BASE="${IMAGE_BASE:-bombom}"

need_cmd() { command -v "$1" >/dev/null 2>&1 || { echo "Missing: $1" >&2; exit 1; }; }
need_cmd docker

# Ensure buildx is available
docker buildx version >/dev/null 2>&1 || {
  echo "docker buildx is required (Docker Desktop usually includes it)." >&2
  exit 1
}

# Register QEMU (needed if you build/run arm64 on an amd64 host)
docker run --privileged --rm tonistiigi/binfmt --install all >/dev/null 2>&1 || true

run_one() {
  local arch="$1"
  local platform="$2"
  local image="${IMAGE_BASE}:${arch}"

  mkdir -p "$DIST_ROOT/$arch"

  # Build image for the specific platform if it doesn't exist
  if ! docker image inspect "$image" >/dev/null 2>&1; then
    echo "Building image $image for platform $platform..."
    docker buildx build \
      --load \
      --platform "$platform" \
      -t "$image" \
      -f "$ROOT/scripts/Dockerfile.build-linux" \
      "$ROOT"
  else
    echo "Image $image already exists, using existing image"
  fi

  docker run --rm \
    --platform "$platform" \
    -e ARCH_IN="$arch" \
    -e OTP_VERSION="$OTP_VERSION" \
    -e OPENSSL_VERSION="$OPENSSL_VERSION" \
    -e MUSL_VERSION="$MUSL_VERSION" \
    -e OTP_CDN_BASE_URL="$OTP_CDN_BASE_URL" \
    -e BEAMMACHINE_HOME_URL="$BEAMMACHINE_HOME_URL" \
    -e REBAR3_URL="$REBAR3_URL" \
    -e BOMBOM_REPO_URL="$BOMBOM_REPO_URL" \
    -e BOMBOM_REF="$BOMBOM_REF" \
    -e PIADINA_REPO_URL="$PIADINA_REPO_URL" \
    -e PIADINA_REF="$PIADINA_REF" \
    -e APP_VER="${APP_VER:-dev-local}" \
    -v "$ROOT:/work:rw" \
    -w /work \
    "$image" \
    bash -lc '
      set -euo pipefail

      # Map arch -> BM_ARCH / NEEDLE_ARCH (same as workflow)
      case "${ARCH_IN}" in
        amd64) BM_ARCH="x86_64"; OUT_ARCH="amd64"; NEEDLE_ARCH="x86_64" ;;
        arm64) BM_ARCH="aarch64"; OUT_ARCH="arm64"; NEEDLE_ARCH="aarch64" ;;
        *) echo "Unsupported arch: ${ARCH_IN} (use amd64 or arm64)" >&2; exit 1 ;;
      esac

      OTP="${OTP_VERSION}"
      OPENSSL="${OPENSSL_VERSION}"
      MUSLVER="${MUSL_VERSION}"

      OTP_TARBALL_URL="${OTP_CDN_BASE_URL}/OTP-${OTP}/linux/${BM_ARCH}/any/otp_${OTP}_linux_any_${BM_ARCH}.tar.gz?please-respect-my-bandwidth-costs=thank-you&openssl=${OPENSSL}&musl=${MUSLVER}"

      # Resolve musl runtime URL by scraping the beammachine page (same logic as workflow, but in bash here)
      HTML=$(curl -fsSL --max-time 20 --retry 3 --retry-delay 2 --retry-all-errors "${BEAMMACHINE_HOME_URL}")
      export HTML NEEDLE_ARCH HOME_URL="${BEAMMACHINE_HOME_URL}"

      MUSL_SO_URL=$(python3 <<'PY'
import os, re, sys, html as htmllib
from urllib.parse import urljoin

base = os.environ["HOME_URL"]
page = os.environ["HTML"]
needle = os.environ["NEEDLE_ARCH"].lower()

anchors = re.findall(
  r"<a\s+[^>]*href=\"([^\"]+)\"[^>]*>(.*?)</a>",
  page,
  flags=re.IGNORECASE | re.DOTALL
)

def norm(s: str) -> str:
  s = re.sub(r"<[^>]+>", " ", s)
  s = htmllib.unescape(s)
  return re.sub(r"\s+", " ", s).strip().lower()

for href, inner in anchors:
  href_u = htmllib.unescape(href)
  hay = (href_u + " " + norm(inner)).lower()
  if "runtime" in hay and needle in hay:
    print(urljoin(base, href_u))
    sys.exit(0)

sys.exit(1)
PY
) || {
        echo "Could not find musl runtime URL for arch: ${NEEDLE_ARCH}" >&2
        exit 1
      }

      # Validate URL is reachable
      curl -fsSI --max-time 20 "${MUSL_SO_URL}" >/dev/null

      # Install rebar3
      curl -fL "${REBAR3_URL}" -o rebar3
      chmod +x rebar3
      mv rebar3 /usr/local/bin/rebar3
      rebar3 --version || true

      export OTP_TARBALL_URL MUSL_SO_URL
      export OTP_VERSION="${OTP}"
      export BOMBOM_REPO_URL BOMBOM_REF PIADINA_REPO_URL PIADINA_REF
      export OUTPUT="bombom-linux-${OUT_ARCH}.bin"

      chmod +x scripts/build-linux.sh
      ./scripts/build-linux.sh

      # Move artifacts into dist/<arch>/
      mkdir -p "dist/${ARCH_IN}"
      mv -f "dist/${OUTPUT}" "dist/${ARCH_IN}/${OUTPUT}"
      mv -f "dist/${OUTPUT}.sha256" "dist/${ARCH_IN}/${OUTPUT}.sha256"

      echo "Artifacts:"
      ls -lh "dist/${ARCH_IN}/" || true
    '
}

main() {
  local arch="${1:-}"
  local arches=()

  case "$arch" in
    amd64|arm64)
      arches=("$arch")
      ;;
    "")
      # No architecture specified, build both
      arches=(amd64 arm64)
      ;;
    *)
      echo "ERROR: Invalid architecture '$arch'. Use 'amd64' or 'arm64'." >&2
      echo "Usage: $0 [amd64|arm64]" >&2
      exit 1
      ;;
  esac

  for a in "${arches[@]}"; do
    case "$a" in
      amd64) run_one amd64 linux/amd64 ;;
      arm64) run_one arm64 linux/arm64 ;;
    esac
  done

  echo
  echo "Done. Artifacts are under:"
  for a in "${arches[@]}"; do
    echo "  $DIST_ROOT/$a/"
  done
}

main "$@"
