#!/usr/bin/env bash
# Install line-breaker from GitHub releases
#
# Usage:
#   curl -sL https://raw.githubusercontent.com/hugoduncan/line-breaker/master/scripts/install.sh | bash
#   ./install.sh [--install-dir /path/to/dir]
#
# Options:
#   -d, --install-dir DIR   Install binary to DIR (default: /usr/local/bin)

set -euo pipefail

REPO="hugoduncan/line-breaker"
INSTALL_DIR="/usr/local/bin"
BINARY_NAME="line-breaker"
TMP_DIR=""

cleanup() {
  if [[ -n "$TMP_DIR" && -d "$TMP_DIR" ]]; then
    rm -rf "$TMP_DIR"
  fi
}
trap cleanup EXIT

die() {
  echo "Error: $1" >&2
  exit 1
}

parse_args() {
  while [[ $# -gt 0 ]]; do
    case "$1" in
      -d|--install-dir)
        if [[ $# -lt 2 ]]; then
          die "--install-dir requires an argument"
        fi
        INSTALL_DIR="$2"
        shift 2
        ;;
      -h|--help)
        echo "Usage: $0 [--install-dir DIR]"
        echo ""
        echo "Options:"
        echo "  -d, --install-dir DIR   Install binary to DIR (default: /usr/local/bin)"
        exit 0
        ;;
      *)
        die "Unknown option: $1"
        ;;
    esac
  done
}

detect_platform() {
  local os arch platform

  os="$(uname -s | tr '[:upper:]' '[:lower:]')"
  arch="$(uname -m)"

  case "$os" in
    darwin)
      # macOS: always use x86_64 binary (works on arm64 via Rosetta)
      platform="darwin-x86_64"
      ;;
    linux)
      case "$arch" in
        x86_64)
          platform="linux-x86_64"
          ;;
        aarch64|arm64)
          die "ARM64 Linux is not supported. Only x86_64 binaries are available."
          ;;
        *)
          die "Unsupported architecture: $arch"
          ;;
      esac
      ;;
    *)
      die "Unsupported operating system: $os"
      ;;
  esac

  echo "$platform"
}

fetch_latest_version() {
  local api_url response tag

  api_url="https://api.github.com/repos/${REPO}/releases/latest"

  if command -v curl &> /dev/null; then
    response=$(curl -sL "$api_url")
  elif command -v wget &> /dev/null; then
    response=$(wget -qO- "$api_url")
  else
    die "Neither curl nor wget found. Please install one of them."
  fi

  tag=$(echo "$response" | grep -o '"tag_name": *"[^"]*"' | head -1 | cut -d'"' -f4)

  if [[ -z "$tag" ]]; then
    die "Failed to fetch latest release version"
  fi

  echo "$tag"
}

download_file() {
  local url="$1"
  local dest="$2"

  if command -v curl &> /dev/null; then
    if ! curl -fsSL "$url" -o "$dest"; then
      die "Failed to download: $url"
    fi
  elif command -v wget &> /dev/null; then
    if ! wget -q "$url" -O "$dest"; then
      die "Failed to download: $url"
    fi
  else
    die "Neither curl nor wget found"
  fi
}

verify_checksum() {
  local file="$1"
  local checksum_file="$2"
  local expected actual

  expected=$(awk '{print $1}' "$checksum_file")

  if command -v sha256sum &> /dev/null; then
    actual=$(sha256sum "$file" | awk '{print $1}')
  elif command -v shasum &> /dev/null; then
    actual=$(shasum -a 256 "$file" | awk '{print $1}')
  else
    echo "Warning: Cannot verify checksum (sha256sum/shasum not found)"
    return 0
  fi

  if [[ "$expected" != "$actual" ]]; then
    die "Checksum verification failed. Expected: $expected, Got: $actual"
  fi

  echo "Checksum verified."
}

backup_binary() {
  local install_path="$1"

  if [[ -f "$install_path" ]]; then
    local backup_path="${install_path}.old"
    echo "Backing up existing binary to ${backup_path}"
    mv "$install_path" "$backup_path"
  fi
}

install_binary() {
  local platform version binary_name binary_url checksum_url
  local tmp_binary tmp_checksum install_path

  platform=$(detect_platform)
  echo "Detected platform: $platform"

  version=$(fetch_latest_version)
  echo "Latest version: $version"

  # Extract version number from tag (e.g., v0.1.123 -> 0.1.123)
  local version_num="${version#v}"

  binary_name="line-breaker-${version_num}-${platform}"
  binary_url="https://github.com/${REPO}/releases/download/${version}/${binary_name}"
  checksum_url="${binary_url}.sha256"

  TMP_DIR=$(mktemp -d)
  tmp_binary="${TMP_DIR}/${binary_name}"
  tmp_checksum="${TMP_DIR}/${binary_name}.sha256"

  echo "Downloading ${binary_name}..."
  download_file "$binary_url" "$tmp_binary"
  download_file "$checksum_url" "$tmp_checksum"

  verify_checksum "$tmp_binary" "$tmp_checksum"

  install_path="${INSTALL_DIR}/${BINARY_NAME}"

  # Create install directory if it doesn't exist
  if [[ ! -d "$INSTALL_DIR" ]]; then
    echo "Creating directory: $INSTALL_DIR"
    mkdir -p "$INSTALL_DIR"
  fi

  backup_binary "$install_path"

  echo "Installing to ${install_path}"
  mv "$tmp_binary" "$install_path"
  chmod +x "$install_path"

  echo ""
  echo "Successfully installed ${BINARY_NAME} ${version} to ${install_path}"
  echo ""
  echo "Usage:"
  echo "  ${BINARY_NAME} --check FILE    Check file for long lines"
  echo "  ${BINARY_NAME} --fix FILE      Fix long lines in place"
  echo "  ${BINARY_NAME} --stdout FILE   Output fixed content to stdout"
}

main() {
  parse_args "$@"
  install_binary
}

main "$@"
