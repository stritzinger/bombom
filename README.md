<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2025 Stritzinger GmbH
-->

# bombom

A standalone Erlang escript that wraps [rebar3](https://www.rebar3.org/) and automatically injects [rebar3_sbom](https://github.com/stritzinger/rebar3_sbom) plugin into the runtime configuration of any rebar3 project, providing a convenient way to generate Software Bill of Materials (SBOM) for Erlang/OTP projects.

## Overview

`bombom` is a self-contained executable that combines rebar3 with the SBOM plugin, allowing you to generate SBOM files for your Erlang projects without needing to manually install or configure the plugin. It automatically injects the `rebar3_sbom` plugin as a dependency into any rebar3 project it's invoked in, providing a streamlined interface for SBOM generation.

The project provides two distribution formats:
- **`bombom` escript**: The standard Erlang escript that requires Erlang/OTP to be installed
- **`bombom.bin`**: A fully self-contained binary bundle that includes OTP 28.2 and all dependencies, compiled with musl-libc for maximum portability

## Features

- **Standalone executable**: No need to install rebar3 or plugins separately
- **Automatic dependency injection**: Automatically injects `rebar3_sbom` as a dependency into any rebar3 project
- **Rebar3-compatible execution**: Uses rebar3 internally and respects standard rebar3 project layouts, profiles, and configuration
- **Environment-based logging**: Control verbosity via environment variables
- **CI/CD ready**: `bombom.bin` requires no configuration or extra tooling for CI/CD integration
- **Portable binary**: `bombom.bin` works on any x86/64 Linux distribution without requiring libc compatibility

## Installation

### Using the Bundled Binary (Recommended for CI/CD)

The `bombom.bin` executable is a fully self-contained binary that includes:
- OTP 28.2 runtime
- rebar3 and all dependencies
- Automatic installation and execution

**Features:**
- ✅ No Erlang/OTP installation required
- ✅ No libc dependencies (compiled with musl-libc)
- ✅ Works on any x86/64 Linux distribution
- ✅ Zero configuration needed for CI/CD environments
- ✅ Seamlessly forwards all arguments to rebar3_sbom

Simply download and execute:
```bash
$ ./bombom.bin [sbom options]
```

The binary automatically installs and executes, requiring no additional tooling at runtime.

### Building from Source

Build the escript using rebar3:

```bash
$ rebar3 escriptize
```

This will create the executable at `_build/default/bin/bombom`.

**Requirements:**
- Erlang/OTP installed on your system
- rebar3 (for building)

After building, you can:

1. **Use directly from build directory**:
   ```bash
   $ _build/default/bin/bombom
   ```

2. **Install globally** (optional):
   ```bash
   $ cp _build/default/bin/bombom /usr/local/bin/
   ```

## Usage

Run `bombom` with any rebar3 sbom command arguments:

```bash
$ bombom [sbom options]
```

The tool automatically runs `rebar3 sbom` with the provided arguments. For example:

```bash
# Generate SBOM (default behavior)
$ bombom

# With specific options
$ bombom --format cyclonedx
```

## How It Works

`bombom` initializes a rebar3 state and injects the `rebar3_sbom` plugin into the runtime configuration when invoked in any rebar3 project directory.

**Important:** The plugin injection is performed at runtime by modifying the in-memory rebar3 state. No project files (such as `rebar.config` or `rebar.lock`) are modified on disk.

The process works as follows:

1. Loads project configuration from `rebar.config`
2. Automatically injects the `rebar3_sbom` plugin into the runtime state
3. Merges with global rebar3 configuration (if present)
4. Executes the `sbom` command with your provided arguments

The plugin is automatically fetched from:
- Repository: `https://github.com/stritzinger/rebar3_sbom.git`
- Default reference: `main`

For reproducible builds, the plugin reference is intended to be pinned to a specific tag or commit in future releases. The current default prioritizes ease of development.

### Bundled Binary Architecture

The `bombom.bin` executable is assembled using reusable tooling:
- **[Piadina](https://github.com/stritzinger/piadina)**: A generic launcher that automatically installs and executes a payload attached to itself
- **Azdora**: A packaging application that assembles the final executable

The bundled binary includes:
- OTP 28.2 runtime (compiled with musl-libc)
- All rebar3 dependencies
- The bombom escript
- All required libraries

This makes `bombom.bin` completely independent of system libraries and Erlang installations, making it ideal for CI/CD environments where you want zero-configuration SBOM generation.

## Configuration

### Global Configuration

`bombom` respects rebar3's global configuration file if it exists at:
```
$HOME/.config/rebar3/rebar.config
```

### Project Configuration

Your project's `rebar.config` will be automatically loaded and merged with the plugin configuration.

## CI/CD Integration

`bombom.bin` is designed for seamless CI/CD integration:

- **No prerequisites**: No need to install Erlang, rebar3, or any plugins
- **No configuration**: Works out of the box in any x86/64 Linux environment
- **Portable**: Compiled with musl-libc, independent of system libc
- **Self-contained**: All dependencies included in the binary
- **Docker-friendly**: Works in minimal container images, including `scratch` base images

### GitHub Actions Example

```yaml
- name: Generate SBOM
  run: |
    wget https://github.com/stritzinger/bombom/releases/latest/download/bombom.bin
    chmod +x bombom.bin
    ./bombom.bin
```

### Docker Example

`bombom.bin` is ideal for Docker multi-stage builds. Download the binary, verify its SHA256 checksum, and copy it into your container:

```dockerfile
ARG BOMBOM_VERSION
RUN curl -L https://github.com/stritzinger/bombom/releases/download/${BOMBOM_VERSION}/bombom.bin -o /usr/local/bin/bombom \
    && curl -L "https://github.com/stritzinger/bombom/releases/download/${BOMBOM_VERSION}/bombom.bin.sha256" -o /tmp/bombom.sha256 \
    && shasum -a 256 -c /tmp/bombom.sha256 \
    && chmod +x /usr/local/bin/bombom \
    && rm /tmp/bombom.sha256
```

## Dependencies

### For the Escript Version
- **rebar3**: Bundled as a dependency (version 3.25.1)
- **rebar3_sbom plugin**: Automatically fetched from GitHub
- **Erlang/OTP**: Must be installed on the system

### For the Bundled Binary
- **OTP 28.2**: Included in the binary
- **rebar3**: Included in the binary
- **rebar3_sbom plugin**: Automatically fetched from GitHub
- **All system dependencies**: Compiled with musl-libc, no external dependencies

## License

Licensed under the Apache License 2.0. See [LICENSE.md](LICENSE.md) for details.

## See Also

- [rebar3 Documentation](https://www.rebar3.org/docs)
- [rebar3_sbom Plugin](https://github.com/stritzinger/rebar3_sbom)
- [Piadina](https://github.com/stritzinger/piadina) - Generic launcher tooling
