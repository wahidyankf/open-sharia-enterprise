# How to Set Up Android Development on This Machine

## Overview

This guide covers installing the Android SDK and configuring the development environment
for building the `organiclever-app` Flutter Android target on this machine.

**Platform**: ARM64 Linux (Ubuntu 24.04, running on Apple Silicon via Docker/Colima)

**Why this guide exists**: Google's Android SDK `platform-tools` ships only x86_64 binaries.
On ARM64 Linux, the `adb` binary requires a native ARM64 replacement sourced from Ubuntu's
apt repository. Gradle 8.x also does not support Java 25 (class file major version 69),
which required upgrading to Gradle 9.1.0 + AGP 9.0.1 — see the Android project files
for the current versions.

## Prerequisites

Before starting, verify these tools are available:

- **Java 25** — `java -version` (Eclipse Temurin 25 recommended)
- **Flutter 3.41.1+** — `flutter --version`
- **sudo access** — needed for the ARM64 `adb` fix

## Steps

### 1. Create the SDK directory

```bash
mkdir -p ~/Android/Sdk/cmdline-tools
```

### 2. Download and extract Android command-line tools

```bash
cd ~/Android/Sdk/cmdline-tools

# Download (165 MB)
wget https://dl.google.com/android/repository/commandlinetools-linux-14742923_latest.zip \
  -O cmdline-tools.zip

# Extract and rename to the required "latest" subfolder
unzip -q cmdline-tools.zip
mv cmdline-tools latest
rm cmdline-tools.zip
```

This structure is required by `sdkmanager`:

```
~/Android/Sdk/
└── cmdline-tools/
    └── latest/
        └── bin/
            └── sdkmanager
```

### 3. Accept SDK licenses

```bash
printf 'y\ny\ny\ny\ny\ny\ny\ny\ny\ny\n' | \
  ~/Android/Sdk/cmdline-tools/latest/bin/sdkmanager --licenses
```

Expected last line: `All SDK package licenses accepted`

### 4. Install required SDK components

Flutter 3.41.1 requires Android SDK 36. Install all required components:

```bash
~/Android/Sdk/cmdline-tools/latest/bin/sdkmanager \
  "platform-tools" \
  "platforms;android-35" \
  "platforms;android-36" \
  "build-tools;35.0.0"
```

### 5. Fix ARM64 adb (ARM64 Linux only)

Google's `platform-tools` package ships x86_64 `adb` only. On ARM64 Linux, use the
Ubuntu-packaged ARM64 binary instead:

```bash
# Install ARM64 adb from Ubuntu
sudo apt-get install -y adb

# Replace the x86_64 binary with a symlink to the ARM64 one
mv ~/Android/Sdk/platform-tools/adb ~/Android/Sdk/platform-tools/adb.x86_64
ln -sf /usr/lib/android-sdk/platform-tools/adb ~/Android/Sdk/platform-tools/adb
```

Verify:

```bash
file ~/Android/Sdk/platform-tools/adb
# Expected: ... ARM aarch64 ...
```

### 6. Configure Flutter to use the SDK

```bash
export PATH="$PATH:$HOME/flutter/bin"
flutter config --android-sdk ~/Android/Sdk
```

### 7. Update local.properties

Add `sdk.dir` to the Android project's local properties file
(this file is gitignored — each developer sets it locally):

```bash
echo "sdk.dir=$HOME/Android/Sdk" >> \
  apps/organiclever-app/android/local.properties
```

Verify the file contains both entries:

```
flutter.sdk=~/flutter
sdk.dir=~/Android/Sdk
```

### 8. Add SDK to shell configuration

Add to `~/.zshrc` (or `~/.bashrc`):

```bash
cat >> ~/.zshrc << 'EOF'

# Android SDK
export ANDROID_HOME="$HOME/Android/Sdk"
export PATH="$PATH:$ANDROID_HOME/cmdline-tools/latest/bin:$ANDROID_HOME/platform-tools"
EOF

source ~/.zshrc
```

### 9. Verify

```bash
flutter doctor
```

Expected output for Android-relevant checks:

```
[✓] Flutter (Channel stable, 3.41.1, ...)
[✓] Android toolchain - develop for Android devices (Android SDK version 35.0.0)
[✓] Connected device (1 available)
```

The `Connected device` check may show a device as offline if no physical device or
emulator is connected — this is normal for a server environment.

## After Setup

The Android build system (Gradle) is already configured in the project:

- **Gradle**: 9.1.0 (in `android/gradle/wrapper/gradle-wrapper.properties`)
- **AGP**: 9.0.1 (in `android/settings.gradle.kts`)
- **Kotlin**: Built-in (AGP 9 default, no separate `kotlin-android` plugin needed)

Build the Android APK from the repository root:

```bash
cd apps/organiclever-app
export PATH="$PATH:$HOME/flutter/bin"
flutter build apk --debug
```

## Disk Space

| Component            | Size        |
| -------------------- | ----------- |
| Command-line tools   | ~165 MB     |
| platform-tools       | ~20 MB      |
| platforms;android-35 | ~100 MB     |
| platforms;android-36 | ~100 MB     |
| build-tools;35.0.0   | ~400 MB     |
| **Total**            | **~785 MB** |

## Troubleshooting

### `SDK location not found`

**Symptom**: VS Code or Gradle shows "SDK location not found".

**Fix**: `local.properties` is missing `sdk.dir`. See Step 7 above.
Note: `local.properties` is gitignored and must be set on each machine individually.

### `Unsupported class file major version 69`

**Symptom**: "Can't use Java 25.0.1 and Gradle 8.9 to import Gradle project android".

**Status**: Already fixed in the repository. The project uses Gradle 9.1.0 + AGP 9.0.1
which both support Java 25. If you see this error, verify your `gradle-wrapper.properties`
references `gradle-9.1.0-bin.zip`.

### `rosetta error: failed to open elf`

**Symptom**: `flutter doctor` shows "Unable to run adb ... rosetta error: failed to open elf at /lib64/ld-linux-x86-64.so.2".

**Fix**: Google's platform-tools `adb` is x86_64-only. Follow Step 5 above to replace
it with the ARM64 binary from Ubuntu apt.

### `flutter: command not found`

**Fix**: Add Flutter to PATH:

```bash
export PATH="$PATH:$HOME/flutter/bin"
```

Or add to `~/.zshrc` permanently:

```bash
echo 'export PATH="$PATH:$HOME/flutter/bin"' >> ~/.zshrc && source ~/.zshrc
```

## Related Documentation

- [organiclever-app README](../../apps/organiclever-app/README.md)
- [Local Dev with Docker](./hoto__local-dev-docker.md)
- [Flutter Linux Android setup](https://flutter.dev/to/linux-android-setup)
