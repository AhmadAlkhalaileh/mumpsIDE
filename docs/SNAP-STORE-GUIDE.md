# Snap Store publishing guide (mumps-studio)

## 1) One-time setup

```bash
sudo snap install snapcraft --classic
sudo snap install lxd
sudo lxd init --minimal
```

Optional but recommended (avoid `sudo` for LXD):

```bash
sudo usermod -aG lxd "$USER"
newgrp lxd
```

## 2) Build (local)

Install Node dependencies (includes Electron):

```bash
npm install --include=dev
```

Build the snap (uses LXD so it matches `base: core22`):

```bash
./BUILD-SNAP.sh
```

If LXD can’t be installed on your system, build on the host instead (Ubuntu 22.04 only):

```bash
BUILD_MODE=destructive ./BUILD-SNAP.sh
```

This creates a file like `mumps-studio_<version>_amd64.snap`.

## 3) Test before uploading (required)

Install your local snap (strict confinement):

```bash
./INSTALL-LOCAL.sh
```

Launch from terminal (so you see errors):

```bash
mumps-studio
```

Verify desktop integration:

```bash
ls /var/lib/snapd/desktop/applications | grep -i mumps-studio || true
ls /var/lib/snapd/desktop/icons | grep -i mumps-studio || true
```

If the icon still doesn’t show in the app menu, log out/in (desktop cache refresh).

## 4) Upload & release (recommended safe flow)

Login:

```bash
snapcraft login
snapcraft whoami
```

If this is your first upload for this name:

```bash
snapcraft register mumps-studio
```

Upload to **edge** first:

```bash
snapcraft upload --release=edge ./mumps-studio_<version>_amd64.snap
```

Check status and get the revision number:

```bash
snapcraft status mumps-studio
```

Promote that revision to stable only after testing:

```bash
snapcraft release mumps-studio <REVISION> stable
```

## Troubleshooting

- If the app "does nothing", run it from a terminal: `mumps-studio`
- Check system logs: `journalctl -e | grep -Ei "snap\\.mumps-studio|mumps-studio" || true`
- If the process doesn't exit (or `snap remove` complains), kill it: `pkill -f "/snap/mumps-studio/" || true`
- If `snap remove mumps-studio` is stuck, you can still test a new local build by installing it as a parallel instance:
  - `sudo snap install --dangerous --name mumps-studio_test ./mumps-studio_<version>_amd64.snap`
  - Run: `snap run mumps-studio_test.mumps-studio`
- Avoid `snapcraft --destructive-mode` unless your host matches the snap base (Ubuntu 22.04), otherwise you can ship incompatible GTK/libs and the app won’t start.
