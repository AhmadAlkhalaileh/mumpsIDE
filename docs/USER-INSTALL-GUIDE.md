# Mumps Studio (Snap) - User install guide

## Install

```bash
sudo snap install mumps-studio
```

## Optional interfaces (for Docker/SSH/features)

```bash
sudo snap connect mumps-studio:docker
sudo snap connect mumps-studio:ssh-keys
sudo snap connect mumps-studio:ssh-public-keys
sudo snap connect mumps-studio:removable-media
```

Check connections:

```bash
snap connections mumps-studio
```

## Launch

```bash
mumps-studio
```

## Troubleshooting

```bash
mumps-studio
journalctl -e | grep -Ei "snap\\.mumps-studio|mumps-studio" || true
pkill -f "/snap/mumps-studio/" || true
```
