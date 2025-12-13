# MUMPS IDE Branding

## Logo & Visual Identity

### Logo
- **Design**: Letter "M" on gradient background
- **Colors**: Purple gradient (#667eea → #764ba2)
- **Shape**: Rounded square (24px border radius for 120px size)
- **Typography**: Bold, modern sans-serif

### Color Palette

**Primary Gradient**
- Start: `#667eea` (Purple Blue)
- End: `#764ba2` (Deep Purple)

**Dark Theme Background**
- Primary: `#1a1a2e` (Deep Navy)
- Secondary: `#16213e` (Navy Blue)
- Accent: `#0f3460` (Dark Blue)

**Text Colors**
- Primary: `#ffffff` (White)
- Secondary: `rgba(255, 255, 255, 0.7)` (70% White)
- Tertiary: `rgba(255, 255, 255, 0.5)` (50% White)

## Splash Screen

The splash screen appears for 2.5 seconds when the IDE starts.

**Elements:**
1. **Logo**: 120x120px "M" with gradient
2. **Title**: "MUMPS Language IDE" (42px, bold)
3. **Subtitle**: "Developed by Ahmad Alkhalaileh" (18px)
4. **Version**: "Version 1.0" (14px, uppercase)
5. **Loading bar**: Animated gradient progress bar

**Animations:**
- Fade in: 0.8s
- Logo pulse: 2s loop
- Loading bar: 2s gradient animation
- Fade out: 0.6s

## Brand Icon (Header)

Located in the top-left corner of the IDE:
- Size: 32x32px
- Same gradient as splash logo
- Letter "M" in white
- 6px border radius

## Typography

**Primary Font**: Segoe UI, Tahoma, Geneva, Verdana, sans-serif

**Font Weights:**
- Regular: 400 (body text)
- Medium: 500 (subtitles)
- Bold: 700 (titles)
- Heavy: 900 (logo "M")

## Usage Guidelines

### Logo
- Minimum size: 32px
- Clear space: 20% of logo size around all sides
- Always use on dark background for best visibility
- Don't distort or change aspect ratio

### Colors
- Use gradient for branding elements only
- Don't use gradient on text (except loading bars)
- Maintain contrast ratios for accessibility

### Typography
- Logo "M" always in white
- Titles in white
- Body text in white with opacity for hierarchy

## File Locations

```
styles/splash.css     - Splash screen styles
index.html            - Splash screen HTML
icon.png              - App icon (512x512)
create-icon.sh        - Script to generate icon
```

## Creating Icons

### For Snap Package
```bash
./create-icon.sh
cp icon.png snap/gui/icon.png
```

### Different Sizes
```bash
# 512x512 (original)
./create-icon.sh

# 256x256
convert icon.png -resize 256x256 icon-256.png

# 128x128
convert icon.png -resize 128x128 icon-128.png

# 64x64
convert icon.png -resize 64x64 icon-64.png

# 32x32
convert icon.png -resize 32x32 icon-32.png

# 16x16 (favicon)
convert icon.png -resize 16x16 favicon.ico
```

## Brand Assets

All brand assets should maintain:
1. **Consistency**: Use same colors and typography
2. **Quality**: Vector or high-DPI when possible
3. **Accessibility**: Proper contrast ratios
4. **Recognition**: "M" logo for instant recognition

## Examples

### Marketing Use
- Social media posts: Use splash screen screenshot
- Documentation: Use header with M logo
- Website: Full splash screen as hero image

### Technical Use
- App icon: 512x512 PNG
- Snap store: icon.png + screenshots
- Desktop entry: icon.png
- Favicon: 16x16 ICO

---

**Brand Identity Created**: December 2025
**Designer**: Ahmad Alkhalaileh
