
#!/usr/bin/env python3
"""
Convert IMGL (CLX TImageList) binary data to LCL 'Li' (Lazarus) format.

IMGL format (from .xfm files):
  - 4 bytes: "IMGL" magic
  - 2 bytes: uint16 LE version (1)
  - 2 bytes: uint16 LE (1, number of resolutions?)
  - 4 bytes: uint32 LE icon width
  - 4 bytes: uint32 LE icon height
  - 4 bytes: uint32 LE count of images
  - Color BMP: tiled grid of all icons, 24bpp
  - Mask BMP: tiled grid of all icons, 1bpp monochrome

LCL 'Li' (SIG_LAZ3) format:
  - 2 bytes: "Li" signature
  - 4 bytes: uint32 LE count
  - 4 bytes: uint32 LE width
  - 4 bytes: uint32 LE height
  - Raw RGBA data: width * height * count * 4 bytes
    Each pixel: R, G, B, A (PRGBAQuad)
    Note: FData stores pixels top-to-bottom, left-to-right
"""

import struct
import re
import sys
import os


def extract_imgl_hex_from_xfm(xfm_path, component_name):
    """Extract the hex data from a TImageList's Bitmap property in an .xfm file."""
    with open(xfm_path, 'r') as f:
        content = f.read()
    
    # Find the component
    pattern = rf'object\s+{re.escape(component_name)}\s*:\s*TImageList.*?Bitmap\s*=\s*\{{(.*?)\}}'
    match = re.search(pattern, content, re.DOTALL)
    if not match:
        raise ValueError(f"Could not find TImageList '{component_name}' with Bitmap data in {xfm_path}")
    
    hex_str = match.group(1)
    # Remove whitespace and newlines
    hex_str = re.sub(r'\s+', '', hex_str)
    return hex_str


def parse_bmp(data, offset):
    """Parse a BMP from binary data at given offset. Returns (pixels, width, height, bpp, next_offset)."""
    if data[offset:offset+2] != b'BM':
        raise ValueError(f"Not a BMP at offset {offset}: {data[offset:offset+2]}")
    
    file_size = struct.unpack_from('<I', data, offset + 2)[0]
    data_offset = struct.unpack_from('<I', data, offset + 10)[0]
    header_size = struct.unpack_from('<I', data, offset + 14)[0]
    width = struct.unpack_from('<i', data, offset + 18)[0]
    height = struct.unpack_from('<i', data, offset + 22)[0]
    bpp = struct.unpack_from('<H', data, offset + 28)[0]
    
    # BMP stores rows bottom-to-top
    top_down = height < 0
    abs_height = abs(height)
    
    # Calculate row stride (padded to 4-byte boundary)
    row_bits = width * bpp
    row_bytes = (row_bits + 7) // 8
    stride = (row_bytes + 3) & ~3
    
    pixel_data_start = offset + data_offset
    
    # Read palette if needed (for 1bpp)
    palette = None
    if bpp <= 8:
        palette_offset = offset + 14 + header_size
        num_colors = 2 ** bpp
        palette = []
        for i in range(num_colors):
            b, g, r, _ = struct.unpack_from('BBBB', data, palette_offset + i * 4)
            palette.append((r, g, b))
    
    # Read pixel data row by row
    rows = []
    for y in range(abs_height):
        row_start = pixel_data_start + y * stride
        row = []
        if bpp == 24:
            for x in range(width):
                px = row_start + x * 3
                b, g, r = data[px], data[px+1], data[px+2]
                row.append((r, g, b))
        elif bpp == 1:
            for x in range(width):
                byte_idx = x // 8
                bit_idx = 7 - (x % 8)
                bit = (data[row_start + byte_idx] >> bit_idx) & 1
                row.append(bit)
        else:
            raise ValueError(f"Unsupported BPP: {bpp}")
        rows.append(row)
    
    # BMP is bottom-to-top unless top_down
    if not top_down:
        rows.reverse()
    
    next_offset = offset + file_size
    return rows, width, abs_height, bpp, next_offset


def parse_imgl(hex_str):
    """Parse IMGL binary data. Returns (icon_width, icon_height, count, color_rows, mask_rows, bmp_width, bmp_height)."""
    data = bytes.fromhex(hex_str)
    
    # Parse IMGL header
    magic = data[0:4]
    if magic != b'IMGL':
        raise ValueError(f"Not IMGL data: {magic}")
    
    version = struct.unpack_from('<H', data, 4)[0]
    num_res = struct.unpack_from('<H', data, 6)[0]
    icon_w = struct.unpack_from('<I', data, 8)[0]
    icon_h = struct.unpack_from('<I', data, 12)[0]
    count = struct.unpack_from('<I', data, 16)[0]
    
    print(f"  IMGL: version={version}, resolutions={num_res}, icon={icon_w}x{icon_h}, count={count}")
    
    # Parse color BMP (24bpp tiled grid)
    color_rows, bmp_w, bmp_h, bpp, next_off = parse_bmp(data, 20)
    print(f"  Color BMP: {bmp_w}x{bmp_h} @ {bpp}bpp")
    
    # Parse mask BMP (1bpp)
    if next_off < len(data):
        mask_rows, mask_w, mask_h, mask_bpp, _ = parse_bmp(data, next_off)
        print(f"  Mask BMP: {mask_w}x{mask_h} @ {mask_bpp}bpp")
    else:
        mask_rows = None
        print(f"  No mask BMP found")
    
    return icon_w, icon_h, count, color_rows, mask_rows, bmp_w, bmp_h


def extract_icons_rgba(icon_w, icon_h, count, color_rows, mask_rows, bmp_w, bmp_h):
    """Extract individual icons as RGBA pixel data from the tiled grid."""
    cols = bmp_w // icon_w
    
    icons = []
    for idx in range(count):
        col = idx % cols
        row = idx // cols
        
        icon_pixels = []  # List of (R, G, B, A) tuples, top-to-bottom, left-to-right
        for y in range(icon_h):
            for x in range(icon_w):
                src_y = row * icon_h + y
                src_x = col * icon_w + x
                
                r, g, b = color_rows[src_y][src_x]
                
                # Mask: in CLX, mask bit=1 means transparent, 0 means opaque
                if mask_rows is not None:
                    mask_bit = mask_rows[src_y][src_x]
                    alpha = 0 if mask_bit else 255
                else:
                    alpha = 255
                
                icon_pixels.append((r, g, b, alpha))
        
        icons.append(icon_pixels)
    
    return icons


def icons_to_lcl_li(icon_w, icon_h, count, icons):
    """Convert icons to LCL 'Li' (SIG_LAZ3) binary format."""
    # Header
    result = b'Li'  # Signature
    result += struct.pack('<I', count)   # Count
    result += struct.pack('<I', icon_w)  # Width
    result += struct.pack('<I', icon_h)  # Height
    
    # Raw RGBA data for each icon, sequentially
    for icon_pixels in icons:
        for r, g, b, a in icon_pixels:
            # FData is array of TRGBAQuad = packed record Red, Green, Blue, Alpha: Byte
            result += struct.pack('BBBB', r, g, b, a)
    
    return result


def binary_to_lfm_hex(data, indent='      '):
    """Convert binary data to LFM hex format (lines of up to 64 hex chars)."""
    hex_str = data.hex().upper()
    lines = []
    pos = 0
    chars_per_line = 64  # 32 bytes per line
    while pos < len(hex_str):
        lines.append(indent + hex_str[pos:pos+chars_per_line])
        pos += chars_per_line
    return '\n'.join(lines)


def convert_imgl_to_lcl(xfm_path, component_name):
    """Full conversion pipeline: extract IMGL from .xfm, convert to LCL 'Li' format."""
    print(f"Converting {xfm_path} / {component_name}...")
    
    # Extract hex data
    hex_str = extract_imgl_hex_from_xfm(xfm_path, component_name)
    
    # Parse IMGL
    icon_w, icon_h, count, color_rows, mask_rows, bmp_w, bmp_h = parse_imgl(hex_str)
    
    # Extract individual icons as RGBA
    icons = extract_icons_rgba(icon_w, icon_h, count, color_rows, mask_rows, bmp_w, bmp_h)
    
    # Convert to LCL format
    lcl_data = icons_to_lcl_li(icon_w, icon_h, count, icons)
    
    # Format as LFM hex block
    lfm_hex = binary_to_lfm_hex(lcl_data)
    
    print(f"  Output: {len(lcl_data)} bytes, {count} icons @ {icon_w}x{icon_h}")
    
    return lfm_hex


def update_lfm_bitmap(lfm_path, component_name, new_hex_block):
    """Replace or insert the Bitmap data block in an .lfm file for a TImageList component."""
    with open(lfm_path, 'r') as f:
        content = f.read()
    
    # Check if Bitmap block already exists for this component
    pattern = rf'(object\s+{re.escape(component_name)}\s*:\s*TImageList.*?)Bitmap\s*=\s*\{{.*?\}}' 
    match = re.search(pattern, content, re.DOTALL)
    
    if match:
        # Replace existing Bitmap block
        old_start = match.start()
        old_end = match.end()
        # Find just the Bitmap = { ... } part
        bitmap_pattern = r'Bitmap\s*=\s*\{.*?\}'
        bitmap_match = re.search(bitmap_pattern, content[match.start():], re.DOTALL)
        if bitmap_match:
            abs_start = match.start() + bitmap_match.start()
            abs_end = match.start() + bitmap_match.end()
            new_bitmap = f"Bitmap = {{\n{new_hex_block}}}"
            content = content[:abs_start] + new_bitmap + content[abs_end:]
            print(f"  Replaced Bitmap block in {lfm_path}")
    else:
        # Need to insert Bitmap block after TImageList header
        comp_pattern = rf'(object\s+{re.escape(component_name)}\s*:\s*TImageList\b)'
        comp_match = re.search(comp_pattern, content)
        if not comp_match:
            raise ValueError(f"Component {component_name} not found in {lfm_path}")
        
        # Find the insertion point - after the last property before 'end'
        # Look for the line after the last property of this component
        insert_pos = comp_match.end()
        # Skip to end of line
        nl = content.index('\n', insert_pos)
        
        # Find next lines - look for properties (Left = , Top = ) and insert after them
        lines_after = content[nl+1:]
        line_offset = nl + 1
        last_prop_end = nl
        for line in lines_after.split('\n'):
            stripped = line.strip()
            if stripped.startswith('end') or stripped.startswith('object '):
                break
            if '=' in stripped or stripped == '':
                last_prop_end = line_offset + len(line)
            line_offset += len(line) + 1
        
        # Insert Bitmap block after last property
        bitmap_block = f"\n    Bitmap = {{\n{new_hex_block}}}"
        content = content[:last_prop_end] + bitmap_block + content[last_prop_end:]
        print(f"  Inserted Bitmap block in {lfm_path}")
    
    with open(lfm_path, 'w') as f:
        f.write(content)


# Define the 7 conversions needed
CONVERSIONS = [
    ("DBConnSelect.xfm", "DBConnSelect.lfm", "DBConnImgList"),
    ("EERPlaceModel.xfm", "EERPlaceModel.lfm", "LMImgList"),
    ("EERStoreInDatabase.xfm", "EERStoreInDatabase.lfm", "ImageList"),
    ("EditorQuery.xfm", "EditorQuery.lfm", "StoredSQLImageList"),
    ("EditorTable.xfm", "EditorTable.lfm", "DatatypesImgList"),
    ("PaletteDatatypes.xfm", "PaletteDatatypes.lfm", "DatatypesImgList"),
    ("PaletteModel.xfm", "PaletteModel.lfm", "ModelImgList"),
]


if __name__ == '__main__':
    for xfm, lfm, comp in CONVERSIONS:
        try:
            hex_block = convert_imgl_to_lcl(xfm, comp)
            update_lfm_bitmap(lfm, comp, hex_block)
            print(f"  ✓ Done: {lfm} / {comp}\n")
        except Exception as e:
            print(f"  ✗ ERROR: {e}\n")
