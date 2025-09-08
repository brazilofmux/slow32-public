# Feature: Logical Operations

## Purpose
Tests AND, OR, XOR operations with both register and immediate operands.

## Tests
- AND register (0x55 & 0x33 = 0x11)
- OR register (0x55 | 0x33 = 0x77)
- XOR register (0x55 ^ 0x33 = 0x66)
- ANDI immediate (0x55 & 0x0F = 0x05)
- ORI immediate (0x55 | 0x80 = 0xD5)
- XORI immediate (0x55 ^ 0xFF = 0xAA)

## Expected
Should output "OK" if all logical operations work correctly.