# Bytecode Spec

Little endianness

## Header

| Name         | Type   | Description                          |
|--------------|--------|--------------------------------------|
| Magic        | u8[4]  | `"TPBC"`                             |
| Version      | u16    | Currently `3`                        |
| Reserved     | u16    | Always `0`                           |
| Locals Count | u16    | Top-level locals (stack frame size)  |
| Offsets      | u64[4] | Starting positions of each section   |

Section offset indices:

| Name                          | Section Index |
|-------------------------------|---------------|
| [Globals](#Globals)           | 0             |
| [Constants](#Constants)       | 1             |
| [Instructions](#Instructions) | 2             |
| [Debug Info](#DebugInfo)      | 3             |

## Globals

| Name  | Type                     |
|-------|--------------------------|
| Count | u64                      |
| Items | [Global](#Global)[Count] |

### Global

| Name       | Type       |
|------------|------------|
| Length     | u8         |
| Name       | u8[Length] |
| Index      | u32        |
| Is Mutable | u8         |
| UUID       | u8[17]     |

## Constants

| Name  | Type                         |
|-------|------------------------------|
| Count | u64                          |
| Items | [Constant](#Constant)[Count] |

### Constant

| Name  | Type                                                                                                                                                                                           |
|-------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Type  | [Type](#ValueType)                                                                                                                                                                             |
| Value | [Nil](#NilVoid) \| [Void](#NilVoid) \| [Bool](#Bool) \| [Number](#Number) \| [Range](#Range) \| [Visit](#Visit) \| [Timestamp](#Timestamp) \| [ConstString](#ConstString) \| [Object](#Object) |

## ValueType

Used for `Constant.Type`:
(u8 enum)

| ID | Name         |
|----|--------------|
| 0  | void         |
| 1  | nil          |
| 2  | bool         |
| 3  | number       |
| 4  | range        | // not constant
| 5  | obj          |
| 6  | map_pair     | // not constant
| 7  | visit        |
| 8  | enum_value   | // not constant
| 9  | timestamp    |
| 10 | const_string |
| 11 | ref          | // not constant

#### Nil/Void

Empty

#### Bool

| Name  | Type          |
|-------|---------------|
| Value | u8 ('1'\|'0') |

#### Number

Stringified value up to 5 decimal places

| Name   | Type       |
|--------|------------|
| Length | u8         |
| Value  | u8[Length] |

#### Range

| Name  | Type |
|-------|------|
| Start | i32  |
| End   | i32  |

#### Visit

| Name  | Type |
|-------|------|
| Value | u32  |

#### EnumValue

| Name   | Type       |
|--------|------------|
| Length | u8         |
| Name   | u8[Length] |

#### Timestamp

| Name  | Type |
|-------|------|
| Value | i64  |

#### ConstString

Plain UTF-8 string with no interpolation segments. (See [Object → String](#String)
for interpolated dialogue/locale strings.)

| Name   | Type        |
|--------|-------------|
| Length | u16         |
| Value  | u8[Length]  |

#### Object

| Name  | Type                                                                                                                                                            |
|-------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Type  | [Object Type](#ObjectType)                                                                                                                                      |
| id    | [UUID](#UUID)                                                                                                                                                   |
| Value | [String](#String) \| [Enum](#Enum) \| [Function](#Function) \| [ExternFunction](#ExternFunction) \| [Builtin](#Builtin) \| [Class](#Class) \| [Anchor](#Anchor) |

## ObjectType

Used for `Object.Type`:
(u8 enum)

| ID | Name     |
|----|----------|
| 0  | string   |
| 1  | enum     |
| 2  | list     | // not constant
| 3  | map      | // not constant
| 4  | set      | // not constant
| 5  | function |
| 6  | extern   |
| 7  | builtin  |
| 8  | class    |
| 9  | instance | // not constant
| 10 | anchor   |

#### String

Bytes blob plus a segment table that the VM walks to splice literal runs and
interpolated args. The bytes blob includes `{N}` markers (kept for cat-readability);
the segment table tells the runtime which byte ranges are literal and which slots
are interpolations.

| Name           | Type                          |
|----------------|-------------------------------|
| Length         | u16                           |
| Value          | u8[Length]                    |
| Segment Count  | u8                            |
| Segments       | [Segment](#Segment)[Count]    |

#### Segment

| Name | Type                                                                 |
|------|----------------------------------------------------------------------|
| Tag  | u8 (`0` = literal, `1` = interp)                                     |
| Body | [SegmentLiteral](#SegmentLiteral) \| [SegmentInterp](#SegmentInterp) |

##### SegmentLiteral

Byte range into the parent String's `Value`.

| Name  | Type |
|-------|------|
| Start | u16  |
| End   | u16  |

##### SegmentInterp

Stack-relative arg index for an interpolation slot.

| Name  | Type |
|-------|------|
| Index | u8   |

#### Enum

| Name        | Type                            |
|-------------|---------------------------------|
| Name Length | u8                              |
| Name        | u8[Name Length]                 |
| Is Sequence | u8                              |
| Count       | u8                              |
| Items       | [Enum Value](#EnumValue)[Count] |

#### Function

The Name field is a u16 length-prefixed blob; an unnamed function writes
length `0` and no bytes.

| Name               | Type                                      |
|--------------------|-------------------------------------------|
| Name Length        | u16                                       |
| Name               | u8[Name Length]                           |
| Arity              | u8                                        |
| Is Method          | u8 (0\|1)                                 |
| Locals Count       | u16                                       |
| Instructions Count | u32                                       |
| Instructions       | u8[Instructions Count]                    |
| Debug Info Count   | u32                                       |
| Debug Info         | [DebugItem](#DebugItem)[Debug Info Count] |

#### ExternFunction

| Name   | Type       |
|--------|------------|
| Length | u8         |
| Name   | u8[Length] |
| Arity  | u8         |

#### Builtin

| Name   | Type       |
|--------|------------|
| Length | u8         |
| Name   | u8[Length] |

#### Class

| Name         | Type                            |
|--------------|---------------------------------|
| Name Length  | u8                              |
| Name         | u8[Name Length]                 |
| Field Count  | u8                              |
| Fields       | [Member](#Member)[Field Count]  |
| Method Count | u8                              |
| Methods      | [Member](#Member)[Method Count] |

#### Member

| Name        | Type                  |
|-------------|-----------------------|
| Name Length | u8                    |
| Name        | u8[Name Length]       |
| Value       | [Constant](#Constant) |

#### Anchor

| Name                      | Type              |
|---------------------------|-------------------|
| Length                    | u16               |
| Name                      | u8[Length]        |
| Ip                        | u32               |
| VisitGlobalsIndex         | u32               |
| HasParentAnchor           | u8 (0\|1)         |
| ParentAnchorConstantIndex | [u32](#Constants) | # if hasParentAnchor == 1

## Instructions

| Name         | Type |
|--------------|------|
| Count        | u64  |
| Instructions | u8[] |

## DebugInfo

| Name  | Type                            |
|-------|---------------------------------|
| Count | u32                             |
| Info  | [Debug Item](#DebugItem)[Count] |

### DebugItem

| Name        | Type                                   |
|-------------|----------------------------------------|
| Length      | u16                                    |
| File Name   | u8[Length]                             |
| Range Count | u16                                    |
| Range       | [DebugRange](#DebugRange)[Range Count] |

### DebugRange

| Name  | Type |
|-------|------|
| Start | u32  |
| End   | u32  |
| Line  | u32  |

## UUID

| Name  | Type   |
|-------|--------|
| Value | u8[17] |



