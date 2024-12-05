# Bytecode Spec 

Little endianness

## Header

Starting Positions of each Section

| Name                          | Type |
|-------------------------------|------|
| [Globals](#Globals)           | u64  |
| [Boughs](#Boughs)             | u64  |
| [Instructions](#Instructions) | u64  |
| [Debug Info](#Debug)          | u64  |
| [Constants](#Constants)       | u64  |
| [UUIDS](#UUIDs)               | u64  |
| [Localization](#Localization) | u64  |

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
| Is Extern  | u8         |
| Is Mutable | u8         |

## Boughs

| Name  | Type                   |
|-------|------------------------|
| Count | u64                    |
| Items | [Bough](#Bough)[Count] |

### Bough

| Name     | Type       |
|----------|------------|
| Length   | u8         |
| Name     | u8[Length] |
| Position | u32        |

## Instructions

| Name         | Type |
|--------------|------|
| Count        | u64  |
| Instructions | u8[] |

## Constants

| Name  | Type                         |
|-------|------------------------------|
| Count | u64                          |
| Items | [Constant](#Constant)[Count] |

### Constant

| Name  | Type                                                                                                              |
|-------|-------------------------------------------------------------------------------------------------------------------|
| Type  | u8                                                                                                                |
| Value | [Nil](#NilVoid) \| [Void](#NilVoid) \| [Bool](#Bool) \| [Number](#Number) \| [Visit](#Visit) \| [Object](#Object) |

#### Nil/Void

Empty

#### Bool

| Name     | Type       |
|----------|------------|
| Value    | u8         |


#### Number

Stringified value up to 5 decimal places

| Name   | Type       |
|--------|------------|
| Length | u8         |
| Value  | u8[Length] |

#### Visit

| Name     | Type |
|----------|------|
| Value    | u32  |

#### Object

| Name  | Type                                                        |
|-------|-------------------------------------------------------------|
| Value | [String](#String) \| [Function](#Function) \| [Enum](#Enum) |

#### String

| Name   | Type       |
|--------|------------|
| Length | u16        |
| Value  | u8[Length] |

#### Function

| Name         | Type                         |
|--------------|------------------------------|
| Arity        | u8                           |
| Is Method    | u8                           |
| Locals Count | u16                          |
| Count        | u16                          |
| Instructions | u8[Count]                    |
| Debug Count  | u32                          |
| Debug Info   | [Debug](#Debug)[Debug Count] |

## Debug

| Name      | Type                   |
|-----------|------------------------|
| Length    | u16                    |
| File Name | u8[Length]             |
| Count     | u16                    |
| Range     | [Range](#Range)[Count] |

#### Range

| Name  | Type |
|-------|------|
| Start | u32  |
| End   | u32  |
| Line  | u32  |

#### Enum

| Name        | Type                            |
|-------------|---------------------------------|
| Length      | u16                             |
| Name        | u8[Length]                      |
| Is Sequence | u8                              |
| Count       | u8                              |
| Items       | [Enum Value](#EnumValue)[Count] |

#### EnumValue

| Name        | Type              |
|-------------|-------------------|
| Length      | u16               |
| Name        | u8[Length]        |

## UUIDs

| Name  | Type                 |
|-------|----------------------|
| Count | u64                  |
| Items | [UUID](#UUID)[Count] |

## UUID

| Name  | Type   |
|-------|--------|
| Value | u8[17] |

## Localization

| Name  | Type      |
|-------|-----------|
| Count | u128      |
| Items | u8[Count] |






