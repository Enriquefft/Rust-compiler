//! Shared compile-time constants used across compiler stages.
//!
//! This module centralizes constants that must be kept in sync between
//! MIR lowering and backend code generation. Having them in one place
//! ensures consistency and makes it easier to adjust limits.
//!
//! ## Field Layout Strategy
//!
//! Struct fields use declaration order for layout computation. Each field is
//! placed at a sequential offset from the struct's base address using
//! LOCAL_STACK_MULTIPLIER spacing. This ensures stable, collision-free access
//! compared to the previous hash-based approach.

/// Maximum number of struct fields supported.
/// This limits local allocation for struct variables.
/// Note: Field layout now uses declaration order, not hash-based indexing.
pub const MAX_STRUCT_FIELDS: u32 = 16;

/// Assumed number of fields for generic type parameters when the concrete type is unknown.
/// This is a workaround for lack of full monomorphization - assumes structs have this many fields.
/// Generic types with more fields than this value may have incorrect behavior.
pub const ASSUMED_GENERIC_STRUCT_FIELDS: u32 = 4;

/// Multiplier for local variable stack allocation.
/// Each local gets this many 8-byte slots to accommodate structs and arrays.
/// A higher value allows larger structs but uses more stack space.
/// Field offsets are computed as: field_index * LOCAL_STACK_MULTIPLIER * sizeof(i64)
pub const LOCAL_STACK_MULTIPLIER: u32 = 4;

/// Maximum number of additional array elements (beyond the first) that can be
/// stored via physical registers during array initialization.
/// Element 1 -> rdx, element 2 -> rcx, element 3 -> r8, element 4 -> r9,
/// element 5 -> r10, element 6 -> r12, element 7 -> r13
pub const MAX_EXTRA_ARRAY_ELEMENTS: usize = 7;

/// Offset from the first field to the second field in a struct.
/// This is calculated as: -field_index * LOCAL_STACK_MULTIPLIER * sizeof(i64)
/// For the second field (index 1): -1 * 4 * 8 = -32
///
/// Note: Currently only 2-field structs are fully supported for passing/returning
/// through registers. The first field goes in rax/vreg, the second in rdx.
/// This matches the System V ABI for small struct returns.
pub const STRUCT_SECOND_FIELD_OFFSET: i64 = -@as(i64, @intCast(@sizeOf(i64) * LOCAL_STACK_MULTIPLIER));
