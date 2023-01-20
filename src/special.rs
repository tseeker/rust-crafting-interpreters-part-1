use std::collections::HashMap;

use lazy_static::lazy_static;

/// The various "special" class members.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SpecialClassMember {
    Init,
    ToString,
    Compare,
}

/// The characteristics of a special class member.
#[derive(Debug, Clone)]
pub struct SpecialCharacteristics {
    /// The type of the special member
    pub which: SpecialClassMember,
    /// The identifier for the member
    pub identifier: &'static str,
    /// The minimal amount of arguments for the member
    pub min_args: usize,
    /// The maximal amount of arguments for the member
    pub max_args: usize,
}

lazy_static! {
    /// A map that can be used to retrieve a special member's characteristics.
    pub static ref SPECIAL_MEMBERS: HashMap<SpecialClassMember, SpecialCharacteristics> = {
        let characteristics = vec![
            SpecialCharacteristics {
                which: SpecialClassMember::Init,
                identifier: "init",
                min_args: 0,
                max_args: usize::MAX,
            },
            SpecialCharacteristics {
                which: SpecialClassMember::ToString,
                identifier: "to_string",
                min_args: 0,
                max_args: 0,
            },
        ];
        characteristics.into_iter().map(|c| (c.which, c)).collect()
    };
    /// A map associating special member identifiers to types.
    pub static ref SPECIAL_MEMBER_IDENTIFIERS: HashMap<&'static str, SpecialClassMember> = {
        SPECIAL_MEMBERS
            .iter()
            .map(|(k, v)| (v.identifier, *k))
            .collect()
    };
}
