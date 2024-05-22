// structs1.rs
//
// Address all the TODOs to make the tests pass!
//
// Execute `rustlings hint structs1` or use the `hint` watch subcommand for a
// hint.

struct ColorClassicStruct {
    red: u8,
    green: u8,
    blue: u8,
}

struct ColorTupleStruct(u8, u8, u8);

#[derive(Debug)]
struct UnitLikeStruct;

trait SomeTrait {
    fn my_super_name(&self) -> String;
}

impl SomeTrait for UnitLikeStruct {
    fn my_super_name(&self) -> String {
        String::from("UnitLikeStruct!")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classic_c_structs() {
        let green = ColorClassicStruct {
            red: 0,
            green: 255,
            blue: 0,
        };

        assert_eq!(green.red, 0);
        assert_eq!(green.green, 255);
        assert_eq!(green.blue, 0);
    }

    #[test]
    fn tuple_structs() {
        let green = ColorTupleStruct(0, 255, 0);

        assert_eq!(green.0, 0);
        assert_eq!(green.1, 255);
        assert_eq!(green.2, 0);
    }

    #[test]
    fn unit_structs() {
        let unit_like_struct = UnitLikeStruct;
        let message = format!("{:?}s are fun!", unit_like_struct);
        assert_eq!(message, "UnitLikeStructs are fun!");

        // https://doc.rust-lang.org/book/ch05-01-defining-structs.html#unit-like-structs-without-any-fields
        // "Unit-like structs can be useful when you need to implement a trait on some type but don’t have any data that you want to store in the type itself."
        assert_eq!(unit_like_struct.my_super_name(), "UnitLikeStruct!");
    }
}
