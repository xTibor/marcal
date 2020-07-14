use std::fmt;

enum Expression {
    Value,
    AddValue { lhs: Box<Expression> },
    SubValue { lhs: Box<Expression> },
    ShiftBy { lhs: Box<Expression>, shift: usize },
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value => write!(f, "X"),
            Self::AddValue { lhs } => write!(f, "({} + X)", lhs),
            Self::SubValue { lhs } => write!(f, "({} - X)", lhs),
            Self::ShiftBy { lhs, shift } => write!(f, "({} << {})", lhs, shift),
        }
    }
}

impl Expression {
    fn eval(&self) -> usize {
        match self {
            Self::Value => 1,
            Self::AddValue { lhs } => lhs.eval() + 1,
            Self::SubValue { lhs } => lhs.eval() - 1,
            Self::ShiftBy { lhs, shift } => lhs.eval() * (3usize.pow(*shift as u32)),
        }
    }

    fn complexity(&self) -> usize {
        match self {
            Self::Value => 1,
            Self::AddValue { lhs } => lhs.complexity() + 1,
            Self::SubValue { lhs } => lhs.complexity() + 1,
            Self::ShiftBy { lhs, .. } => lhs.complexity() + 1,
        }
    }

    fn has_subtraction(&self) -> bool {
        match self {
            Self::Value => false,
            Self::AddValue { lhs } => lhs.has_subtraction(),
            Self::SubValue { .. } => true,
            Self::ShiftBy { lhs, .. } => lhs.has_subtraction(),
        }
    }

    fn new(value: usize) -> Self {
        match value {
            0 => unreachable!(),
            1 => Self::Value,
            2 => Self::AddValue {
                lhs: Box::new(Self::Value),
            },
            3 => Self::ShiftBy {
                lhs: Box::new(Self::Value),
                shift: 1,
            },
            _ => {
                let method_1 = Self::new_method_1(value);
                let method_2 = Self::new_method_2(value);

                if method_1.complexity() == method_2.complexity() {
                    if !method_1.has_subtraction() {
                        method_1
                    } else {
                        method_2
                    }
                } else if method_1.complexity() < method_2.complexity() {
                    method_1
                } else {
                    method_2
                }
            }
        }
    }

    fn new_method_1(value: usize) -> Self {
        let mut base = Self::new(value / 3);

        if let Self::ShiftBy { ref mut shift, .. } = base {
            *shift += 1;
        } else {
            base = Self::ShiftBy {
                lhs: Box::new(base),
                shift: 1,
            }
        }

        match value % 3 {
            0 => base,
            1 => Self::AddValue {
                lhs: Box::new(base),
            },
            2 => Self::AddValue {
                lhs: Box::new(Self::AddValue {
                    lhs: Box::new(base),
                }),
            },
            _ => unreachable!(),
        }
    }

    fn new_method_2(value: usize) -> Self {
        let mut base = Self::new((value + 1) / 3);

        if let Self::ShiftBy { ref mut shift, .. } = base {
            *shift += 1;
        } else {
            base = Self::ShiftBy {
                lhs: Box::new(base),
                shift: 1,
            }
        }

        match ((value + 1) % 3) as isize - 1 {
            -1 => Self::SubValue {
                lhs: Box::new(base),
            },
            0 => base,
            1 => Self::AddValue {
                lhs: Box::new(base),
            },
            _ => unreachable!(),
        }
    }

    fn assembly(&self) -> String {
        let mut s = String::new();
        self.assembly_impl(&mut s, self.has_subtraction())
            .expect("failed to generate assembly");
        s
    }

    fn assembly_impl<W: fmt::Write>(&self, f: &mut W, has_subtraction: bool) -> fmt::Result {
        // U1 - Operand
        // U2 - Operand (inverse)
        // U3 - Result
        match self {
            Self::Value => {
                writeln!(f, "LDLH U1 0")?;
                if has_subtraction {
                    writeln!(f, "NEGR U2 U1")?;
                }
                writeln!(f, "ADDR U3 U1 ZERO")?;
            }
            Self::AddValue { lhs } => {
                lhs.assembly_impl(f, has_subtraction)?;
                writeln!(f, "ADDR U3 U3 U1")?;
            }
            Self::SubValue { lhs } => {
                lhs.assembly_impl(f, has_subtraction)?;
                writeln!(f, "ADDR U3 U3 U2")?;
            }
            Self::ShiftBy { lhs, shift } => {
                lhs.assembly_impl(f, has_subtraction)?;
                writeln!(f, "SHLQ U3 U3 {}", shift)?;
            }
        }
        Ok(())
    }
}

fn main() {
    for i in 1..=200 {
        let expr = Expression::new(i);
        println!("{}", expr);
        println!("eval: {}, complexity: {}", expr.eval(), expr.complexity());
        println!("{}", expr.assembly());
    }

    let expr = Expression::new(1234);
    println!("{}", expr);
    println!("eval: {}, complexity: {}", expr.eval(), expr.complexity());
    println!("{}", expr.assembly());
}
