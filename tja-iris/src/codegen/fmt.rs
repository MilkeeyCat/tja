use std::fmt::{Arguments, Result, Write};

pub(super) struct IndentWriter<W: Write> {
    writer: W,
    indent: usize,
}

impl<W: Write> IndentWriter<W> {
    pub(crate) fn new(writer: W) -> Self {
        Self { writer, indent: 0 }
    }

    pub(crate) fn indent(&mut self) {
        self.indent += 4;
    }

    pub(crate) fn dedent(&mut self) {
        self.indent -= 4;
    }

    pub(crate) fn write_indented(&mut self, args: Arguments<'_>) -> Result {
        write!(self.writer, "{}", " ".repeat(self.indent))?;
        self.writer.write_fmt(args)
    }

    pub(crate) fn write_indented_nl(&mut self, args: Arguments<'_>) -> Result {
        write!(self.writer, "{}", " ".repeat(self.indent))?;
        self.writer.write_fmt(args)?;
        writeln!(self.writer)
    }
}

impl<W: Write> Write for IndentWriter<W> {
    fn write_str(&mut self, s: &str) -> Result {
        self.writer.write_str(s)
    }
}

#[macro_export]
macro_rules! write_indented {
    ($writer:expr, $($arg:tt)*) => {
        $writer.write_indented(format_args!($($arg)*))
    };
    ($($arg:tt)*) => {
        compile_error!("requires a writer and format arguments, like `write_indented!(writer, \"format string\", args...)`")
    };
}

#[macro_export]
macro_rules! writeln_indented {
    ($writer:expr $(,)?) => {
        write_indented!($writer, "\n")
    };
    ($writer:expr, $($arg:tt)*) => {
        $writer.write_indented_nl(format_args!($($arg)*))
    };
    ($($arg:tt)*) => {
        compile_error!("requires a writer and format arguments, like `writeln_indented!(writer, \"format string\", args...)`")
    };
}

#[macro_export]
macro_rules! with_indent {
    ($writer:expr, $body:block) => {
        $writer.indent();
        $body;
        $writer.dedent();
    };
}

pub(super) trait Generator {
    type Writer: Write;

    fn writer(&mut self) -> &mut Self::Writer;
}

pub(super) fn gen_comma_separated<G: Generator, I: Copy, F: Fn(&mut G, I) -> Result>(
    generator: &mut G,
    items: &[I],
    generate: F,
) -> Result {
    if let Some((&first, rest)) = items.split_first() {
        generate(generator, first)?;

        for &item in rest {
            write!(generator.writer(), ", ")?;

            generate(generator, item)?;
        }
    }

    Ok(())
}
