# ccl-armv8-early-experiments
This project is for *students* and *professionals* with a **Common Lisp** background (or curiosity) who are interested in Lisp *compilation techniques, language translators, and simulators.*

The project includes the *executable* results of a number of experiments undertaken to learn how the **Clozure Common Lisp** (CCL) compiler and run-time kernel work.
It includes three architectural simulators: one for a small subset of the Intel x86-64, a second for the 32-bit ARM, and a third for the 64-bit ARMv8. (\*)
For the x86 and the 32-bit ARM, there are *runnable* examples of the CCL Lisp compiler output from small, carefully curated Lisp programs: one recursive integer algorithm, and one example of in-line, thread-safe storage allocation (i.e. CONS).
For the 64-bit ARMv8 there is experimental ("straw man"), hand-compiled, *verified* ARMv8 code for the same Lisp examples. The simulators let you trace execution, and the Lisp REPL (Read-Eval-Print Loop) lets you interactively examine simulated memory and register contents.

Finally, there is an automatically-generated **.s** file that successfully runs on a Raspberry Pi 400 with a 64-bit Linux distro (when linked with a choice of C "main" source files).
This assembly language code is not a conclusion, but rather a conversation-*starter* on how to port CCL to the ARMv8, and eventually to MacOS on the Apple Silicon "M1" system-on-a-chip family and its successors.

Advanced Lisp programmers might be interested in seeing how the authors wrote a "LAP" (Lisp Assembly Language) to Common Lisp "compiler" that uses LAMBDA and GO to implement branch addresses (Teaser: each simulation is a single, giant TAGBODY, but how do you push a "return address" onto the stack?).

For more details, please refer to included documentation: **2022-CCL-project-write-up** in PDF and OpenDocument format.  **Invitation**: If you have the means to convert the **.odt** file to Markdown format and the willingness to check it in, be our guest.  Thanks!

**Footnotes**: (\*) Do not even *dream* that these simulators are accurate or comprehensive (look elsewhere if you have a requirement for that). They only implement the instructions emitted by the CCL Lisp compiler.  The simulated instructions are not bit- or flag-accurate, and do not reflect limitations of the instruction formats (e.g. the size of literal constants). Thankfully, these simulators *are* small and distraction-free enough for study and teaching purposes.
