Better error management
    Find a way to put source code into the errors without charging for string clones

Simplify the process further
    I still feel like there's too much code for what I'm trying to do. Too many special cases
    I've done a lot for this now. We're approaching the point where it's acceptably clean

Increase flexibility
    At the moment you need a wrapper packer to handle special cases such as optionals, vecs and primitives.
    It should be possible to make these special cases first class, as regular packers are, but you're going to need to rework a lot of stuff to make that happen. Eyes on the prize


Token repacker should become something I can use casually.
