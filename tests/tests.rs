use lang_packer::*;

#[test]
fn expand_macro() {
    #[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Hash, Clone, Copy)]
    enum Rule {
        RChild1,
        RChild2,
        RChild3,
        RParent,
    }

    #[derive(Packer)]
    #[packer(rule = Rule::RChild1)]
    enum Child1 {
        Child2(Child2),
        Child3(Child3),
    }

    #[derive(Packer)]
    #[packer(rule = Rule::RChild2)]
    struct Child2;

    #[derive(Packer)]
    #[packer(rule = Rule::RChild3)]
    struct Child3;

    #[derive(Packer)]
    #[packer(rule = Rule::RParent)]
    struct Parent(Child2, Child3);
}
