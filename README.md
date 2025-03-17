# Lang packer

When writing programming languages, it's necessary to translate the parsed grammar into a simplified model to simplify evaluation.
`lang_packer` is a utility to bridge the gap between `pest::iterators::Pairs` and your language's model.

## Usage

_Example borrowed from `timesheets` project._

Given the following pest snippet
```pest
REST_OF_LINE = _{ (&!NEWLINE ~ ANY)+ }

SUMMARY = { REST_OF_LINE }

summary = _{ "|" ~ SUMMARY }

NOW = { "NOW" }

time_range_end = { TIME | NOW }

time_range = { TIME ~ "-" ~ time_range_end }

work = { "WORK" ~ time_range ~ summary }

working_day = { "WORKING DAY" ~ time_range ~ summary? }

lunch = { "LUNCH" ~ time_range ~ summary? }

log_event =
    { work
    | working_day
    | lunch
    }
```

And the following language model
```rs
pub struct Summary(String);

pub struct Time(pub NaiveTime);

pub struct Now;

pub enum TimeRangeEnd {
    Now(Now),
    Time(Time),
}

pub struct TimeRange(pub Time, pub TimeRangeEnd);

pub struct WorkLog(pub TimeRange, pub Summary);

pub struct WorkingDayLog(pub TimeRange, pub Option<Summary>);

pub struct LunchLog(pub TimeRange, pub Option<Summary>);

pub enum LogEvent {
    Work(WorkLog),
    WorkingDay(WorkingDayLog),
    Lunch(LunchLog),
    Break(BreakLog),
    Leave(LeaveLog),
}
```


The code to map from one to the other could be generated using the Packer macro.
```rs
#[derive(Debug, Packer)]
#[packer(rule = Rule::SUMMARY)]
pub struct Summary(String);

#[derive(Debug, Clone, Packer)]
#[packer(rule = Rule::TIME)]
pub struct Time(pub NaiveTime);

#[derive(Debug, Clone, Packer)]
#[packer(rule = Rule::NOW)]
pub struct Now;

#[derive(Debug, Clone, Packer)]
#[packer(rule = Rule::time_range_end)]
pub enum TimeRangeEnd {
    Now(Now),
    Time(Time),
}

#[derive(Debug, Clone, Packer)]
#[packer(rule = Rule::time_range)]
pub struct TimeRange(pub Time, pub TimeRangeEnd);

#[derive(Debug, Packer)]
#[packer(rule = Rule::work)]
pub struct WorkLog(pub TimeRange, pub Summary);

#[derive(Debug, Packer)]
#[packer(rule = Rule::working_day)]
pub struct WorkingDayLog(pub TimeRange, pub Option<Summary>);

#[derive(Debug, Packer)]
#[packer(rule = Rule::lunch)]
pub struct LunchLog(pub TimeRange, pub Option<Summary>);

#[derive(Debug, Packer)]
#[packer(rule = Rule::log_event)]
pub enum LogEvent {
    Work(WorkLog),
    WorkingDay(WorkingDayLog),
    Lunch(LunchLog),
    Break(BreakLog),
    Leave(LeaveLog),
}
```


And then invoked as follows:
```rs
use pest_derive::Parser;
use pest::{iterators::Pairs, Parser};
use lang_packer_model::{generic_utils::{SyntaxTree, DropRules}, pack_trees::{unpack_only_tree, TokenPacker}};

#[derive(Parser)]
#[grammar = "src/parser/parser.pest"]
pub struct TimesheetsParser;

impl DropRules for Rule {
    fn get_drop_rules(&self) -> Vec<Self> {
        vec![ Rule::TAB ]
    }
}

fn parse_and_pack(code: &str) -> Result<LogEvent, PackingError<Rule>> {
    let pairs = TimesheetsParser::parse(Rule::log_event, code).unwrap();

    let trees: Vec<_> = pairs.map(SyntaxTree::from)
        .collect();

    unpack_only_tree(trees)
        .and_then(LogEvent::pack)
}
```

## Install
```sh
cargo build
```

