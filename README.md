# timetracker
TimeTracker (`track`) is a small command-line application for tracking how your time is spent. There are dozens of web and/or desktop time tracking applications, but they're almost exsclusively focused on tracking time against projects for billing. I've personally used several of these for freelance work & they're generally great. Unfortunatley, what I'm looking for is simply user-driven time tracking from the command line.

### Inspiration
The motivation behind `track` comes from Peter Drucker's "The Effective Executive", which opens with a description of how its impossible to improve your time management if you're unaware of where your time is spent. He proceeds to make a compelling case that our memories are faulty (write-on-read memory) and that the only way to understand how our time is spent is to measure it. `track` is intended to provide an unintrusive CLI for recording the start/end of various tasks throughout the day. The remainder of this document will explain how to interact with `track`.

### The CLI
Getting started:

OS X
```
> brew install track
> track category new Meeting
> track category new Coding
> track start
Choose the activity category:
1) Meeting
2) Coding
1
> track start Coding
> track stop
> track logs analyze --start "2018-01-15 14:30" --end "2018-06-01 15:45" -f myTime.log
> track logs delete
```

The core idea of `track` is that you can only do one thing at a time, so there will only ever be a single activity in progress. You can start a new activity, stop performing any activity, or swith between activities. For the full list of commands, you can run `track --help`.

#### Analyzing your time
Tracking time is all well and good, but the whole point is to see where its going, right? `track` generates either a JSON dump of your activity by category, time of day. You can then import this file into actual visualization tools like D3 or gnuplot if you want.
