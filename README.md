# timetracker
TimeTracker (`track`) is a small command-line application for tracking how your time is spent. There are dozens of web and/or desktop time tracking applications, but they're almost exsclusively focused on tracking time against projects for billing. I've personally used several of these for freelance work & they're generally great. Unfortunatley, what I'm looking for is simply user-driven time tracking from the command line.

### Inspiration
The motivation behind `track` comes from Peter Drucker's "The Effective Executive", which opens with a description of how its impossible to improve your time management if you're unaware of where your time is spent. He proceeds to make a compelling case that our memories are faulty (write-on-read memory) and that the only way to understand how our time is spent is to measure it. `track` is intended to provide an unintrusive CLI for recording the start/end of various tasks throughout the day. The remainder of this document will explain how to interact with `track`.

### The CLI
Getting started:

OS X
```
> brew install track
> track category add Meeting
> track category add Coding
> track start
Choose the activity category:
1) Meeting
2) Coding
1
> track start -c Coding
> track stop
> track analyze -f myTime.log
```
