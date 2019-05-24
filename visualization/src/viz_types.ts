// Chunks days up into the typical slices. Notice that they are not evenly
// distributed across 24 hours, but are clustered around typical "awake"
// time
export enum TimeOfDay {
    LateNight = "LateNight"
    ,Morning = "Morning"
    ,MidDay = "MidDay"
    ,Afternoon = "Afternoon"
    ,Evening = "Evening"
}


export function parseTimeOfDay(term: string): TimeOfDay | ValidationFailure {
    switch(term.toLowerCase()){
        case "latenight":
            { return TimeOfDay.LateNight; }
        case "morning":
            { return TimeOfDay.Morning; }
        case "midday":
            { return TimeOfDay.MidDay; }
        case "afternoon":
            { return TimeOfDay.Afternoon; }
        case "evening":
            { return TimeOfDay.Evening; }
        default:
            { return new ValidationFailure(
                term + " is not a valid TimeOfDay.");
            }
    }
}

export class ValidationFailure {
    public readonly message: string;
    constructor(msg: string){
        this.message = msg;
    }
}

export class Natural {
    public readonly n : number
    constructor(value : number) {
        if( value >= 0){
            this.n = value;
        }
        else {
            throw new RangeError(
                value + " is less than 0, and therefore not a natural number.");
        }
    }
}

export function unmatched() {
    throw new Error("Non-exhaustive pattern");
}

// Creates a new natural, or returns a failure indicating that it was out of range
export function toNatural (n: number) : Natural | ValidationFailure {
    try {
        const nat = new Natural(n);
        return nat;
    } catch (RangeError) {
        return new ValidationFailure(RangeError.message);
    }
}

export class TimeOfDayItem {
    readonly time : TimeOfDay;
    readonly duration : Natural;

    constructor(time: TimeOfDay, duration: Natural) {
        this.time = time;
        this.duration = duration;
    }
}

export function parseTODItem(pair: [string, number]) : TimeOfDayItem | ValidationFailure {
    const tod = parseTimeOfDay(pair[0]);
    const dur = toNatural(pair[1]);

    if(tod instanceof ValidationFailure){
        return tod;
    } else if (dur instanceof ValidationFailure){
        return dur;
    } else {
        return new TimeOfDayItem(tod, dur);
    }

}

export class TimeOfDayReport {
    readonly items : [TimeOfDayItem];

    constructor(items: [TimeOfDayItem]){
        this.items = items;
    }
}
