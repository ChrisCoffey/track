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

export parseTimeOfDay(term: string): TimeOfDay | ValidationFailure {
    switch(term.toLower()){
        case "latenight":
            return LateNight;
        case "morning":
            return Morning;
        case "midday":
            return MidDay;
        case "afternoon":
            return Afternoon;
        case "evening":
            return Evening;
        default:
            return new ValidationFailure(term + " is not a valid TimeOfDay.");
    }
}

export class ValidationFailure {
    readonly message: string;
    constructor(msg: string){
        this.message = msg;
    }
}

export class Natural {
    readonly n : number
    constructor(value : number) {
        if( value >= 0){
            this.n = value;
        }
        else {
            throw new RangeError(value + " is less than 0, and therefore not a natural number.")
        }
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

export class TimeOfDayReport {
    readonly items : [TimeOfDayItem];

    constructor(items: [TimeOfDayItem]){
        this.items = items;
    }
}
