import * as fs from 'fs';
const contents = fs.readFileSync('./input').toString();
const elvesCalories = contents.split('\n\n');
const cals =
    elvesCalories.map(elf =>
        elf.split("\n")
            .map(v => parseInt(v))
            .reduce((sum, v) => sum + v)
    );
const maxcals = cals.reduce((max, v) => v > max ? v : max);
console.log(maxcals);


