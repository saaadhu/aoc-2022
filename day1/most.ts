import * as fs from 'fs';
const contents = fs.readFileSync('./input').toString();
const elvesCalories = contents.split('\n\n');
const cals =
    elvesCalories.map(elf =>
        elf.split("\n")
            .map(v => parseInt(v))
            .reduce((sum, v) => sum + v)
    );
cals.sort((a, b) => b - a);
console.log(cals[0] + cals[1] + cals[2]);


